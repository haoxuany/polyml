(*
    Copyright (c) 2016 David C.J. Matthews

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License version 2.1 as published by the Free Software Foundation.
    
    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.
    
    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*)

functor X86FOREIGNCALL(

    structure X86CODE: X86CODESIG

    structure X86OPTIMISE:
    sig
        type operation
        type code
        type operations = operation list

        val optimise: code * operations -> operations

        structure Sharing:
        sig
            type operation = operation
            type code = code
        end
    end

    structure DEBUG: DEBUGSIG

    sharing X86CODE.Sharing = X86OPTIMISE.Sharing
): FOREIGNCALLSIG
=
struct
    open X86CODE
    open Address
    
    exception InternalError = Misc.InternalError
    
    val pushR = PushToStack o RegisterArg

    fun moveRR{source, output} = MoveToRegister{source=RegisterArg source, output=output}

    fun condBranch(test, predict) =
    let
        val label as Labels{uses, ...} = mkLabel()
    in
        uses := 1;
        (ConditionalBranch{test=test, predict=predict, label=label}, label)
    end

    fun loadMemory(reg, base, offset) =
        MoveToRegister{source=MemoryArg{base=base, offset=offset, index=NoIndex}, output=reg}
    and storeMemory(reg, base, offset) =
        StoreRegToMemory{toStore=reg, address={base=base, offset=offset, index=NoIndex}}

    fun createProfileObject _ (*functionName*) =
    let
        (* The profile object is a single mutable with the F_bytes bit set. *)
        open Address
        val profileObject = RunCall.allocateByteMemory(0w1, Word.fromLargeWord(Word8.toLargeWord(Word8.orb(F_mutable, F_bytes))))
        fun clear 0w0 = ()
        |   clear i = (assignByte(profileObject, i-0w1, 0w0); clear (i-0w1))
        val () = clear(Word.fromInt wordSize)
    in
        toMachineWord profileObject
    end

    val makeEntryPoint: string -> machineWord = Compat560.createEntryPointObject

    datatype abi = X86_32 | X64Win | X64Unix

    val noException = 1

    (* Full RTS call version.  An extra argument is passed that contains the thread ID.
       This allows the taskData object to be found which is needed if the code allocates
       any ML memory or raises an exception.  It also saves the stack and heap pointers
       in case of a GC. *)
    fun rtsCallFull (functionName, nArgs (* Not counting the thread ID *), debugSwitches) =
    let
        val entryPointAddr = makeEntryPoint functionName

        (* Get the ABI.  On 64-bit Windows and Unix use different calling conventions. *)
        val abi =
            case Compat560.polySpecificGeneral (108, 0) of
                1 => X64Unix
            |   2 => X64Win
            |   _ => X86_32

        (* Branch to check for exception. *)
        val (checkExc, exLabel) = condBranch(JNE, PredictNotTaken)
        
        (* Unix X64.  The first six arguments are in rdi, rsi, rdx, rcx, r8, r9.
                      The rest are on the stack.
           Windows X64. The first four arguments are in rcx, rdx, r8 and r9.  The rest are
                       on the stack.  The caller must ensure the stack is aligned on 16-byte boundary
                       and must allocate 32-byte save area for the register args.
                       rbx, rbp, rdi, rsi, rsp, r12-r15 are saved by the called function.
           X86/32.  Arguments are pushed to the stack.
                   ebx, edi, esi, ebp and esp are saved by the called function.
                   We use esi to hold the argument data pointer and edi to save the ML stack pointer
           Our ML conventions use eax, ebx for the first two arguments in X86/32 and
                   rax, ebx, r8, r9, r10 for the first five arguments in X86/64.
        *)
        
        (* This is a pointer to data that the RTS provides on entry.  Fields
           may be updated in the RTS call.  Where it is stored differs between the ABIs. *)
        val memRegArgumentPtr = case abi of X64Unix => ~48 | X64Win => 16 | X86_32 => 8

        (* The argument pointer and the ML stack pointer need to be
           loaded into registers that will be saved by the callee.
           The entry point doesn't need to be but must be a register
           that isn't used for an argument. *)
        (* We have to save the stack pointer to the argument structure but
           we still need a register if we have args on the stack. *)
        val (entryPtrReg, argPtrReg, saveMLStackPtrReg) =
            if isX64 then (r11, r12, r13) else (ecx, esi, edi)
        
        val stackSpace =
            case abi of
                X64Unix => memRegSize
            |   X64Win => memRegSize + 32 (* Requires 32-byte save area. *)
            |   X86_32 =>
                let
                    (* GCC likes to keep the stack on a 16-byte alignment. *)
                    val argSpace = (nArgs+1)*4
                    val align = argSpace mod 16
                in
                    (* Add sufficient space so that esp will be 16-byte aligned *)
                    if align = 0
                    then memRegSize
                    else memRegSize + 16 - align
                end

        val code =
            [
                MoveToRegister{source=LongConstArg entryPointAddr, output=entryPtrReg}, (* Load the entry point ref. *)
                loadMemory(entryPtrReg, entryPtrReg, 0),(* Load its value. *)
                loadMemory(argPtrReg, ebp, memRegArgumentPtr) (* Get argument data ptr  - use r12 to save reloading after the call *)
            ] @
            (
                (* Save heap ptr.  This is in r15 in X86/64 *)
                if isX64 then [storeMemory(r15, argPtrReg, argLocalMpointer)] (* Save heap ptr *)
                else [loadMemory(edx, ebp, memRegLocalMPointer), storeMemory(edx, argPtrReg, argLocalMpointer) ]
            ) @
            [
                moveRR{source=esp, output=saveMLStackPtrReg}, (* Save this in case it is needed for arguments. *)
                (* Have to save the stack pointer to the arg structure in case we need to scan the stack for a GC. *)
                storeMemory(esp, argPtrReg, argStackPointer), (* Save ML stack and switch to C stack. *)
                moveRR{source=ebp, output=esp},
                (* Set the stack pointer past the data on the stack.  For Windows/64 add in a 32 byte save area *)
                ArithToGenReg{opc=SUB, output=esp, source=ShortConstArg(LargeInt.fromInt stackSpace)}
            ] @
            (
                case (abi, nArgs) of  (* Set the argument registers. *)
                    (X64Unix, 0) => [ loadMemory(edi, argPtrReg, argThreadSelf) ]
                |   (X64Unix, 1) => [ loadMemory(edi, argPtrReg, argThreadSelf), moveRR{source=eax, output=esi} ]
                |   (X64Unix, 2) =>
                        [ loadMemory(edi, argPtrReg, argThreadSelf), moveRR{source=eax, output=esi}, moveRR{source=ebx, output=edx} ]
                |   (X64Unix, 3) => 
                        [ loadMemory(edi, argPtrReg, argThreadSelf), moveRR{source=eax, output=esi}, moveRR{source=ebx, output=edx}, moveRR{source=r8, output=ecx} ]
                |   (X64Win, 0) => [ loadMemory(ecx, argPtrReg, argThreadSelf) ]
                |   (X64Win, 1) => [ loadMemory(ecx, argPtrReg, argThreadSelf), moveRR{source=eax, output=edx} ]
                |   (X64Win, 2) =>
                        [ loadMemory(ecx, argPtrReg, argThreadSelf), moveRR{source=eax, output=edx}, moveRR{source=ebx, output=r8} ]
                |   (X64Win, 3) =>
                        [ loadMemory(ecx, argPtrReg, argThreadSelf), moveRR{source=eax, output=edx}, moveRR{source=r8, output=r9}, moveRR{source=ebx, output=r8} ]
                |   (X86_32, 0) => [ PushToStack(MemoryArg{base=argPtrReg, offset=argThreadSelf, index=NoIndex}) ]
                |   (X86_32, 1) => [ pushR eax, PushToStack(MemoryArg{base=argPtrReg, offset=argThreadSelf, index=NoIndex}) ]
                |   (X86_32, 2) => [ pushR ebx, pushR eax, PushToStack(MemoryArg{base=argPtrReg, offset=argThreadSelf, index=NoIndex}) ]
                |   (X86_32, 3) =>
                        [
                            (* We need to move an argument from the ML stack. *)
                            PushToStack(MemoryArg{base=saveMLStackPtrReg, offset=4, index=NoIndex}), pushR ebx, pushR eax,
                            PushToStack(MemoryArg{base=argPtrReg, offset=argThreadSelf, index=NoIndex})
                        ]
                |   _ => raise InternalError "rtsCall: Abi/argument count not implemented"
            ) @
            [
                CallFunction(DirectReg entryPtrReg), (* Call the function *)
                moveRR{source=saveMLStackPtrReg, output=esp} (* Restore the ML stack pointer *)
            ] @
            (
            if isX64 then [loadMemory(r15, argPtrReg, argLocalMpointer) ] (* Copy back the heap ptr *)
            else [loadMemory(edx, esi, argLocalMpointer), storeMemory(edx, ebp, memRegLocalMPointer) ]
            ) @
            [
                loadMemory(edx, argPtrReg, argLocalMbottom), storeMemory(edx, ebp, memRegLocalMbottom), (* and base ptr *)
                ArithMemConst{opc=CMP, offset=argExceptionPacket, base=argPtrReg, source=noException},
                checkExc,
                (* Remove any arguments that have been passed on the stack. *)
                ReturnFromFunction(Int.max(case abi of X86_32 => nArgs-2 | _ => nArgs-5, 0)),
                JumpLabel exLabel, (* else raise the exception *)
                loadMemory(eax, argPtrReg, argExceptionPacket),
                RaiseException
            ]
 
        val profileObject = createProfileObject functionName
        val newCode = codeCreate (functionName, profileObject, debugSwitches)
        val createdCode = createCodeSegment(X86OPTIMISE.optimise(newCode, code), newCode)
        (* Have to create a closure for this *)
        open Address
        val closure = allocWordData(0w1, Word8.orb (F_mutable, F_words), toMachineWord 0w0)
    in
        assignWord(closure, 0w0, toMachineWord createdCode);
        lock closure;
        closure
    end

    (* This is a quicker version but can only be used if the RTS entry does
       not allocated ML memory, raise an exception or need to suspend the thread. *)
    fun rtsCallFast (functionName, nArgs, debugSwitches) =
    let
        val entryPointAddr = makeEntryPoint functionName

        (* Get the ABI.  On 64-bit Windows and Unix use different calling conventions. *)
        val abi =
            case Compat560.polySpecificGeneral (108, 0) of
                1 => X64Unix
            |   2 => X64Win
            |   _ => X86_32

        (* We don't need an argument pointer but we do need to save
           the ML stack pointer across the call. *)
        val (entryPtrReg, saveMLStackPtrReg) =
            if isX64 then (r11, r13) else (ecx, edi)
        
        val stackSpace =
            case abi of
                X64Unix => memRegSize
            |   X64Win => memRegSize + 32 (* Requires 32-byte save area. *)
            |   X86_32 =>
                let
                    (* GCC likes to keep the stack on a 16-byte alignment. *)
                    val argSpace = nArgs*4
                    val align = argSpace mod 16
                in
                    (* Add sufficient space so that esp will be 16-byte aligned *)
                    if align = 0
                    then memRegSize
                    else memRegSize + 16 - align
                end

        val code =
            [
                MoveToRegister{source=LongConstArg entryPointAddr, output=entryPtrReg}, (* Load the entry point ref. *)
                loadMemory(entryPtrReg, entryPtrReg, 0),(* Load its value. *)
                moveRR{source=esp, output=saveMLStackPtrReg}, (* Save ML stack and switch to C stack. *)
                moveRR{source=ebp, output=esp},
                (* Set the stack pointer past the data on the stack.  For Windows/64 add in a 32 byte save area *)
                ArithToGenReg{opc=SUB, output=esp, source=ShortConstArg(LargeInt.fromInt stackSpace)}
            ] @
            (
                case (abi, nArgs) of  (* Set the argument registers. *)
                    (_, 0) => []
                |   (X64Unix, 1) => [ moveRR{source=eax, output=edi} ]
                |   (X64Unix, 2) =>
                        [ moveRR{source=eax, output=edi}, moveRR{source=ebx, output=esi} ]
                |   (X64Unix, 3) => 
                        [ moveRR{source=eax, output=edi}, moveRR{source=ebx, output=esi}, moveRR{source=r8, output=edx} ]
                |   (X64Unix, 4) => 
                        [ moveRR{source=eax, output=edi}, moveRR{source=ebx, output=esi}, moveRR{source=r8, output=edx}, moveRR{source=r9, output=ecx} ]
                |   (X64Win, 1) => [ moveRR{source=eax, output=ecx} ]
                |   (X64Win, 2) => [ moveRR{source=eax, output=ecx}, moveRR{source=ebx, output=edx} ]
                |   (X64Win, 3) =>
                        [ moveRR{source=eax, output=ecx}, moveRR{source=ebx, output=edx} (* Arg3 is already in r8. *) ]
                |   (X64Win, 4) =>
                        [ moveRR{source=eax, output=ecx}, moveRR{source=ebx, output=edx} (* Arg3 is already in r8 and arg4 in r9. *) ]
                |   (X86_32, 1) => [ pushR eax ]
                |   (X86_32, 2) => [ pushR ebx, pushR eax ]
                |   (X86_32, 3) =>
                        [
                            (* We need to move an argument from the ML stack. *)
                            loadMemory(edx, saveMLStackPtrReg, 4), pushR edx, pushR ebx, pushR eax
                        ]
                |   (X86_32, 4) =>
                        [
                            (* We need to move an arguments from the ML stack. *)
                            loadMemory(edx, saveMLStackPtrReg, 4), pushR edx,
                            loadMemory(edx, saveMLStackPtrReg, 8), pushR edx,
                            pushR ebx, pushR eax
                        ]
                |   _ => raise InternalError "rtsCall: Abi/argument count not implemented"
            ) @
            [
                CallFunction(DirectReg entryPtrReg), (* Call the function *)
                moveRR{source=saveMLStackPtrReg, output=esp}, (* Restore the ML stack pointer *)
                (* Remove any arguments that have been passed on the stack. *)
                ReturnFromFunction(Int.max(case abi of X86_32 => nArgs-2 | _ => nArgs-5, 0))
            ]
 
        val profileObject = createProfileObject functionName
        val newCode = codeCreate (functionName, profileObject, debugSwitches)
        val createdCode = createCodeSegment(X86OPTIMISE.optimise(newCode, code), newCode)
        (* Have to create a closure for this *)
        open Address
        val closure = allocWordData(0w1, Word8.orb (F_mutable, F_words), toMachineWord 0w0)
    in
        assignWord(closure, 0w0, toMachineWord createdCode);
        lock closure;
        closure
    end
    
    (* RTS call with one double-precision floating point argument and a floating point result.
       First version.  This will probably be merged into the above code in due
       course.
       Currently ML always uses boxed values for floats.  *)
    fun rtsCallFastFloattoFloat (functionName, debugSwitches) =
    let
        val entryPointAddr = makeEntryPoint functionName

        (* Get the ABI.  On 64-bit Windows and Unix use different calling conventions. *)
        val abi =
            case Compat560.polySpecificGeneral (108, 0) of
                1 => X64Unix
            |   2 => X64Win
            |   _ => X86_32

        (* We don't need an argument pointer but we do need to save
           the ML stack pointer across the call. *)
        val (entryPtrReg, saveMLStackPtrReg) =
            if isX64 then (r11, r13) else (ecx, edi)
        
        val stackSpace =
            case abi of
                X64Unix => memRegSize
            |   X64Win => memRegSize + 32 (* Requires 32-byte save area. *)
            |   X86_32 =>
                let
                    (* GCC likes to keep the stack on a 16-byte alignment. *)
                    val argSpace = 8 (*nArgs*4*) (* One "double" value. *)
                    val align = argSpace mod 16
                in
                    (* Add sufficient space so that esp will be 16-byte aligned *)
                    if align = 0
                    then memRegSize
                    else memRegSize + 16 - align
                end

        (* Constants for a box for a float *)
        val fpBoxSize = 8 div wordSize
        val fpBoxLengthWord32 = IntInf.orb(IntInf.fromInt fpBoxSize, IntInf.<<(Word8.toLargeInt F_bytes, 0w24))

        val code =
            [
                MoveToRegister{source=LongConstArg entryPointAddr, output=entryPtrReg}, (* Load the entry point ref. *)
                loadMemory(entryPtrReg, entryPtrReg, 0),(* Load its value. *)
                moveRR{source=esp, output=saveMLStackPtrReg}, (* Save ML stack and switch to C stack. *)
                moveRR{source=ebp, output=esp},
                (* Set the stack pointer past the data on the stack.  For Windows/64 add in a 32 byte save area *)
                ArithToGenReg{opc=SUB, output=esp, source=ShortConstArg(LargeInt.fromInt stackSpace)}
            ] @
            (
                case abi of
                    (* X64 on both Windows and Unix take the first arg in xmm0.  We need to
                       unbox the value pointed at by rax. *)
                    X64Unix => [ XMMArith { opc= SSE2Move, source=MemoryArg{base=eax, offset=0, index=NoIndex}, output=xmm0 } ]
                |   X64Win => [ XMMArith { opc= SSE2Move, source=MemoryArg{base=eax, offset=0, index=NoIndex}, output=xmm0 } ]
                |   X86_32 =>
                     (* eax contains the address of the value.  This must be unboxed onto the stack. *)
                    [
                        FPLoadFromMemory{address={base=eax, offset=0, index=NoIndex}, precision=DoublePrecision},
                        ArithToGenReg{ opc=SUB, output=esp, source=ShortConstArg 8},
                        FPStoreToMemory{ address={base=esp, offset=0, index=NoIndex}, precision=DoublePrecision, andPop=true }
                    ]
            ) @
            [
                CallFunction(DirectReg entryPtrReg), (* Call the function *)
                moveRR{source=saveMLStackPtrReg, output=esp} (* Restore the ML stack pointer *)
            ] @
            (
                (* Put the floating point result into a box. *)
                case abi of
                   X86_32 =>
                    [
                        AllocStore{size=fpBoxSize, output=eax, saveRegs=[]},
                        StoreConstToMemory{toStore=fpBoxLengthWord32,
                                address={offset= ~wordSize, base=eax, index=NoIndex}},
                        FPStoreToMemory{ address={base=eax, offset=0, index=NoIndex}, precision=DoublePrecision, andPop=true },
                        StoreInitialised
                    ]
                |   _ => (* X64 The result is in xmm0 *)
                    [
                        AllocStore{size=fpBoxSize, output=eax, saveRegs=[]},
                        StoreConstToMemory{toStore=LargeInt.fromInt fpBoxSize,
                            address={offset= ~wordSize, base=eax, index=NoIndex}},
                        StoreNonWordConst{size=Size8Bit, toStore=Word8.toLargeInt F_bytes, address={offset= ~1, base=eax, index=NoIndex}},
                        XMMStoreToMemory { address={base=eax, offset=0, index=NoIndex}, precision=DoublePrecision, toStore=xmm0 },
                        StoreInitialised
                    ]                    
            ) @
            [
                (* Remove any arguments that have been passed on the stack. *)
                ReturnFromFunction 0
            ]
 
        val profileObject = createProfileObject functionName
        val newCode = codeCreate (functionName, profileObject, debugSwitches)
        val createdCode = createCodeSegment(X86OPTIMISE.optimise(newCode, code), newCode)
        (* Have to create a closure for this *)
        open Address
        val closure = allocWordData(0w1, Word8.orb (F_mutable, F_words), toMachineWord 0w0)
    in
        assignWord(closure, 0w0, toMachineWord createdCode);
        lock closure;
        closure
    end

    (* RTS call with one general (i.e. ML word) argument and a floating point result.
       This is used only to convert arbitrary precision values to floats.
       In due course this will be merged into the other functions. *)
    fun rtsCallFastGeneraltoFloat (functionName, debugSwitches) =
    let
        val entryPointAddr = makeEntryPoint functionName

        (* Get the ABI.  On 64-bit Windows and Unix use different calling conventions. *)
        val abi =
            case Compat560.polySpecificGeneral (108, 0) of
                1 => X64Unix
            |   2 => X64Win
            |   _ => X86_32

        (* We don't need an argument pointer but we do need to save
           the ML stack pointer across the call. *)
        val (entryPtrReg, saveMLStackPtrReg) =
            if isX64 then (r11, r13) else (ecx, edi)
        
        val stackSpace =
            case abi of
                X64Unix => memRegSize
            |   X64Win => memRegSize + 32 (* Requires 32-byte save area. *)
            |   X86_32 =>
                let
                    (* GCC likes to keep the stack on a 16-byte alignment. *)
                    val argSpace = 8 (*nArgs*4*) (* One "double" value. *)
                    val align = argSpace mod 16
                in
                    (* Add sufficient space so that esp will be 16-byte aligned *)
                    if align = 0
                    then memRegSize
                    else memRegSize + 16 - align
                end

        (* Constants for a box for a float *)
        val fpBoxSize = 8 div wordSize
        val fpBoxLengthWord32 = IntInf.orb(IntInf.fromInt fpBoxSize, IntInf.<<(Word8.toLargeInt F_bytes, 0w24))

        val code =
            [
                MoveToRegister{source=LongConstArg entryPointAddr, output=entryPtrReg}, (* Load the entry point ref. *)
                loadMemory(entryPtrReg, entryPtrReg, 0),(* Load its value. *)
                moveRR{source=esp, output=saveMLStackPtrReg}, (* Save ML stack and switch to C stack. *)
                moveRR{source=ebp, output=esp},
                (* Set the stack pointer past the data on the stack.  For Windows/64 add in a 32 byte save area *)
                ArithToGenReg{opc=SUB, output=esp, source=ShortConstArg(LargeInt.fromInt stackSpace)}
            ] @
            (
                case abi of
                    X64Unix => [ moveRR{source=eax, output=edi} ]
                |   X64Win => [ moveRR{source=eax, output=ecx} ]
                |   X86_32 => [ pushR eax ]
            ) @
            [
                CallFunction(DirectReg entryPtrReg), (* Call the function *)
                moveRR{source=saveMLStackPtrReg, output=esp} (* Restore the ML stack pointer *)
            ] @
            (
                (* Put the floating point result into a box. *)
                case abi of
                   X86_32 =>
                    [
                        AllocStore{size=fpBoxSize, output=eax, saveRegs=[]},
                        StoreConstToMemory{toStore=fpBoxLengthWord32,
                                address={offset= ~wordSize, base=eax, index=NoIndex}},
                        FPStoreToMemory{ address={base=eax, offset=0, index=NoIndex}, precision=DoublePrecision, andPop=true },
                        StoreInitialised
                    ]
                |   _ => (* X64 The result is in xmm0 *)
                    [
                        AllocStore{size=fpBoxSize, output=eax, saveRegs=[]},
                        StoreConstToMemory{toStore=LargeInt.fromInt fpBoxSize,
                            address={offset= ~wordSize, base=eax, index=NoIndex}},
                        StoreNonWordConst{size=Size8Bit, toStore=Word8.toLargeInt F_bytes, address={offset= ~1, base=eax, index=NoIndex}},
                        XMMStoreToMemory { address={base=eax, offset=0, index=NoIndex}, precision=DoublePrecision, toStore=xmm0 },
                        StoreInitialised
                    ]                    
            ) @
            [
                (* Remove any arguments that have been passed on the stack. *)
                ReturnFromFunction 0
            ]
 
        val profileObject = createProfileObject functionName
        val newCode = codeCreate (functionName, profileObject, debugSwitches)
        val createdCode = createCodeSegment(X86OPTIMISE.optimise(newCode, code), newCode)
        (* Have to create a closure for this *)
        open Address
        val closure = allocWordData(0w1, Word8.orb (F_mutable, F_words), toMachineWord 0w0)
    in
        assignWord(closure, 0w0, toMachineWord createdCode);
        lock closure;
        closure
    end

end;
