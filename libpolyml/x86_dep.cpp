/*
    Title:  Machine dependent code for i386 and X64 under Windows and Unix

    Copyright (c) 2000-7
        Cambridge University Technical Services Limited

    Further work copyright David C. J. Matthews 2011-20

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

*/

#ifdef HAVE_CONFIG_H
#include "config.h"
#elif defined(_WIN32)
#include "winconfig.h"
#else
#error "No configuration file"
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#include <stdio.h>

#ifdef HAVE_SIGNAL_H
#include <signal.h>
#endif

#ifdef HAVE_ASSERT_H 
#include <assert.h>
#define ASSERT(x)   assert(x)
#else
#define ASSERT(x)
#endif

#ifdef HAVE_STRING_H 
#include <string.h>
#endif

#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif

#if (defined(_WIN32))
#include <windows.h>
#include <excpt.h>
#endif

#include "globals.h"
#include "run_time.h"
#include "diagnostics.h"
#include "processes.h"
#include "profiling.h"
#include "machine_dep.h"
#include "scanaddrs.h"
#include "memmgr.h"
#include "rtsentry.h"

#include "sys.h" // Temporary


/**********************************************************************
 *
 * Register usage:
 *
 *  %Reax: First argument to function.  Result of function call.
 *  %Rebx: Second argument to function.
 *  %Recx: General register
 *  %Redx: Closure pointer in call.
 *  %Rebp: Points to memory used for extra registers
 *  %Resi: General register.
 *  %Redi: General register.
 *  %Resp: Stack pointer.
 *  The following apply only on the X64
 *  %R8:   Third argument to function
 *  %R9:   Fourth argument to function
 *  %R10:  Fifth argument to function
 *  %R11:  General register
 *  %R12:  General register
 *  %R13:  General register
 *  %R14:  General register
 *  %R15:  Memory allocation pointer

 *
 **********************************************************************/

#ifdef HOSTARCHITECTURE_X86_64
struct fpSaveArea {
    double fpregister[7]; // Save area for xmm0-6
};
#else
// Structure of floating point save area.
// This is dictated by the hardware.
typedef byte fpregister[10];

struct fpSaveArea {
    unsigned short cw;
    unsigned short _unused0;
    unsigned short sw;
    unsigned short _unused1;
    unsigned short tw;
    unsigned short _unused2;
    unsigned fip;
    unsigned short fcs0;
    unsigned short _unused3;
    unsigned foo;
    unsigned short fcs1;
    unsigned short _unused4;
    fpregister registers[8];
};
#endif

/* the amount of ML stack space to reserve for registers,
   C exception handling etc. The compiler requires us to
   reserve 2 stack-frames worth (2 * 20 words). We actually reserve
   slightly more than this.
*/
#if (!defined(_WIN32) && !defined(HAVE_SIGALTSTACK))
// If we can't handle signals on a separate stack make sure there's space
// on the Poly stack.
#define OVERFLOW_STACK_SIZE (50+1024)
#else
#define OVERFLOW_STACK_SIZE 50
#endif

class X86TaskData;

// This is passed as the argument vector to X86AsmSwitchToPoly.
// The offsets are built into the assembly code and the code-generator.
// localMpointer and stackPtr are updated before control returns to C.
typedef struct _AssemblyArgs {
public:
    PolyWord        *localMpointer;     // Allocation ptr + 1 word
    stackItem       *handlerRegister;   // Current exception handler
    PolyWord        *localMbottom;      // Base of memory + 1 word
    stackItem       *stackLimit;        // Lower limit of stack
    stackItem       exceptionPacket;    // Set if there is an exception
    byte            unusedRequestCode;  // No longer used.
    byte            unusedFlag;         // No longer used
    byte            returnReason;       // Reason for returning from ML.
    byte            unusedRestore;      // No longer used.
    uintptr_t       saveCStack;         // Saved C stack frame.
    PolyWord        threadId;           // My thread id.  Saves having to call into RTS for it.
    stackItem       *stackPtr;          // Current stack pointer
    byte            *noLongerUsed;      // Now removed
    byte            *heapOverFlowCall;  // These are filled in with the functions.
    byte            *stackOverFlowCall;
    byte            *stackOverFlowCallEx;
    byte            *trapHandlerEntry;
    // Saved registers, where applicable.
    stackItem       p_rax;
    stackItem       p_rbx;
    stackItem       p_rcx;
    stackItem       p_rdx;
    stackItem       p_rsi;
    stackItem       p_rdi;
#ifdef HOSTARCHITECTURE_X86_64
    stackItem       p_r8;
    stackItem       p_r9;
    stackItem       p_r10;
    stackItem       p_r11;
    stackItem       p_r12;
    stackItem       p_r13;
    stackItem       p_r14;
#endif
    struct fpSaveArea p_fp;
} AssemblyArgs;

// These next few are temporarily added for the interpreter
// This duplicates some code in reals.cpp but is now updated.
#define DOUBLESIZE (sizeof(double)/sizeof(POLYUNSIGNED))

union realdb { double dble; POLYUNSIGNED puns[DOUBLESIZE]; };

#define LGWORDSIZE (sizeof(uintptr_t) / sizeof(PolyWord))

class X86TaskData: public TaskData {
public:
    X86TaskData();
    unsigned allocReg; // The register to take the allocated space.
    POLYUNSIGNED allocWords; // The words to allocate.
    AssemblyArgs assemblyInterface;
    int saveRegisterMask; // Registers that need to be updated by a GC.

    virtual void GarbageCollect(ScanAddress *process);
    void ScanStackAddress(ScanAddress *process, stackItem &val, StackSpace *stack);
    virtual void EnterPolyCode(); // Start running ML
    virtual void InterruptCode();
    virtual bool AddTimeProfileCount(SIGNALCONTEXT *context);
    virtual void InitStackFrame(TaskData *parentTask, Handle proc, Handle arg);
    virtual void SetException(poly_exn *exc);

    // Release a mutex in exactly the same way as compiler code
    virtual Handle AtomicDecrement(Handle mutexp);
    virtual void AtomicReset(Handle mutexp);

    // Return the minimum space occupied by the stack.  Used when setting a limit.
    // N.B. This is PolyWords not native words.
    virtual uintptr_t currentStackSpace(void) const
        { return (this->stack->top - (PolyWord*)assemblyInterface.stackPtr) +
            OVERFLOW_STACK_SIZE*sizeof(uintptr_t)/sizeof(PolyWord); }

    // Increment the profile count for an allocation.  Also now used for mutex contention.
    virtual void addProfileCount(POLYUNSIGNED words)
    { addSynchronousCount(assemblyInterface.stackPtr[0].codeAddr, words); }

    // PreRTSCall: After calling from ML to the RTS we need to save the current heap pointer
    virtual void PreRTSCall(void) { TaskData::PreRTSCall();  SaveMemRegisters(); }
    // PostRTSCall: Before returning we need to restore the heap pointer.
    // If there has been a GC in the RTS call we need to create a new heap area.
    virtual void PostRTSCall(void) { SetMemRegisters(); TaskData::PostRTSCall();  }

    virtual void CopyStackFrame(StackObject *old_stack, uintptr_t old_length, StackObject *new_stack, uintptr_t new_length);

    void HeapOverflowTrap(byte *pcPtr);

    void SetMemRegisters();
    void SaveMemRegisters();
    void SetRegisterMask();

    void HandleTrap();

    PLock interruptLock;

    stackItem *get_reg(int n);

    stackItem *&regSP() { return assemblyInterface.stackPtr; }

    stackItem &regAX() { return assemblyInterface.p_rax; }
    stackItem &regBX() { return assemblyInterface.p_rbx; }
    stackItem &regCX() { return assemblyInterface.p_rcx; }
    stackItem &regDX() { return assemblyInterface.p_rdx; }
    stackItem &regSI() { return assemblyInterface.p_rsi; }
    stackItem &regDI() { return assemblyInterface.p_rdi; }
#ifdef HOSTARCHITECTURE_X86_64
    stackItem &reg8() { return assemblyInterface.p_r8; }
    stackItem &reg9() { return assemblyInterface.p_r9; }
    stackItem &reg10() { return assemblyInterface.p_r10; }
    stackItem &reg11() { return assemblyInterface.p_r11; }
    stackItem &reg12() { return assemblyInterface.p_r12; }
    stackItem &reg13() { return assemblyInterface.p_r13; }
    stackItem &reg14() { return assemblyInterface.p_r14; }
#endif

#if (defined(_WIN32))
    DWORD savedErrno;
#else
    int savedErrno;
#endif
};

class X86Dependent: public MachineDependent {
public:
    X86Dependent() {}

    // Create a task data object.
    virtual TaskData *CreateTaskData(void) { return new X86TaskData(); }

    // Initial size of stack in PolyWords
    virtual unsigned InitialStackSize(void) { return (128+OVERFLOW_STACK_SIZE) * sizeof(uintptr_t) / sizeof(PolyWord); }
    virtual void ScanConstantsWithinCode(PolyObject *addr, PolyObject *oldAddr, POLYUNSIGNED length, ScanAddress *process);

    virtual Architectures MachineArchitecture(void)
#ifndef HOSTARCHITECTURE_X86_64
         { return MA_I386; }
#elif defined(POLYML32IN64)
        { return MA_X86_64_32; }
#else
         { return MA_X86_64; }
#endif
};

// Values for the returnReason byte
enum RETURN_REASON {
    RETURN_HEAP_OVERFLOW = 1,
    RETURN_STACK_OVERFLOW = 2,
    RETURN_STACK_OVERFLOWEX = 3,
};

extern "C" {

    // These are declared in the assembly code segment.
    void X86AsmSwitchToPoly(void *);
    extern int X86AsmCallExtraRETURN_HEAP_OVERFLOW(void);
    extern int X86AsmCallExtraRETURN_STACK_OVERFLOW(void);
    extern int X86AsmCallExtraRETURN_STACK_OVERFLOWEX(void);

    POLYUNSIGNED X86AsmAtomicDecrement(PolyObject*);

    void X86TrapHandler(PolyWord threadId);
};

X86TaskData::X86TaskData(): allocReg(0), allocWords(0), saveRegisterMask(0)
{
    assemblyInterface.heapOverFlowCall = (byte*)X86AsmCallExtraRETURN_HEAP_OVERFLOW;
    assemblyInterface.stackOverFlowCall = (byte*)X86AsmCallExtraRETURN_STACK_OVERFLOW;
    assemblyInterface.stackOverFlowCallEx = (byte*)X86AsmCallExtraRETURN_STACK_OVERFLOWEX;
    assemblyInterface.trapHandlerEntry = (byte*)X86TrapHandler;
    savedErrno = 0;
}

void X86TaskData::GarbageCollect(ScanAddress *process)
{
    TaskData::GarbageCollect(process); // Process the parent first
    assemblyInterface.threadId = threadObject;

    if (stack != 0)
    {
        ASSERT(assemblyInterface.stackPtr >= (stackItem*)stack->bottom && assemblyInterface.stackPtr <= (stackItem*)stack->top);
        // Now the values on the stack.
        for (stackItem *q = assemblyInterface.stackPtr; q < (stackItem*)stack->top; q++)
            ScanStackAddress(process, *q, stack);
    }
    // Register mask
    for (int i = 0; i < 16; i++)
    {
        if (saveRegisterMask & (1 << i))
            ScanStackAddress(process, *get_reg(i), stack);
    }
}

// Process a value within the stack.
void X86TaskData::ScanStackAddress(ScanAddress *process, stackItem &stackItem, StackSpace *stack)
{
    // We may have return addresses on the stack which could look like
    // tagged values.  Check whether the value is in the code area before
    // checking whether it is untagged.
#ifdef POLYML32IN64
    // In 32-in-64 return addresses always have the top 32 bits non-zero. 
    if (stackItem.argValue < ((uintptr_t)1 << 32))
    {
        // It's either a tagged integer or an object pointer.
        if (stackItem.w().IsDataPtr())
        {
            PolyWord val = process->ScanObjectAddress(stackItem.w().AsObjPtr());
            stackItem = val;
        }
    }
    else
    {
        // Could be a code address or a stack address.
        MemSpace *space = gMem.SpaceForAddress(stackItem.codeAddr - 1);
        if (space == 0 || space->spaceType != ST_CODE) return;
        PolyObject *obj = gMem.FindCodeObject(stackItem.codeAddr);
        ASSERT(obj != 0);
        // Process the address of the start.  Don't update anything.
        process->ScanObjectAddress(obj);
    }
#else
    // The -1 here is because we may have a zero-sized cell in the last
    // word of a space.
    MemSpace *space = gMem.SpaceForAddress(stackItem.codeAddr-1);
    if (space == 0) return; // In particular we may have one of the assembly code addresses.
    if (space->spaceType == ST_CODE)
    {
        PolyObject *obj = gMem.FindCodeObject(stackItem.codeAddr);
        // If it is actually an integer it might be outside a valid code object.
        if (obj == 0)
        {
            ASSERT(stackItem.w().IsTagged()); // It must be an integer
        }
        else // Process the address of the start.  Don't update anything.
            process->ScanObjectAddress(obj);
    }
    else if (space->spaceType == ST_LOCAL && stackItem.w().IsDataPtr())
        // Local values must be word addresses.
    {
        PolyWord val = process->ScanObjectAddress(stackItem.w().AsObjPtr());
        stackItem = val;
    }
#endif
}


// Copy a stack
void X86TaskData::CopyStackFrame(StackObject *old_stack, uintptr_t old_length, StackObject *new_stack, uintptr_t new_length)
{
    /* Moves a stack, updating all references within the stack */
#ifdef POLYML32IN64
    old_length = old_length / 2;
    new_length = new_length / 2;
#endif

    stackItem *old_base  = (stackItem *)old_stack;
    stackItem *new_base  = (stackItem*)new_stack;
    stackItem *old_top   = old_base + old_length;

    /* Calculate the offset of the new stack from the old. If the frame is
       being extended objects in the new frame will be further up the stack
       than in the old one. */

    uintptr_t offset = new_base - old_base + new_length - old_length;

    stackItem *oldStackPtr = assemblyInterface.stackPtr;

    // Adjust the stack pointer and handler pointer since these point into the stack.
    assemblyInterface.stackPtr = assemblyInterface.stackPtr + offset;
    assemblyInterface.handlerRegister = assemblyInterface.handlerRegister + offset;

    // We need to adjust any values on the stack that are pointers within the stack.
    // Skip the unused part of the stack.

    size_t i = oldStackPtr - old_base;

    ASSERT (i <= old_length);

    i = old_length - i;

    stackItem *old = oldStackPtr;
    stackItem *newp = assemblyInterface.stackPtr;

    while (i--)
    {
        stackItem old_word = *old++;
        if (old_word.w().IsDataPtr() && old_word.stackAddr >= old_base && old_word.stackAddr <= old_top)
            old_word.stackAddr = old_word.stackAddr + offset;
        else if (old_word.w().IsDataPtr() && IsHeapAddress(old_word.stackAddr))
        {
            stackItem *addr = (stackItem*)old_word.w().AsStackAddr();
            if (addr >= old_base && addr <= old_top)
            {
                addr += offset;
                old_word = PolyWord::FromStackAddr((PolyWord*)addr);
            }
        }
        *newp++ = old_word;
    }
    ASSERT(old == ((stackItem*)old_stack)+old_length);
    ASSERT(newp == ((stackItem*)new_stack)+new_length);
    // And change any registers that pointed into the old stack
    for (int j = 0; j < 16; j++)
    {
        if (saveRegisterMask & (1 << j))
        {
            stackItem *regAddr = get_reg(j);
            stackItem old_word = *regAddr;
            if (old_word.w().IsDataPtr() && old_word.stackAddr >= old_base && old_word.stackAddr <= old_top)
                old_word.stackAddr = old_word.stackAddr + offset;
            else if (old_word.w().IsDataPtr() && IsHeapAddress(old_word.stackAddr))
            {
                stackItem *addr = (stackItem*)old_word.w().AsStackAddr();
                if (addr >= old_base && addr <= old_top)
                {
                    addr += offset;
                    old_word = PolyWord::FromStackAddr((PolyWord*)addr);
                }
            }
            *regAddr = old_word;
       }
    }
}

void X86TaskData::EnterPolyCode()
/* Called from "main" to enter the code. */
{
    SetMemRegisters();
    // Enter the ML code.
    X86AsmSwitchToPoly(&this->assemblyInterface);
    // This should never return
    ASSERT(0);
 }

// Called from the assembly code as a result of a trap i.e. a request for
// a GC or to extend the stack.
void X86TrapHandler(PolyWord threadId)
{
    X86TaskData* taskData = (X86TaskData*)TaskData::FindTaskForId(threadId);
    taskData->HandleTrap();
}

void X86TaskData::HandleTrap()
{
    SaveMemRegisters(); // Update globals from the memory registers.

    switch (this->assemblyInterface.returnReason)
    {

    case RETURN_HEAP_OVERFLOW:
        // The heap has overflowed.
        SetRegisterMask();
        this->HeapOverflowTrap(assemblyInterface.stackPtr[0].codeAddr); // Computes a value for allocWords only
        break;

    case RETURN_STACK_OVERFLOW:
    case RETURN_STACK_OVERFLOWEX:
    {
        SetRegisterMask();
        uintptr_t min_size; // Size in PolyWords
        if (assemblyInterface.returnReason == RETURN_STACK_OVERFLOW)
        {
            min_size = (this->stack->top - (PolyWord*)assemblyInterface.stackPtr) +
                OVERFLOW_STACK_SIZE * sizeof(uintptr_t) / sizeof(PolyWord);
        }
        else
        {
            // Stack limit overflow.  If the required stack space is larger than
            // the fixed overflow size the code will calculate the limit in %EDI.
            stackItem* stackP = regDI().stackAddr;
            min_size = (this->stack->top - (PolyWord*)stackP) +
                OVERFLOW_STACK_SIZE * sizeof(uintptr_t) / sizeof(PolyWord);
        }
        try {
            // The stack check has failed.  This may either be because we really have
            // overflowed the stack or because the stack limit value has been adjusted
            // to result in a call here.
            CheckAndGrowStack(this, min_size);
        }
        catch (IOException&) {
            // We may get an exception while handling this if we run out of store
        }
        {
            PLocker l(&interruptLock);
            // Set the stack limit.  This clears any interrupt and also sets the
            // correct value if we've grown the stack.
            this->assemblyInterface.stackLimit = (stackItem*)this->stack->bottom + OVERFLOW_STACK_SIZE;
        }
        // We're in a safe state to handle any interrupts.
        try {
            // Process any asynchronous events i.e. interrupts or kill
            processes->ProcessAsynchRequests(this);
            // Release and re-acquire use of the ML memory to allow another thread to GC.
            processes->ThreadReleaseMLMemory(this);
            processes->ThreadUseMLMemory(this);
        }
        catch (IOException&) {
            // If this resulted in an ML exception it will also raise a C++ exception.
        }
        catch (KillException&) {
            processes->ThreadExit(this);
        }
        break;
    }

    default:
        Crash("Unknown return reason code %u", this->assemblyInterface.returnReason);
    }
    SetMemRegisters();
}

void X86TaskData::InitStackFrame(TaskData *parentTaskData, Handle proc, Handle arg)
/* Initialise stack frame. */
{
    StackSpace *space = this->stack;
    StackObject * newStack = space->stack();
    uintptr_t stack_size     = space->spaceSize() * sizeof(PolyWord) / sizeof(stackItem);
    // Set the top of the stack inside the stack rather than at the end.  This wastes
    // a word but if sp is actually at the end OpenBSD segfaults because it isn't in
    // a MAP_STACK area.
    uintptr_t topStack = stack_size - 1;
    stackItem* stackTop = (stackItem*)newStack + topStack;
    *stackTop = TAGGED(0); // Set it to non-zero.
    assemblyInterface.stackPtr = stackTop;
    assemblyInterface.stackLimit = (stackItem*)space->bottom + OVERFLOW_STACK_SIZE;
    assemblyInterface.handlerRegister = stackTop;

    // Floating point save area.
    memset(&assemblyInterface.p_fp, 0, sizeof(struct fpSaveArea));
#ifndef HOSTARCHITECTURE_X86_64
    // Set the control word for 64-bit precision otherwise we get inconsistent results.
    assemblyInterface.p_fp.cw = 0x027f ; // Control word
    assemblyInterface.p_fp.tw = 0xffff; // Tag registers - all unused
#endif
    // Store the argument and the closure.
    assemblyInterface.p_rdx = proc->Word(); // Closure
    assemblyInterface.p_rax = (arg == 0) ? TAGGED(0) : DEREFWORD(arg); // Argument
    // Have to set the register mask in case we get a GC before the thread starts.
    saveRegisterMask = (1 << 2) | 1; // Rdx and rax

#ifdef POLYML32IN64
    // In 32-in-64 RBX always contains the heap base address.
    assemblyInterface.p_rbx.stackAddr = (stackItem*)globalHeapBase;
#endif
}

// In Solaris-x86 the registers are named EIP and ESP.
#if (!defined(REG_EIP) && defined(EIP))
#define REG_EIP EIP
#endif
#if (!defined(REG_ESP) && defined(ESP))
#define REG_ESP ESP
#endif


// Get the PC and SP(stack) from a signal context.  This is needed for profiling.
// This version gets the actual sp and pc if we are in ML.
// N.B. This must not call malloc since we're in a signal handler.
bool X86TaskData::AddTimeProfileCount(SIGNALCONTEXT *context)
{
    stackItem * sp = 0;
    POLYCODEPTR pc = 0;
    if (context != 0)
    {
        // The tests for HAVE_UCONTEXT_T, HAVE_STRUCT_SIGCONTEXT and HAVE_WINDOWS_H need
        // to follow the tests in processes.h.
#if defined(HAVE_WINDOWS_H)
#ifdef _WIN64
        sp = (stackItem *)context->Rsp;
        pc = (POLYCODEPTR)context->Rip;
#else
        // Windows 32 including cygwin.
        sp = (stackItem *)context->Esp;
        pc = (POLYCODEPTR)context->Eip;
#endif
#elif defined(HAVE_UCONTEXT_T)
#ifdef HAVE_MCONTEXT_T_GREGS
        // Linux
#ifndef HOSTARCHITECTURE_X86_64
        pc = (byte*)context->uc_mcontext.gregs[REG_EIP];
        sp = (stackItem*)context->uc_mcontext.gregs[REG_ESP];
#else /* HOSTARCHITECTURE_X86_64 */
        pc = (byte*)context->uc_mcontext.gregs[REG_RIP];
        sp = (stackItem*)context->uc_mcontext.gregs[REG_RSP];
#endif /* HOSTARCHITECTURE_X86_64 */
#elif defined(HAVE_MCONTEXT_T_MC_ESP)
       // FreeBSD
#ifndef HOSTARCHITECTURE_X86_64
        pc = (byte*)context->uc_mcontext.mc_eip;
        sp = (stackItem*)context->uc_mcontext.mc_esp;
#else /* HOSTARCHITECTURE_X86_64 */
        pc = (byte*)context->uc_mcontext.mc_rip;
        sp = (stackItem*)context->uc_mcontext.mc_rsp;
#endif /* HOSTARCHITECTURE_X86_64 */
#else
       // Mac OS X
#ifndef HOSTARCHITECTURE_X86_64
#if(defined(HAVE_STRUCT_MCONTEXT_SS)||defined(HAVE_STRUCT___DARWIN_MCONTEXT32_SS))
        pc = (byte*)context->uc_mcontext->ss.eip;
        sp = (stackItem*)context->uc_mcontext->ss.esp;
#elif(defined(HAVE_STRUCT___DARWIN_MCONTEXT32___SS))
        pc = (byte*)context->uc_mcontext->__ss.__eip;
        sp = (stackItem*)context->uc_mcontext->__ss.__esp;
#endif
#else /* HOSTARCHITECTURE_X86_64 */
#if(defined(HAVE_STRUCT_MCONTEXT_SS)||defined(HAVE_STRUCT___DARWIN_MCONTEXT64_SS))
        pc = (byte*)context->uc_mcontext->ss.rip;
        sp = (stackItem*)context->uc_mcontext->ss.rsp;
#elif(defined(HAVE_STRUCT___DARWIN_MCONTEXT64___SS))
        pc = (byte*)context->uc_mcontext->__ss.__rip;
        sp = (stackItem*)context->uc_mcontext->__ss.__rsp;
#endif
#endif /* HOSTARCHITECTURE_X86_64 */
#endif
#elif defined(HAVE_STRUCT_SIGCONTEXT)
#if defined(HOSTARCHITECTURE_X86_64) && defined(__OpenBSD__)
        // CPP defines missing in amd64/signal.h in OpenBSD
        pc = (byte*)context->sc_rip;
        sp = (stackItem*)context->sc_rsp;
#else // !HOSTARCHITEXTURE_X86_64 || !defined(__OpenBSD__)
        pc = (byte*)context->sc_pc;
        sp = (stackItem*)context->sc_sp;
#endif
#endif
    }
    if (pc != 0)
    {
        // See if the PC we've got is an ML code address.
        MemSpace *space = gMem.SpaceForAddress(pc);
        if (space != 0 && (space->spaceType == ST_CODE || space->spaceType == ST_PERMANENT))
        {
            incrementCountAsynch(pc);
            return true;
        }
    }
    // See if the sp value is in the current stack.
    if (sp >= (stackItem*)this->stack->bottom && sp < (stackItem*)this->stack->top)
    {
        // We may be in the assembly code.  The top of the stack will be a return address.
        pc = sp[0].w().AsCodePtr();
        MemSpace *space = gMem.SpaceForAddress(pc);
        if (space != 0 && (space->spaceType == ST_CODE || space->spaceType == ST_PERMANENT))
        {
            incrementCountAsynch(pc);
            return true;
        }
    }
    // See if the value of regSP is a valid stack pointer.
    // This works if we happen to be in an RTS call using a "Full" call.
    // It doesn't work if we've used a "Fast" call because that doesn't save the SP.
    sp = assemblyInterface.stackPtr;
    if (sp >= (stackItem*)this->stack->bottom && sp < (stackItem*)this->stack->top)
    {
        // We may be in the run-time system.
        pc = sp[0].w().AsCodePtr();
        MemSpace *space = gMem.SpaceForAddress(pc);
        if (space != 0 && (space->spaceType == ST_CODE || space->spaceType == ST_PERMANENT))
        {
            incrementCountAsynch(pc);
            return true;
        }
    }
    // None of those worked
    return false;
}

// This is called from a different thread so we have to be careful.
void X86TaskData::InterruptCode()
{
    PLocker l(&interruptLock);
    // Set the stack limit pointer to the top of the stack to cause
    // a trap when we next check for stack overflow.
    // We use a lock here to ensure that we always use the current value of the
    // stack.  The thread we're interrupting could be growing the stack at this point.
    if (this->stack != 0) 
        this->assemblyInterface.stackLimit = (stackItem*)(this->stack->top-1);
}

// This is called from SwitchToPoly before we enter the ML code.
void X86TaskData::SetMemRegisters()
{
    // Copy the current store limits into variables before we go into the assembly code.

    // If we haven't yet set the allocation area or we don't have enough we need
    // to create one (or a new one).
    if (this->allocPointer <= this->allocLimit + this->allocWords)
    {
        if (this->allocPointer < this->allocLimit)
            Crash ("Bad length in heap overflow trap");

        // Find some space to allocate in.  Updates taskData->allocPointer and
        // returns a pointer to the newly allocated space (if allocWords != 0)
        PolyWord *space =
            processes->FindAllocationSpace(this, this->allocWords, true);
        if (space == 0)
        {
            // We will now raise an exception instead of returning.
            // Set allocWords to zero so we don't set the allocation register
            // since that could be holding the exception packet.
            this->allocWords = 0;
        }
        // Undo the allocation just now.
        this->allocPointer += this->allocWords;
    }

    if (this->allocWords != 0)
    {
        // If we have had a heap trap we actually do the allocation here.
        // We will have already garbage collected and recovered sufficient space.
        // This also happens if we have just trapped because of store profiling.
        this->allocPointer -= this->allocWords; // Now allocate
        // Set the allocation register to this area. N.B.  This is an absolute address.
        if (this->allocReg < 15)
            get_reg(this->allocReg)[0].codeAddr = (POLYCODEPTR)(this->allocPointer + 1); /* remember: it's off-by-one */
        this->allocWords = 0;
    }

    // If we have run out of store, either just above or while allocating in the RTS,
    // allocPointer and allocLimit will have been set to zero as part of the GC.  We will
    // now be raising an exception which may free some store but we need to come back here
    // before we allocate anything.  The compiled code uses unsigned arithmetic to check for
    // heap overflow but only after subtracting the space required.  We need to make sure
    // that the values are still non-negative after substracting any object size.
    if (this->allocPointer == 0) this->allocPointer += MAX_OBJECT_SIZE;
    if (this->allocLimit == 0) this->allocLimit += MAX_OBJECT_SIZE;

    this->assemblyInterface.localMbottom = this->allocLimit + 1;
    this->assemblyInterface.localMpointer = this->allocPointer + 1;
    // If we are profiling store allocation we set mem_hl so that a trap
    // will be generated.
    if (profileMode == kProfileStoreAllocation)
        this->assemblyInterface.localMbottom = this->assemblyInterface.localMpointer;

    this->assemblyInterface.threadId = this->threadObject;
}

// This is called whenever we have returned from ML to C.
void X86TaskData::SaveMemRegisters()
{
    this->allocPointer = this->assemblyInterface.localMpointer - 1;
    this->allocWords = 0;
    this->assemblyInterface.exceptionPacket = TAGGED(0);
    this->saveRegisterMask = 0;
}

// Called on a GC or stack overflow trap.  The register mask
// is in the bytes after the trap call.
void X86TaskData::SetRegisterMask()
{
    byte *pc = assemblyInterface.stackPtr[0].codeAddr;
    if (*pc == 0xcd) // CD - INT n is used for a single byte
    {
        pc++;
        saveRegisterMask = *pc++;
    }
    else if (*pc == 0xca) // CA - FAR RETURN is used for a two byte mask
    {
        pc++;
        saveRegisterMask = pc[0] | (pc[1] << 8);
        pc += 2;
    }
    assemblyInterface.stackPtr[0].codeAddr = pc;
}

stackItem *X86TaskData::get_reg(int n)
/* Returns a pointer to the register given by n. */
{
    switch (n) 
    {
    case 0: return &assemblyInterface.p_rax;
    case 1: return &assemblyInterface.p_rcx;
    case 2: return &assemblyInterface.p_rdx;
    case 3: return &assemblyInterface.p_rbx;
        // Should not have rsp or rbp.
    case 6: return &assemblyInterface.p_rsi;
    case 7: return &assemblyInterface.p_rdi;
#ifdef HOSTARCHITECTURE_X86_64
    case 8: return &assemblyInterface.p_r8;
    case 9: return &assemblyInterface.p_r9;
    case 10: return &assemblyInterface.p_r10;
    case 11: return &assemblyInterface.p_r11;
    case 12: return &assemblyInterface.p_r12;
    case 13: return &assemblyInterface.p_r13;
    case 14: return &assemblyInterface.p_r14;
    // R15 is the heap pointer so shouldn't occur here.
#endif /* HOSTARCHITECTURE_X86_64 */
    default: Crash("Unknown register %d\n", n);
    }
}

// Called as a result of a heap overflow trap
void X86TaskData::HeapOverflowTrap(byte *pcPtr)
{
    X86TaskData *mdTask = this;
    POLYUNSIGNED wordsNeeded = 0;
    // The next instruction, after any branches round forwarding pointers or pop
    // instructions, will be a store of register containing the adjusted heap pointer.
    // We need to find that register and the value in it in order to find out how big
    // the area we actually wanted is.  N.B.  The code-generator and assembly code
    // must generate the correct instruction sequence.
//    byte *pcPtr = assemblyInterface.programCtr;
    while (true)
    {
        if (pcPtr[0] == 0xeb)
        {
            // Forwarding pointer
            if (pcPtr[1] >= 128) pcPtr += 256 - pcPtr[1] + 2;
            else pcPtr += pcPtr[1] + 2;
        }
        else if ((pcPtr[0] & 0xf8) == 0x58) // Pop instruction.
            pcPtr++;
        else if (pcPtr[0] == 0x41 && ((pcPtr[1] & 0xf8) == 0x58)) // Pop with Rex prefix
            pcPtr += 2;
        else break;
    }
#ifndef HOSTARCHITECTURE_X86_64
    // This should be movl REG,0[%ebp].
    ASSERT(pcPtr[0] == 0x89);
    mdTask->allocReg = (pcPtr[1] >> 3) & 7; // Remember this until we allocate the memory
    stackItem *reg = get_reg(mdTask->allocReg);
    stackItem reg_val = *reg;
    // The space we need is the difference between this register
    // and the current value of newptr.
    // The +1 here is because assemblyInterface.localMpointer is A.M.pointer +1.  The reason
    // is that after the allocation we have the register pointing at the address we will
    // actually use.
    wordsNeeded = (this->allocPointer - (PolyWord*)reg_val.stackAddr) + 1;
    *reg = TAGGED(0); // Clear this - it's not a valid address.
    /* length in words, including length word */

    ASSERT (wordsNeeded <= (1<<24)); /* Max object size including length/flag word is 2^24 words.  */
#else /* HOSTARCHITECTURE_X86_64 */
    ASSERT(pcPtr[1] == 0x89 || pcPtr[1] == 0x8b);
    if (pcPtr[1] == 0x89)
    {
        // New (5.4) format.  This should be movq REG,%r15
        ASSERT(pcPtr[0] == 0x49 || pcPtr[0] == 0x4d);
        mdTask->allocReg = (pcPtr[2] >> 3) & 7; // Remember this until we allocate the memory
        if (pcPtr[0] & 0x4) mdTask->allocReg += 8;
    }
    else
    {
        // Alternative form of movq REG,%r15
        ASSERT(pcPtr[0] == 0x4c || pcPtr[0] == 0x4d);
        mdTask->allocReg = pcPtr[2] & 7; // Remember this until we allocate the memory
        if (pcPtr[0] & 0x1) mdTask->allocReg += 8;
    }
    stackItem *reg = get_reg(this->allocReg);
    stackItem reg_val = *reg;
    wordsNeeded = (POLYUNSIGNED)((this->allocPointer - (PolyWord*)reg_val.stackAddr) + 1);
    *reg = TAGGED(0); // Clear this - it's not a valid address.
 #endif /* HOSTARCHITECTURE_X86_64 */
    if (profileMode == kProfileStoreAllocation)
        addProfileCount(wordsNeeded);

    mdTask->allocWords = wordsNeeded; // The actual allocation is done in SetMemRegisters.
}

void X86TaskData::SetException(poly_exn *exc)
// The RTS wants to raise an exception packet.  Normally this is as the
// result of an RTS call in which case the caller will check this.  It can
// also happen in a trap.
{
    assemblyInterface.exceptionPacket = (PolyWord)exc; // Set for direct calls.
}

// Decode and process an effective address.  There may
// be a constant address in here but in any case we need
// to decode it to work out where the next instruction starts.
// If this is an lea instruction any addresses are just constants
// so must not be treated as addresses.
static void skipea(PolyObject *base, byte **pt, ScanAddress *process, bool lea)
{
    unsigned int modrm = *((*pt)++);
    unsigned int md = modrm >> 6;
    unsigned int rm = modrm & 7;

    if (md == 3) { } /* Register. */
    else if (rm == 4)
    {
        /* s-i-b present. */
        unsigned int sib = *((*pt)++);

        if (md == 0)
        {
            if ((sib & 7) == 5) 
            {
                if (! lea) {
#ifndef HOSTARCHITECTURE_X86_64
                    process->ScanConstant(base, *pt, PROCESS_RELOC_DIRECT);
#endif /* HOSTARCHITECTURE_X86_64 */
                }
                (*pt) += 4;
            }
        }
        else if (md == 1) (*pt)++;
        else if (md == 2) (*pt) += 4;
    }
    else if (md == 0 && rm == 5)
    {
        if (!lea) {
#ifndef HOSTARCHITECTURE_X86_64
            /* Absolute address. */
            process->ScanConstant(base, *pt, PROCESS_RELOC_DIRECT);
#endif /* HOSTARCHITECTURE_X86_64 */
        }
        *pt += 4;
    }
    else
    {
        if (md == 1) *pt += 1;
        else if (md == 2) *pt += 4;
    }
}

/* Added to deal with constants within the
   code rather than in the constant area.  The constant
   area is still needed for the function name.
   DCJM 2/1/2001 
*/
void X86Dependent::ScanConstantsWithinCode(PolyObject *addr, PolyObject *old, POLYUNSIGNED length, ScanAddress *process)
{
    byte *pt = (byte*)addr;
    PolyWord *end = addr->Offset(length - 1);
#ifdef POLYML32IN64
    // If this begins with enter-int it's interpreted code - ignore
    if (pt[0] == 0xff && pt[1] == 0x55 && pt[2] == 0x48) return;
#endif

    while (true)
    {
        // Escape prefixes come before any Rex byte
        if (*pt == 0xf2 || *pt == 0xf3 || *pt == 0x66)
            pt++;
#ifdef HOSTARCHITECTURE_X86_64
        // REX prefixes.  Set this first.
        byte lastRex;
        if (*pt >= 0x40 && *pt <= 0x4f)
            lastRex = *pt++;
        else
            lastRex = 0;

        //printf("pt=%p *pt=%x\n", pt, *pt);

#endif /* HOSTARCHITECTURE_X86_64 */
        switch (*pt)
        {
        case 0x00: return; // This is actually the first byte of the old "marker" word.
        case 0xf4: return; // Halt - now used as a marker.
        case 0x50: case 0x51: case 0x52: case 0x53:
        case 0x54: case 0x55: case 0x56: case 0x57: /* Push */
        case 0x58: case 0x59: case 0x5a: case 0x5b:
        case 0x5c: case 0x5d: case 0x5e: case 0x5f: /* Pop */
        case 0x90: /* nop */ case 0xc3: /* ret */
        case 0xf9: /* stc */ case 0xce: /* into */
        case 0xf0: /* lock. */ case 0xf3: /* rep/repe */
        case 0xa4: case 0xa5: case 0xaa: case 0xab: /* movs/stos */
        case 0xa6: /* cmpsb */ case 0x9e: /* sahf */ case 0x99: /* cqo/cdq */
            pt++; break;

        case 0x70: case 0x71: case 0x72: case 0x73: case 0x74: case 0x75: case 0x76: case 0x77:
        case 0x78: case 0x79: case 0x7a: case 0x7b: case 0x7c: case 0x7d: case 0x7e: case 0x7f:
        case 0xeb:
            /* short jumps. */
        case 0xcd: /* INT - now used for a register mask */
        case 0xa8: /* TEST_ACC8 */
        case 0x6a: /* PUSH_8 */
            pt += 2; break;

        case 0xc2: /* RET_16 */
        case 0xca: /* FAR RET 16 - used for a register mask */
            pt += 3; break;

        case 0x8d: /* leal. */
            pt++; skipea(addr, &pt, process, true); break;

        case 0x03: case 0x0b: case 0x13: case 0x1b:
        case 0x23: case 0x2b: case 0x33: case 0x3b: /* Add r,ea etc. */
        case 0x88: /* MOVB_R_A */ case 0x89: /* MOVL_R_A */
        case 0x8b: /* MOVL_A_R */
        case 0x62: /* BOUNDL */
        case 0xff: /* Group5 */
        case 0xd1: /* Group2_1_A */
        case 0x8f: /* POP_A */
        case 0xd3: /* Group2_CL_A */
        case 0x87: // XCHNG
        case 0x63: // MOVSXD
            pt++; skipea(addr, &pt, process, false); break;

        case 0xf6: /* Group3_a */
            {
                int isTest = 0;
                pt++;
                /* The test instruction has an immediate operand. */
                if ((*pt & 0x38) == 0) isTest = 1;
                skipea(addr, &pt, process, false);
                if (isTest) pt++;
                break;
            }

        case 0xf7: /* Group3_A */
            {
                int isTest = 0;
                pt++;
                /* The test instruction has an immediate operand. */
                if ((*pt & 0x38) == 0) isTest = 1;
                skipea(addr, &pt, process, false);
                if (isTest) pt += 4;
                break;
            }

        case 0xc1: /* Group2_8_A */
        case 0xc6: /* MOVB_8_A */
        case 0x83: /* Group1_8_A */
        case 0x80: /* Group1_8_a */
        case 0x6b: // IMUL Ev,Ib
            pt++; skipea(addr, &pt, process, false); pt++; break;

        case 0x69: // IMUL Ev,Iv
            pt++; skipea(addr, &pt, process, false); pt += 4; break;

        case 0x81: /* Group1_32_A */
            {
                pt ++;
#ifndef HOSTARCHITECTURE_X86_64
                unsigned opCode = *pt;
#endif
                skipea(addr, &pt, process, false);
                // Only check the 32 bit constant if this is a comparison.
                // For other operations this may be untagged and shouldn't be an address.
#ifndef HOSTARCHITECTURE_X86_64
                if ((opCode & 0x38) == 0x38)
                    process->ScanConstant(addr, pt, PROCESS_RELOC_DIRECT);
#endif
                pt += 4;
                break;
            }

        case 0xe8: case 0xe9:
            // Long jump and call.  These are used to call constant (known) functions
            // and also long jumps within the function.
            {
                pt++;
                POLYSIGNED disp = (pt[3] & 0x80) ? -1 : 0; // Set the sign just in case.
                for(unsigned i = 4; i > 0; i--)
                    disp = (disp << 8) | pt[i-1];
                byte *absAddr = pt + disp + 4; // The address is relative to AFTER the constant

                // If the new address is within the current piece of code we don't do anything
                if (absAddr >= (byte*)addr && absAddr < (byte*)end) {}
                else {
#ifdef HOSTARCHITECTURE_X86_64
                    ASSERT(sizeof(PolyWord) == 4); // Should only be used internally on x64
#endif /* HOSTARCHITECTURE_X86_64 */
                    if (addr != old)
                    {
                        // The old value of the displacement was relative to the old address before
                        // we copied this code segment.
                        // We have to correct it back to the original address.
                        absAddr = absAddr - (byte*)addr + (byte*)old;
                        // We have to correct the displacement for the new location and store
                        // that away before we call ScanConstant.
                        size_t newDisp = absAddr - pt - 4;
                        byte* wr = gMem.SpaceForAddress(pt)->writeAble(pt);
                        for (unsigned i = 0; i < 4; i++)
                        {
                            wr[i] = (byte)(newDisp & 0xff);
                            newDisp >>= 8;
                        }
                    }
                    process->ScanConstant(addr, pt, PROCESS_RELOC_I386RELATIVE);
                }
                pt += 4;
                break;
            }

        case 0xc7:/* MOVL_32_A */
            {
                pt++;
                if ((*pt & 0xc0) == 0x40 /* Byte offset or sib present */ &&
                    ((*pt & 7) != 4) /* But not sib present */ && pt[1] == 256-sizeof(PolyWord))
                {
                    /* We may use a move instruction to set the length
                       word on a new segment.  We mustn't try to treat this as a constant.  */
                    pt += 6; /* Skip the modrm byte, the offset and the constant. */
                }
                else
                {
                    skipea(addr, &pt, process, false);
#ifndef HOSTARCHITECTURE_X86_64
                    // This isn't used for addresses even in 32-in-64
                    process->ScanConstant(addr, pt, PROCESS_RELOC_DIRECT);
#endif /* HOSTARCHITECTURE_X86_64 */
                    pt += 4;
                }
                break;
            }

        case 0xb8: case 0xb9: case 0xba: case 0xbb:
        case 0xbc: case 0xbd: case 0xbe: case 0xbf: /* MOVL_32_64_R */
            pt ++;
#ifdef HOSTARCHITECTURE_X86_64
            if ((lastRex & 8) == 0)
                pt += 4; // 32-bit mode on 64-bits
            else
#endif /* HOSTARCHITECTURE_X86_64 */
            {
                // This is used in native 32-bit for constants and in
                // 32-in-64 for the special case of an absolute address.
                process->ScanConstant(addr, pt, PROCESS_RELOC_DIRECT);
                pt += sizeof(uintptr_t);
            }
            break;

        case 0x68: /* PUSH_32 */
            pt ++;
#if (!defined(HOSTARCHITECTURE_X86_64))
            process->ScanConstant(addr, pt, PROCESS_RELOC_DIRECT);
#endif
            pt += 4;
            break;

        case 0x0f: /* ESCAPE */
            {
                pt++;
                switch (*pt)
                {
                case 0xb6: /* movzl */
                case 0xb7: // movzw
                case 0xbe: // movsx
                case 0xbf: // movsx
                case 0xc1: /* xaddl */
                case 0xae: // ldmxcsr/stmxcsr
                case 0xaf: // imul
                case 0x40: case 0x41: case 0x42: case 0x43: case 0x44: case 0x45: case 0x46: case 0x47:
                case 0x48: case 0x49: case 0x4a: case 0x4b: case 0x4c: case 0x4d: case 0x4e: case 0x4f:
                    // cmov
                    pt++; skipea(addr, &pt, process, false); break;

                case 0x80: case 0x81: case 0x82: case 0x83:
                case 0x84: case 0x85: case 0x86: case 0x87:
                case 0x88: case 0x89: case 0x8a: case 0x8b:
                case 0x8c: case 0x8d: case 0x8e: case 0x8f:
                    /* Conditional branches with 32-bit displacement. */
                    pt += 5; break;

                case 0x90: case 0x91: case 0x92: case 0x93:
                case 0x94: case 0x95: case 0x96: case 0x97:
                case 0x98: case 0x99: case 0x9a: case 0x9b:
                case 0x9c: case 0x9d: case 0x9e: case 0x9f:
                    /* SetCC. */
                    pt++; skipea(addr, &pt, process, false); break;

                // These are SSE2 instructions
                case 0x10: case 0x11: case 0x58: case 0x5c: case 0x59: case 0x5e:
                case 0x2e: case 0x2a: case 0x54: case 0x57: case 0x5a: case 0x6e:
                case 0x7e: case 0x2c: case 0x2d:
                    pt++; skipea(addr, &pt, process, false); break;

                case 0x73: // PSRLDQ - EA,imm
                    pt++; skipea(addr, &pt, process, false); pt++;  break;

                default: Crash("Unknown opcode %d at %p\n", *pt, pt);
                }
                break;
            }

        case 0xd8: case 0xd9: case 0xda: case 0xdb:
        case 0xdc: case 0xdd: case 0xde: case 0xdf: // Floating point escape instructions
            {
                pt++;
                if ((*pt & 0xe0) == 0xe0) pt++;
                else skipea(addr, &pt, process, false);
                break;
            }

        default: Crash("Unknown opcode %d at %p\n", *pt, pt);
        }
    }
}

// Increment the value contained in the first word of the mutex.
Handle X86TaskData::AtomicDecrement(Handle mutexp)
{
    PolyObject *p = DEREFHANDLE(mutexp);
    POLYUNSIGNED result = X86AsmAtomicDecrement(p);
    return this->saveVec.push(PolyWord::FromUnsigned(result));
}

// Release a mutex.  Because the atomic increment and decrement
// use the hardware LOCK prefix we can simply set this to zero.
void X86TaskData::AtomicReset(Handle mutexp)
{
    DEREFHANDLE(mutexp)->Set(0, TAGGED(0));
}

static X86Dependent x86Dependent;

MachineDependent *machineDependent = &x86Dependent;

extern "C" {
    POLYEXTERNALSYMBOL void *PolyX86GetThreadData();
}

// Return the address of assembly data for the current thread.  This is normally in
// RBP except if we are in a callback.
void *PolyX86GetThreadData()
{
    // We should get the task data for the thread that is running this code.
    // If this thread has been created by the foreign code we will have to
    // create a new one here.
    TaskData* taskData = processes->GetTaskDataForThread();
    if (taskData == 0)
    {
        try {
            taskData = processes->CreateNewTaskData(0, 0, 0, TAGGED(0));
        }
        catch (std::bad_alloc&) {
            ::Exit("Unable to create thread data - insufficient memory");
        }
        catch (MemoryException&) {
            ::Exit("Unable to create thread data - insufficient memory");
        }
    }
    return &((X86TaskData*)taskData)->assemblyInterface;
}

struct _entrypts machineSpecificEPT[] =
{
    { "PolyX86GetThreadData",           (polyRTSFunction)& PolyX86GetThreadData },

    { NULL, NULL} // End of list.
};

