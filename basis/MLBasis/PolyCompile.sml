
functor PolyCompile( structure StringMap : STRINGMAPSIG ) : COMPILERSIG = struct

  type 'a map = 'a StringMap.map

  local open PolyML in

  structure Environment = struct

    local open NameSpace in
    type fixities = Infixes.fixity
    type functors = Functors.functorVal
    type signatures = Signatures.signatureVal
    type structures = Structures.structureVal
    type types = TypeConstrs.typeConstr
    type values = Values.value
    end

    local open StringMap in
    type env =
      { fixes : fixities map
      , funcs : functors map
      , sigs : signatures map
      , structs : structures map
      , tys : types map
      , vals : values map
      }

    fun emptyenv () =
      { fixes = empty ()
      , funcs = empty ()
      , sigs = empty ()
      , structs = empty ()
      , tys = empty ()
      , vals = empty ()
      }

    fun update
      { fixes, funcs, sigs, structs, tys, vals }
      { fixes = fixes_map
      , funcs = funcs_map
      , sigs = sigs_map
      , structs = structs_map
      , tys = tys_map
      , vals = vals_map
      } =
      { fixes = fixes_map fixes
      , funcs = funcs_map funcs
      , sigs = sigs_map sigs
      , structs = structs_map structs
      , tys = tys_map tys
      , vals = vals_map vals
      }

    fun merge env { fixes, funcs, sigs, structs, tys, vals } =
      let
        fun append new env =
          StringMap.merge env new
          (fn (_, (_, new)) => new)
      in
        update env
        { fixes = append fixes
        , funcs = append funcs
        , sigs = append sigs
        , structs = append structs
        , tys = append tys
        , vals = append vals
        }
      end
    end

    fun convert_to_ephemeral { fixes, funcs, sigs, structs, tys, vals } = let
      fun ephemeral r =
        ( fn () => StringMap.to_list (!r)
        , fn entry =>
            r := StringMap.add (!r) entry (fn (_, (_, new)) => new)
        , fn s => StringMap.lookup (!r) s
        )
      val (allFix, enterFix, lookupFix) = ephemeral (ref fixes)
      val (allFunct, enterFunct, lookupFunct) = ephemeral (ref funcs)
      val (allSig, enterSig, lookupSig) = ephemeral (ref sigs)
      val (allStruct, enterStruct, lookupStruct) = ephemeral (ref structs)
      val (allType, enterType, lookupType) = ephemeral (ref tys)
      val (allVal, enterVal, lookupVal) = ephemeral (ref vals)
    in
      { allFix = allFix
      , enterFix = enterFix
      , lookupFix = lookupFix
      , allFunct = allFunct
      , enterFunct = enterFunct
      , lookupFunct = lookupFunct
      , allSig = allSig
      , enterSig = enterSig
      , lookupSig = lookupSig
      , allStruct = allStruct
      , enterStruct = enterStruct
      , lookupStruct = lookupStruct
      , allType = allType
      , enterType = enterType
      , lookupType = lookupType
      , allVal = allVal
      , enterVal = enterVal
      , lookupVal = lookupVal
      }
    end
  end

  type env = Environment.env

  fun compile (env : env) file : env = let
    val stream = TextIO.openIn file
    val bnds = ref NONE
    val () = compiler
      ( fn () => TextIO.input1 stream
      , (Compiler.CPResultFun (fn new => bnds := SOME new))
        :: (Compiler.CPNameSpace (Environment.convert_to_ephemeral env))
        :: nil
      ) ()
    val list = StringMap.from_list
  in
    case !bnds of
      NONE => raise Fail "Incomplete compilation: this should never happen"
    | SOME { fixes, values, structures, signatures, functors, types } =>
        { fixes =  list fixes
        , funcs = list functors
        , sigs = list signatures
        , structs = list structures
        , tys = list types
        , vals = list values
        }
  end

  end
end
