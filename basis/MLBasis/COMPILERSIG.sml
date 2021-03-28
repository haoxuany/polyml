
signature COMPILERSIG = sig

  type 'a map

  structure Environment : sig
    type fixities
    type functors
    type signatures
    type structures
    type types
    type values

    type env

    val emptyenv : unit -> env

    val merge : env -> env -> env

    val update :
      env ->
      { fixes : fixities map -> fixities map
      , funcs : functors map -> functors map
      , sigs : signatures map -> signatures map
      , structs : structures map -> structures map
      , tys : types map -> types map
      , vals : values map -> values map
      } ->
      env
  end

  val compile :
    Environment.env ->
    string -> (* filename *)
    Environment.env
end
