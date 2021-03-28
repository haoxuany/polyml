
signature STRINGMAPSIG = sig
  type 'a map

  val empty : unit -> 'a map

  val add :
    'a map ->
    (string * 'a) ->
    ((string * ('a * 'a)) -> 'a) -> (* (key, (old, new) -> keep) *)
    'a map
  val merge :
    'a map ->
    'a map ->
    ((string * ('a * 'a)) -> 'a) -> (* (key, (old, new) -> keep) *)
    'a map
  val lookup : 'a map -> string -> 'a option
  val remove : 'a map -> string -> ('a map * 'a option)

  val from_list : (string * 'a) list -> 'a map
  val to_list : 'a map -> (string * 'a) list
end
