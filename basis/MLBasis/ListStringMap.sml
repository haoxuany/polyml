
(* This is slow but easy to implement and great for testing and printing *)
structure ListStringMap :> STRINGMAPSIG = struct
  type 'a map = (string * 'a) list

  val empty = fn () => nil

  (* dumb implementation, but requires no brainpower to read *)
  fun find l s =
    List.find (fn (s', _) => s = s') l

  fun add l (s : string, v : 'a) f =
    case find l s of
      NONE => (s, v) :: l
    | SOME _ =>
        List.map
        (fn (s', old : 'a) =>
          if s = s' then (s, f (s, (old, v))) else (s', old)) l

  fun merge a b f =
    List.foldl (fn (b, l) => add l b f) a b

  fun lookup l s =
    case find l s of
      NONE => NONE
    | SOME (_, v) => SOME v

  fun remove l s =
    case find l s of
      NONE => (l, NONE)
    | SOME (_, v) =>
        (List.filter (fn (s', _) => not (s = s')) l, SOME v)

  fun from_list l = l
  fun to_list l = l

end
