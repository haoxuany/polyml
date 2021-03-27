
structure Parse = struct

  local open Symbols open Abt open Lex in

  (* We really should functorize this into a queue library usage,
  * but this is easy for LR(2), and the basis itself doesn't
  * provide a queue signature. *)
  fun lookahead_stream lex = let
    val queue = ref nil
    (* This is slow and inefficient normally,
    * on the flip side this we're doing LR(2?) parsing
    * so it's not that big of a deal. *)
    fun push s = queue := (!queue @ [s])

    fun peek () =
      case !queue of
        h :: _ => h
      | nil => let
          val tok = lex ()
        in tok before (push tok) end

    fun pop () =
      case !queue of
        h :: rest => (queue := rest; h)
      | nil => lex ()
  in
    { peek = peek
    , pop = pop
    }
  end

  (* The easy cases, without basdec [;] basedec *)
  fun parse_dec_one (stream as { peek, pop }) =
    case pop () of
      (SymEOF, _) => NONE
    | (SymBasis, _) => SOME (DecBind (parse_binds stream nil))
    | (SymLocal, _) => SOME (DecLocal (
        let
          val dec1 = parse_dec_til_in stream nil
          val dec2 = parse_dec_til_end stream nil
        in
          (DecSeq dec1, DecSeq dec2)
        end
      ))
    | (SymOpen, _) => SOME (DecOpen (
        let
          fun parse_ids ids =
            case peek () of
              (SymId id, _) => (pop (); parse_ids (id :: ids))
            | _ => List.rev ids
        in parse_ids nil end
      ))
    | (SymStructure, _) => SOME (DecStrbind (parse_id_binds stream nil))
    | (SymSignature, _) => SOME (DecSigbind (parse_id_binds stream nil))
    | (SymFunctor, _) => SOME (DecFunbind (parse_id_binds stream nil))
    | (SymPath s, _) => SOME (parse_file s)
    | (SymString s, _) => SOME (parse_file s)
    | (SymAnn, _) => SOME (DecAnn (
        case pop () of
          (SymString s, _) => (
            case pop () of
              (SymIn, _) => (s, DecSeq (parse_dec_til_end stream nil))
            | (_, in_start) => raise Fail "stop"
          )
        | (_, s_start) => raise Fail "stop"
      ))
    | (_, dec_start) => raise Fail "stop"

  and parse_exp (stream as { peek, pop }) =
    case pop () of
      (SymBas, _) => ExpBasic (DecSeq (parse_dec_til_end stream nil))
    | (SymId id, _) => ExpId id
    | (SymLet, _) => ExpLet (
        let
          val dec = parse_dec_til_in stream nil
          val exp = parse_exp stream
        in
          case pop () of
            (SymEnd, _) => (DecSeq dec, exp)
          | (_, end_start) => raise Fail "stop"
        end
      )
    | (_, exp_start) => raise Fail "stop"

  (* Parses (id = basbind [ and id = basbind ]*. *)
  and parse_binds (stream as { peek, pop }) bnds =
    case peek () of
      (* Be a bit more lenient here, so it's easier for people to refactor. *)
      (SymAnd, _) => (pop (); parse_binds stream bnds)
    | (SymId id, _) => (pop ();
        case peek () of
          (SymEq, _) => (pop ();
            parse_binds stream ((id, parse_exp stream) :: bnds)
          )
        | (_, eq_start) => raise Fail "stop"
      )
    | (_, bnd_start) => List.rev bnds

  (* Parses (dec1 [;] dec2 .... decn end), also pops end at the end. *)
  and parse_dec_til_end (stream as { peek, pop }) decs =
    case peek () of
      (* Be a bit more lenient here, so it's easier for people to refactor. *)
      (SymSemicolon, _) => (pop (); parse_dec_til_end stream decs)
    | (SymEnd, _) => (pop (); List.rev decs)
    | (_, dec_start) => (
      case parse_dec_one stream of
        NONE => raise Fail "stop"
      | SOME dec => parse_dec_til_end stream (dec :: decs)
      )

  (* Parses (dec1 [;] dec2 .... decn in), also pops in at the end. *)
  and parse_dec_til_in (stream as { peek, pop }) decs =
    case peek () of
      (* Be a bit more lenient here, so it's easier for people to refactor. *)
      (SymSemicolon, _) => (pop (); parse_dec_til_in stream decs)
    | (SymIn, _) => (pop (); List.rev decs)
    | (_, dec_start) => (
      case parse_dec_one stream of
        NONE => raise Fail "stop"
      | SOME dec => parse_dec_til_in stream (dec :: decs)
      )

  (* Parses (id = id [ and id = id ]*. *)
  and parse_id_binds (stream as { peek, pop }) bnds =
    case peek () of
      (* Be a bit more lenient here, so it's easier for people to refactor. *)
      (SymAnd, _) => (pop (); parse_id_binds stream bnds)
    | (SymId s1, _) => (pop ();
        case peek () of
          (* dervied forms. *)
          (SymAnd, _) => (pop (); parse_id_binds stream ((s1, s1) :: bnds))
        | (SymEq, _) => (pop ();
          case pop () of
            (SymId s2, _) => parse_id_binds stream ((s1, s2) :: bnds)
          | (_, id_start) => raise Fail "stop"
        )
        | (_, eq_and_start) => raise Fail "stop"
      )
    | _ => List.rev bnds

  (* Parses a string into a DecFile. *)
  (* TODO: fix this. *)
  and parse_file s = DecFile (FileSML, s)

  (* Parse basedec [[;] basedec] in toplevel. *)
  and parse_dec_multi (stream as { peek, pop }) decs =
    case peek () of
      (* Be a bit more lenient here, so it's easier for people to refactor. *)
      (SymSemicolon, _) => (pop (); parse_dec_multi stream decs)
    | _ =>
      (case parse_dec_one stream of
         NONE => List.rev decs
       | SOME dec => parse_dec_multi stream (dec :: decs))

  (* Parse toplevel. *)
  and parse_top stream = DecSeq (parse_dec_multi stream nil)

  (* Everything up til this point is internal, here's the more palatable interface. *)
  val parse_top =
    fn lex => parse_top (lookahead_stream (fn () => Lex.lex lex))

  val parse_exp =
    fn lex => parse_exp (lookahead_stream (fn () => Lex.lex lex))
  end
end
