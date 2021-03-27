
functor Parse(
  structure Lex : LEXSIG
    where type symbol = Symbols.sym
) :>
PARSESIG
  where type symbol = Symbols.sym
  and type lex = Lex.lex
= struct

  local open Symbols open Abt open Lex in

  type symbol = sym
  type range = range
  type location = location
  type lex = lex

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

  datatype error =
    ExpectError of { expect : sym list, got : sym, because : sym * location }
  | InvalidStartError of { got : sym, for : string }

  exception ParserError of { location : location, error : error }

  fun expect_error location expect got because =
    raise ParserError
      { location = location
      , error = ExpectError { expect = expect, got = got, because = because } }

  fun invalid_start_error location got for =
    raise ParserError
      { location = location
      , error = InvalidStartError { got = got, for = for } }

  (* The easy cases, without basdec [;] basedec *)
  fun parse_dec_one (stream as { peek, pop }) =
    case pop () of
      (SymEOF, _) => NONE
    | (basis as (SymBasis, _)) => SOME (DecBind (parse_binds stream nil basis))
    | (loc as (SymLocal, _)) => SOME (DecLocal (
        let
          val dec1 = parse_dec_til_in stream nil loc
          val dec2 = parse_dec_til_end stream nil loc
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
    | (structs as (SymStructure, _)) => SOME (DecStrbind (parse_id_binds stream nil structs))
    | (sigs as (SymSignature, _)) => SOME (DecSigbind (parse_id_binds stream nil sigs))
    | (funs as (SymFunctor, _)) => SOME (DecFunbind (parse_id_binds stream nil funs))
    | (SymPath s, _) => SOME (parse_file s)
    | (SymString s, _) => SOME (parse_file s)
    | (annot as (SymAnn, _)) => SOME (DecAnn (
        case pop () of
          (SymString s, _) => (
            case pop () of
              (SymIn, _) => (s, DecSeq (parse_dec_til_end stream nil annot))
            | (s, in_start) => expect_error in_start [SymIn] s annot
          )
        | (s, s_start) => expect_error s_start [SymString "annot_string"] s annot
      ))
    | (s, dec_start) => invalid_start_error dec_start s "declaration"

  and parse_exp (stream as { peek, pop }) =
    case pop () of
      (bas as (SymBas, _)) => ExpBasic (DecSeq (parse_dec_til_end stream nil bas))
    | (SymId id, _) => ExpId id
    | (letexp as (SymLet, _)) => ExpLet (
        let
          val dec = parse_dec_til_in stream nil letexp
          val exp = parse_exp stream
        in
          case pop () of
            (SymEnd, _) => (DecSeq dec, exp)
          | (s, end_start) =>
              expect_error end_start [SymEnd] s letexp
        end
      )
    | (s, exp_start) => invalid_start_error exp_start s "expression"

  (* Parses (id = basbind [ and id = basbind ]*. *)
  and parse_binds (stream as { peek, pop }) bnds because =
    case peek () of
      (* Be a bit more lenient here, so it's easier for people to refactor. *)
      (SymAnd, _) => (pop (); parse_binds stream bnds because )
    | (SymId id, _) => (pop ();
        case peek () of
          (SymEq, _) => (pop ();
            parse_binds stream ((id, parse_exp stream) :: bnds) because
          )
        | (s, eq_start) =>
            expect_error eq_start [SymEq] s because
      )
    | (_, bnd_start) => List.rev bnds

  (* Parses (dec1 [;] dec2 .... decn end), also pops end at the end. *)
  and parse_dec_til_end (stream as { peek, pop }) decs because =
    case peek () of
      (* Be a bit more lenient here, so it's easier for people to refactor. *)
      (SymSemicolon, _) => (pop (); parse_dec_til_end stream decs because)
    | (SymEnd, _) => (pop (); List.rev decs)
    | (s, dec_start) => (
      case parse_dec_one stream of
        NONE => expect_error dec_start [SymEnd] s because
      | SOME dec => parse_dec_til_end stream (dec :: decs) because
      )

  (* Parses (dec1 [;] dec2 .... decn in), also pops in at the end. *)
  and parse_dec_til_in (stream as { peek, pop }) decs because =
    case peek () of
      (* Be a bit more lenient here, so it's easier for people to refactor. *)
      (SymSemicolon, _) => (pop (); parse_dec_til_in stream decs because)
    | (SymIn, _) => (pop (); List.rev decs)
    | (s, dec_start) => (
      case parse_dec_one stream of
        NONE => expect_error dec_start [SymIn] s because
      | SOME dec => parse_dec_til_in stream (dec :: decs) because
      )

  (* Parses (id = id [ and id = id ]*. *)
  and parse_id_binds (stream as { peek, pop }) bnds because =
    case peek () of
      (* Be a bit more lenient here, so it's easier for people to refactor. *)
      (SymAnd, _) => (pop (); parse_id_binds stream bnds because)
    | (SymId s1, _) => (pop ();
        case peek () of
          (* derivied forms. *)
          (SymAnd, _) => (pop (); parse_id_binds stream ((s1, s1) :: bnds) because)
        | (SymEq, _) => (pop ();
          case pop () of
            (SymId s2, _) => parse_id_binds stream ((s1, s2) :: bnds) because
          | (s, id_start) => expect_error id_start [SymId "id"] s because
        )
        | _ => List.rev ((s1, s1) :: bnds)
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
