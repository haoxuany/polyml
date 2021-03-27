
structure Lex = struct
  open Symbols

  (* haoxuany: imperative, so be careful *)
  type lex =
  { stream : unit -> char option
  , name : string
  , lineno : int ref
  , byteno : int ref
  }

  type range = { from : int, to : int }
  type location = { line : range, byte : range }

  fun error ({name, lineno, byteno, ...}) msg =
    raise Fail (String.concat [name, ":", Int.toString (!lineno), ":", Int.toString (!byteno), ":", msg])

  datatype lexstate =
    Token of sym
  | Skip

  fun lex ((state as { stream, name, lineno, byteno, ... }) : lex) = let
    (* Eta expanded to get around value restriction *)
    val error = fn msg => error state msg

    fun bump r = r := !r + 1

    val stream = fn () => (
      case stream () of
        NONE => NONE
      | SOME (c as #"\n") => (SOME c) before (bump byteno; bump lineno)
      | SOME c => (SOME c) before (bump byteno)
    )

    fun lexstart () =
      let

        (* Lexing esclosed strings *)
        fun lexstring s =
          case stream () of
            NONE => error "Unexpected EOF, missing string terminator for open string"
          | SOME c => let in
              case c of
                #"\"" => String.implode (List.rev s)
              | #"\n" => error "Unexpected EOL, missing string terminator for open string"
              | #"\\" => lexstring ((lexescapechar ()) :: s)
              | _ => lexstring (c :: s)
            end

        (* Unlikely to actually appear in an ML basis string, unless they are using a nonascii
        * file path or spaces. Handle this one day *)
        and lexescapechar () = error "Escape characters not parsable"

        (* Lexing nested comments *)
        and lexcomment level =
          case level of
            0 => ()
          | _ => let in
            case stream () of
              NONE => error "Unexpected EOF, missing comment terminator for open comment"
            | SOME #"*" => let in
              case stream () of
                NONE => error "Unexpected EOF, missing comment terminator for open comment"
              | SOME #")" => lexcomment (level - 1)
              | _ => lexcomment level
              end
            | _ => lexcomment level
            end

        (* Lexing reserved words or unquoted paths *)
        and lexid s = let
          fun lexuntil s =
            case stream () of
              NONE => s
            | SOME c =>
                (case Char.isSpace c of
                   true => s
                 | false => lexuntil (c :: s))
        in
          String.implode (List.rev (lexuntil s))
        end

        (* Grab symbol starting position *)
        val fromline = !lineno
        val frombyte = !byteno

        (* Start lexing *)
        val sym =
          case stream () of
            NONE => Token SymEOF
          | SOME c => let in
            case Char.isSpace c of
              (* Spaces don't lex to tokens *)
              true => Skip
            | false => let in
              case c of
              (* Strings *)
                #"\"" => Token (SymString (lexstring nil))
              (* This must be a comment, no parens in mlbasis *)
              | #"(" => (
                  case stream () of
                    SOME #"*" => (lexcomment 1; Skip)
                  | _ => error "Opening parens should indicate a comment, missing '*'"
                )
              (* Anything else would be either a reserved word, or a unquoted path.
              * Here, we lex until a space is reached and try to clasify. *)
              | _ => let
                  val id = lexid nil
                in
                  case Symbols.lookup id of
                    SOME s => Token s
                  | NONE => Token (SymPath id)
                end
              end
          end

        (* Grab symbol end position *)
        val toline = !lineno
        val tobyte = !byteno

      in
        case sym of
          Token s => (s, {line = {from = fromline, to = toline}, byte = {from = frombyte, to = tobyte}})
        | Skip => lexstart ()
      end

  in lexstart () end
end
