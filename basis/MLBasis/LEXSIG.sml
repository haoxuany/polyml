
(* Imperative lexer signature. *)
signature LEXSIG = sig
  type symbol

  type range = { from : int, to : int }
  type location = { line : range, byte : range }

  type lex

  val create : { stream : unit -> char option, name : string, lineno : int, byteno : int } -> lex

  exception LexerError of { name : string, lineno : int, byteno : int, msg : string }

  val lex : lex -> symbol * location
end
