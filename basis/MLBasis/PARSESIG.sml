
signature PARSESIG = sig
  type symbol

  type range = { from : int, to : int }
  type location = { line : range, byte : range }

  type lex

  datatype error =
    ExpectError of
      { expect : symbol list
      , got : symbol
      , because : symbol * location }
  | InvalidStartError of { got : symbol, for : string }

  exception ParserError of { location : location, error : error }

  val parse_top : lex -> Abt.abt_dec

  val parse_exp : lex -> Abt.abt_exp
end
