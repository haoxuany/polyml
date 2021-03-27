
structure Symbols = struct
  datatype sym =
    SymEOF
  | SymId of string
  | SymBasis
  | SymEq
  | SymAnd
  | SymOpen
  | SymLocal
  | SymIn
  | SymEnd
  | SymSemicolon
  | SymStructure
  | SymSignature
  | SymFunctor
  | SymPath of string
  | SymAnn
  | SymString of string
  | SymBas
  | SymLet

  fun string sym =
    case sym of
      SymEOF => ""
    | SymId s => s
    | SymBasis => "basis"
    | SymEq => "="
    | SymAnd => "and"
    | SymOpen => "open"
    | SymLocal => "local"
    | SymIn => "in"
    | SymEnd => "end"
    | SymSemicolon => ";"
    | SymStructure => "structure"
    | SymSignature => "signature"
    | SymFunctor => "functor"
    | SymPath s => s
    | SymAnn => "ann"
    | SymString s => String.concat ["\"", s, "\""]
    | SymBas => "bas"
    | SymLet => "let"

  local
    open HashArray
    val symboltable = hash 4
    val reservedwords =
      [ SymBasis
      , SymEq
      , SymAnd
      , SymOpen
      , SymLocal
      , SymIn
      , SymEnd
      , SymSemicolon
      , SymStructure
      , SymSignature
      , SymFunctor
      , SymAnn
      , SymBas
      , SymLet
      ]

    val () =
      List.app (fn s => update (symboltable, string s, s)) reservedwords
  in
    val lookup = fn s => sub (symboltable, s)
  end

end
