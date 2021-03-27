
structure Abt = struct
  type id = string

  datatype filetype =
    FileMLB
  | FileSML

  datatype abt_dec =
    DecBind of (id * abt_exp) list
  | DecLocal of abt_dec * abt_dec
  | DecOpen of id list
  | DecStrbind of (id * id) list
  | DecSigbind of (id * id) list
  | DecFunbind of (id * id) list
  | DecSeq of abt_dec list
  | DecFile of filetype * string
  | DecAnn of string * abt_dec

  and abt_exp =
    ExpBasic of abt_dec
  | ExpId of id
  | ExpLet of abt_dec * abt_exp
end
