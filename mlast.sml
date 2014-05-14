structure MLAS = 
struct

datatype
    exp = int of string
        | ch of string
        | str of string
        | boolval of string
        | id of string
        | listcon of exp list
        | tuplecon of exp list
        | apply of exp * exp
        | infixexp of string * exp * exp
        | expsequence of exp list
        | letdec of dec * (exp list)
        | raisexp of exp
        | handlexp of exp * match list
        | ifthen of exp * exp * exp
        | whiledo of exp * exp
        | func of int * match list
  and
    match = match of pat * exp
  and
    pat = intpat of string
        | chpat of string
        | strpat of string
        | boolpat of string
        | idpat of string
        | wildcardpat
        | infixpat of string * pat * pat
        | tuplepat of pat list
        | listpat of pat list
        | aspat of string * pat
  and
    dec = bindval of pat * exp
        | bindvalrec of pat * exp
        | funmatch of string * match list
        | funmatches of (string * match list) list 
end;


