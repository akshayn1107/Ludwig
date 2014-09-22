type ident = Symbol.symbol

type binop =
   PLUS
 | MINUS
 | TIMES
 | DIVIDEDBY
 | MODULO

type unop =
   NEG

type lud_type =
   SEQ
 | INT

type exp =
   Let of stm list * exp
 | Call of exp * exp
 | Const of int32
 | PosInf
 | NegInf
 | Binop of binop * exp * exp
 | Unop of unop * exp
 | Var of ident * lud_type option
 | Tuple of exp list
 | Case of exp * ((exp * exp) list)
 | Par of exp * exp
 | FromList of exp list
 | Markede of exp Mark.marked
and stm =
   Assign of exp * exp
 | Fun of ident * (exp list) * exp
 | Markeds of stm Mark.marked

type program = (stm list)
