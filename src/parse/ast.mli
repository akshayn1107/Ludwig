type ident = Symbol.symbol

type binop =
   PLUS
 | MINUS
 | TIMES
 | DIVIDEDBY
 | MODULO
 | UNION
 | INTERSECT

type unop =
   NEG

type lud_type =
   SEQ
 | INT

type exp =
   Let of stm list * exp
 | Call of exp * exp
 | Nth of exp * exp
 | Op of binop
 | Const of int32
 | PosInf
 | NegInf
 | Binop of binop * exp * exp
 | Unop of unop * exp
 | Var of ident * lud_type option
 | Tuple of exp list
 | Case of exp * ((exp * exp) list)
 | Par of exp * exp
 | SeqFromList of exp list
 | SeqMap of exp * ident * exp
 | SeqFilter of exp * ident * exp
 | SetFromList of exp list
 | Markede of exp Mark.marked
and stm =
   Assign of exp * exp
 | Fun of ident * (exp list) * exp
 | Markeds of stm Mark.marked

type program = (stm list)
