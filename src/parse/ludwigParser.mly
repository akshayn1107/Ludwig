(* Author: Akshay Nanavati (ananavat@andrew.cmu.edu)
 *
 * Parser for Ludwig, compiled by ocamlyacc.
 *)

%{
module A = Ast

let ploc (left, right) =
  Parsing.rhs_start left, Parsing.rhs_end right
let marke e (left, right) =
  A.Markede (Mark.mark' (e, ParseState.ext (ploc (left, right))))
let marks e (left, right) =
  A.Markeds (Mark.mark' (e, ParseState.ext (ploc (left, right))))

%}

%token EOF
%token SEMI
%token <Int32.t> INTCONST
%token <Symbol.symbol> IDENT
%token LBRACKET RBRACKET
%token LPAREN RPAREN
%token TILDA
%token ASSIGN
%token PLUS MINUS
%token STAR SLASH
%token PERCENT LARROW
%token BAR DBLBAR
%token APPEND INTERSECT
%token OP
%token COMMA
%token ELIPSES CONSMAP CONSFILTER
%token MATCHARROW
%token SEQCONSL SEQCONSR SETCONSL SETCONSR
%token CASE OF LET VAL
%token IN END FUN
%token NEGINF POSINF
%token REDUCE

%type <Ast.stm list> program

%left DBLBAR
%left PLUS MINUS
%left STAR SLASH PERCENT
%left APPEND INTERSECT
%right UNARY
%left LPAREN
%left BAR
%left LBRACKET RBRACKET
%left IDENT INTCONST

%start program

%%

program :
    stm_list                      { $1 }
  ;

stm_list :
    /* empty */                    { [] }
  | stm stm_list                   { $1 :: $2 }
  ;

stm :
   VAL exp ASSIGN exp              { marks (A.Assign ($2, $4)) (1, 4) }
 | FUN IDENT LPAREN exp_list RPAREN ASSIGN exp
                                   { marks (A.Fun ($2, $4, $7)) (1, 7) }
 ;

exp :
   IDENT                           { marke (A.Var ($1, None)) (1, 1) }
 | INTCONST                        { marke (A.Const ($1)) (1, 1) }
 | exp LBRACKET exp RBRACKET       { marke (A.Nth($1, $3)) (1, 4) }
 | exp PLUS exp                    { marke (A.Binop (A.PLUS, $1, $3)) (1, 3) }
 | exp APPEND exp                  { marke (A.Binop (A.UNION, $1, $3)) (1, 3) }
 | exp INTERSECT exp               { marke (A.Binop (A.INTERSECT, $1, $3)) (1, 3) }
 | CASE exp OF rule_list END       { marke (A.Case ($2, $4)) (1, 5) }
 | POSINF                          { marke (A.PosInf) (1, 1) }
 | NEGINF                          { marke (A.NegInf) (1, 1) }
 | exp DBLBAR exp                  { marke (A.Par ($1, $3)) (1, 3) }
 | LET stm_list IN exp END         { marke (A.Let ($2, $4)) (1, 5) }
 | tuple                           { $1 }
 | exp IDENT                       { marke (A.Call ($1, marke (A.Tuple [A.Var ($2, None)]) (2, 2))) (1, 2) }
 | exp INTCONST                    { marke (A.Call ($1, marke (A.Tuple [A.Const $2]) (2, 2))) (1, 2) }
 | exp tuple                       { marke (A.Call ($1, $2)) (1, 2) }
 | SEQCONSL exp_list SEQCONSR      { marke (A.SeqFromList $2) (1, 3) }
 | SEQCONSL exp CONSMAP IDENT LARROW exp SEQCONSR
                                   { marke (A.SeqMap ($2, $4, $6)) (1, 7) }
 | SEQCONSL IDENT LARROW exp CONSFILTER exp SEQCONSR
                                   { marke (A.SeqFilter ($6, $2, $4)) (1, 7) }
 | SETCONSL exp_list SETCONSR      { marke (A.SetFromList $2) (1, 3) }
 | opfun                           { $1 }
 ;

tuple :
   LPAREN exp_list RPAREN          { A.Tuple $2 }
 ;

opfun :
   OP PLUS                         { marke (A.Op (A.PLUS)) (1, 2) }
 | OP APPEND                       { marke (A.Op (A.UNION)) (1, 2) }
 | OP INTERSECT                    { marke (A.Op (A.INTERSECT)) (1, 2) }
 ;

exp_listf :
   COMMA exp                       { [$2] }
 | COMMA exp exp_listf             { $2 :: $3 }
 ;

exp_list :
   /* empty */                     { [] }
 | exp                             { [$1] }
 | exp exp_listf                   { $1 :: $2 }
 ;

rule :
   exp MATCHARROW exp              { ($1, $3) }
 ;

rule_list :
   rule                            { [$1] }
 | rule BAR rule_list              { $1 :: $3 }
 ;

%%
