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
%token LBRACE RBRACE
%token LPAREN RPAREN
%token TILDA
%token ASSIGN
%token PLUS MINUS
%token STAR SLASH
%token PERCENT LARROW
%token BAR DBLBAR
%token COMMA
%token ELIPSES CONSMAP CONSFILTER
%token MATCHARROW
%token SEQCONSL SEQCONSR
%token CASE OF LET VAL
%token IN END FUN
%token NEGINF POSINF

%type <Ast.stm list> program

%left DBLBAR
%left PLUS MINUS
%left STAR SLASH PERCENT
%right UNARY
%left LPAREN
%left BAR

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
 | CASE exp OF rule_list END       { marke (A.Case ($2, $4)) (1, 5) }
 | POSINF                          { marke (A.PosInf) (1, 1) }
 | NEGINF                          { marke (A.NegInf) (1, 1) }
 | exp DBLBAR exp                  { marke (A.Par ($1, $3)) (1, 3) }
 | LET stm_list IN exp END         { marke (A.Let ($2, $4)) (1, 5) }
 | tuple                           { $1 }
 | exp tuple                       { marke (A.Call ($1, $2)) (1, 2) }
 | SEQCONSL exp_list SEQCONSR         { marke (A.FromList $2) (1, 3) }
 ;

tuple :
   LPAREN exp_list RPAREN          { A.Tuple $2 }
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
