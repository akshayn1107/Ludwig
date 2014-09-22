{
module A = Ast
module S = Symbol
module T = LudwigParser

let start = Lexing.lexeme_start
let l_end = Lexing.lexeme_end
let text = Lexing.lexeme

let commentLevel = ref 0
let commentPos = ref 0

let enterComment lexbuf =
  commentLevel := !commentLevel + 1 ;
  commentPos := start lexbuf

let exitComment () =
  commentLevel := !commentLevel - 1 ;
  !commentLevel = 0

let number s lexbuf =
  try
    T.INTCONST (Int32.of_string s)
  with Failure _ ->
    ErrorMsg.error (ParseState.ext (start lexbuf, l_end lexbuf))
      ("cannot parse integral constant `" ^ text lexbuf ^ "'");
    T.INTCONST Int32.zero

let eof () =
  (if !commentLevel > 0 then
    ErrorMsg.error (ParseState.ext (!commentPos, !commentPos))
      "unterminated comment");
  T.EOF

}

let id = ['A'-'Z' 'a'-'z' '_']['A'-'Z' 'a'-'z' '0'-'9' '_']*
let decnum = ['0'-'9']+
let ws = [' ' '\t']

rule initial =
  parse
    ws+         { initial lexbuf }
  | '\n'        { ParseState.newline (start lexbuf); initial lexbuf }

  | '{'         { T.LBRACE }
  | '}'         { T.RBRACE }
  | '('         { T.LPAREN }
  | ')'         { T.RPAREN }

  | '~'         { T.TILDA }

  | '='         { T.ASSIGN }

  | '+'         { T.PLUS }
  | '-'         { T.MINUS }
  | '*'         { T.STAR }
  | '/'         { T.SLASH }
  | '%'         { T.PERCENT }
  | "<-"        { T.LARROW }
  | '|'         { T.BAR }
  | "||"        { T.DBLBAR }

  | ','         { T.COMMA }

  | "..."       { T.ELIPSES }
  | ".:"        { T.CONSMAP }
  | ".|"        { T.CONSFILTER }

  | "=>"        { T.MATCHARROW }

  | ".<"        { T.SEQCONSL }
  | ">."        { T.SEQCONSR }

  | "case"      { T.CASE }
  | "of"        { T.OF }
  | "let"       { T.LET }
  | "val"       { T.VAL }
  | "in"        { T.IN }
  | "end"       { T.END }
  | "fun"       { T.FUN }

  | "-\\inf"    { T.NEGINF }
  | "\\inf"     { T.POSINF }
  | decnum as n { number n lexbuf }

  | id as name  { let id = Symbol.symbol name in T.IDENT id }

  | "(*"        { enterComment lexbuf; comment lexbuf }
  | "*)"        { ErrorMsg.error (ParseState.ext (start lexbuf, start lexbuf))
                    "unbalanced comments"; initial lexbuf }

  | eof         { eof () }
  | _           { ErrorMsg.error (ParseState.ext (start lexbuf, start lexbuf))
                    ("illegal character: \"" ^ text lexbuf ^ "\"");
                  initial lexbuf }

and comment =
  parse
    "(*"       { enterComment lexbuf; comment lexbuf }
  | "*)"       { (if exitComment () then initial else comment) lexbuf }
  | '\n'       { ParseState.newline (start lexbuf); comment lexbuf }
  | _          { comment lexbuf }

and comment_line =
  parse
    '\n'       { ParseState.newline (start lexbuf); initial lexbuf }
  | _          { comment_line lexbuf }

{}
