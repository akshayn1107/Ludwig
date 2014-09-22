(* parse filename = ast
 * will raise ErrorMsg.Error in case of lexing or parsing error
 *)

let parse filename =
  try
    SafeIO.withOpenIn filename
      (fun chan ->
        let lexbuf = Lexing.from_channel chan in
        let _ = ErrorMsg.reset ()
        and _ = ParseState.setfile filename in
        let ast = LudwigParser.program LudwigLexer.initial lexbuf in
        let _ = if !ErrorMsg.anyErrors then raise ErrorMsg.Error else () in
        ast)
  with Parsing.Parse_error ->
    ErrorMsg.error None ("parse error"); raise ErrorMsg.Error
  | Sys_error s -> ErrorMsg.error None s; raise ErrorMsg.Error
