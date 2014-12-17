(* Author: Akshay Nanavati (ananavat@andrew.cmu.edu)
 *
 * A module for generating latex code from the Ludwig AST
 *)
let extension = ".tex"

let compile_script fname = ();;

let binop_to_string b = match b with
  | Ast.PLUS -> "$+$"
  | Ast.MINUS -> "$-$"
  | Ast.TIMES -> "$*$"
  | Ast.DIVIDEDBY -> "$/$"
  | Ast.MODULO -> "$\\%$"
  | Ast.UNION -> "\\cup"
  | Ast.INTERSECT -> "\\cap"
  ;;

let unop_to_string u = match u with
  | Ast.NEG -> "$-$"
  ;;

let rec gen_spaces n = match n with
  | 0 -> ""
  | _ -> "  " ^ gen_spaces (n - 1)
  ;;

let rec exp_remove_mark e = match e with
  | Ast.Markede e1 -> exp_remove_mark (Mark.data e1)
  | _ -> e;;

let var_to_string id = match Symbol.name id with
  | s -> s;;

let rec stms_to_str n stms =
  let spaces = gen_spaces n in
  match stms with
    | [] -> ""
    | (Ast.Assign (e1, e2)) :: s -> spaces ^
        "val " ^ exp_to_string 0 e1 ^ " = " ^
        exp_to_string 0 e2 ^ "\n" ^
        stms_to_str n s
    | (Ast.Fun (id, es, e)) :: s -> spaces ^
        "fun " ^ Symbol.name id ^ " (" ^
        Util.list_to_string (exp_to_string 0) es ", " ^ ") =\n" ^
        exp_to_string (n + 1) e ^ "\n" ^
        stms_to_str n s
    | (Ast.Markeds stm) :: s -> stms_to_str n (Mark.data stm :: s)
and exp_to_string n e =
  let spaces = gen_spaces n in
  match e with
    | Ast.Let (stms, e1) -> spaces ^
        "let\n" ^ stms_to_str (n + 1) stms ^
        spaces ^ "in\n" ^
        exp_to_string (n + 1) e1 ^ "\n" ^ spaces ^ "end"
    | Ast.Call (e1, e2) -> spaces ^
        call_to_string e1 e2
    | Ast.Nth (e1, e2) ->
        exp_to_string 0 e1 ^ "[" ^ exp_to_string 0 e2 ^ "]"
    | Ast.Const c -> spaces ^ Int32.to_string c
    | Ast.PosInf -> spaces ^ "$\\infty$"
    | Ast.NegInf -> spaces ^ "$-\\infty$"
    | Ast.Binop (b, e1, e2) -> spaces ^
        exp_to_string 0 e1 ^ " " ^ binop_to_string b ^ " " ^
        exp_to_string 0 e2
    | Ast.Unop (u, e1) -> spaces ^
        unop_to_string u ^ "(" ^ exp_to_string 0 e1 ^ ")"
    | Ast.Var (id, _) -> spaces ^ "$" ^ var_to_string id ^ "$"
    | Ast.Tuple (es) -> spaces ^
        "(" ^ Util.list_to_string (exp_to_string 0) es ", " ^ ")"
    | Ast.Case (e1, rules) ->
        let rule_to_string (e1, e2) = exp_to_string (n + 1) e1 ^ " =>\n" ^
          exp_to_string (n + 1) e2 in
        spaces ^
        "case " ^ exp_to_string 0 e1 ^ " of\n" ^
        Util.list_to_string rule_to_string rules "\n| "
    | Ast.Par (e1, e2) -> spaces ^
        "(" ^ exp_to_string 0 e1 ^ ") || (" ^ exp_to_string 0 e2 ^ ")"
    | Ast.SeqMap (e1, id, e2) -> spaces ^
        "$\\langle$" ^ exp_to_string 0 e1 ^ " : " ^ var_to_string id ^ "\\in" ^
        exp_to_string 0 e2 ^ "\\rangle"
    | Ast.SeqFilter (e1, id, e2) -> spaces ^
        "$\\langle$" ^ var_to_string id ^ "\\in" ^ exp_to_string 0 e2 ^
        "$\\mid$" ^ exp_to_string 0 e2 ^ "$\\rangle$"
    | Ast.SeqFromList es -> spaces ^
        "$\\langle$" ^ Util.list_to_string (exp_to_string 0) es ", " ^ "$\\rangle$"
    | Ast.SetFromList es -> spaces ^
        "$\\{$" ^ Util.list_to_string (exp_to_string 0) es ", " ^ "$\\}$"
    | Ast.Op b -> spaces ^ "op" ^ binop_to_string b
    | Ast.Markede e1 -> exp_to_string n (Mark.data e1)
and call_to_string e1 e2 = match (exp_remove_mark e1, exp_remove_mark e2) with
  | _ -> exp_to_string 0 e1 ^ "(" ^ exp_to_string 0 e2 ^ ")";;

let generate stms =
  let header = "\\begin{lstlisting}\n" in
  let footer = "\\end{lstlisting}\n" in
  header ^ stms_to_str 0 stms ^ footer;;
