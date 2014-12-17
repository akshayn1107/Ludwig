(* Author: Akshay Nanavati (ananavat@andrew.cmu.edu)
 *
 * A module for generating SML code from the Ludwig AST
 *)

let extension = ".sml"

let compile_script fname =
  let cm_fname = Util.get_fname fname ^ ".cm" in
  let dir = Util.get_dir fname in
  let cm_fname_with_dir = fname ^ ".cm" in
  let cm_content =
    "Group is\n" ^
    "  Lib210/210lib.cm\n" ^
    "  " ^ Util.get_fname fname ^ extension in
  let makefile_content =
    "all:\n" ^
    "\tsmlnj-m " ^ cm_fname ^ "\n" in
  let _ = SafeIO.withOpenOut cm_fname_with_dir
    (fun af -> output_string af cm_content) in
  let _ = SafeIO.withOpenOut (dir ^ "Makefile")
    (fun af -> output_string af makefile_content) in ();;

let binop_to_string b = match b with
  | Ast.PLUS -> "+"
  | Ast.MINUS -> "-"
  | Ast.TIMES -> "*"
  | Ast.DIVIDEDBY -> "/"
  | Ast.MODULO -> "mod"
  | Ast.UNION -> assert false
  | Ast.INTERSECT -> assert false
  ;;

let unop_to_string u = match u with
  | Ast.NEG -> "~"

let rec gen_spaces n = match n with
  | 0 -> ""
  | _ -> "  " ^ gen_spaces (n - 1);;

let rec exp_remove_mark e = match e with
  | Ast.Markede e1 -> exp_remove_mark (Mark.data e1)
  | _ -> e;;

let var_to_string id = match Symbol.name id with
  | "EMPTY" -> "Seq.EMPTY"
  | "NODE" -> "Seq.NODE"
  | "ELT" -> "Seq.ELT"
  | "merge" -> "Seq.merge"
  | "showt" -> "Seq.showt"
  | "reduce" -> "Seq.reduce"
  | "max" -> assert false
  | s -> s;;

let rec stms_to_str n stms =
  let spaces = gen_spaces n in
  match stms with
    | [] -> ""
    | (Ast.Assign (e1, e2)) :: s -> spaces ^
        "val " ^ exp_to_string 0 e1 ^ " =\n" ^
        exp_to_string (n + 1) e2 ^ "\n" ^
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
    | Ast.Nth (e1, e2) -> spaces ^
        "Seq.nth (" ^ exp_to_string 0 e1 ^ ") (" ^ exp_to_string 0 e2 ^ ")"
    | Ast.Const c -> spaces ^ Int32.to_string c
    | Ast.PosInf -> spaces ^ "valOf (Int.maxInt) (* PosInf *)"
    | Ast.NegInf -> spaces ^ "valOf (Int.minInt) (* NegInf *)"
    | Ast.Binop (Ast.INTERSECT, e1, e2) -> spaces ^
        "Set.intersection (" ^ exp_to_string 0 e1 ^ ") (" ^
        exp_to_string 0 e2 ^ ")"
    | Ast.Binop (Ast.UNION, e1, e2) -> spaces ^
        "Set.union (" ^ exp_to_string 0 e1 ^ ") (" ^
        exp_to_string 0 e2 ^ ")"
    | Ast.Binop (b, e1, e2) -> spaces ^
        exp_to_string 0 e1 ^ " " ^ binop_to_string b ^ " " ^
        exp_to_string 0 e2
    | Ast.Unop (u, e1) -> spaces ^
        unop_to_string u ^ "(" ^ exp_to_string 0 e1 ^ ")"
    | Ast.Var (id, _) -> spaces ^ var_to_string id
    | Ast.Tuple (es) -> spaces ^
        "(" ^ Util.list_to_string (exp_to_string 0) es ", " ^ ")"
    | Ast.Case (e1, rules) ->
        let rule_to_string (e1, e2) = exp_to_string (n + 1) e1 ^ " => " ^
          exp_to_string 0 e2 in
        spaces ^
        "case " ^ exp_to_string 0 e1 ^ " of\n" ^
        Util.list_to_string rule_to_string rules "\n| "
    | Ast.Par (e1, e2) -> spaces ^
        "Primitives.par (fn () => " ^ exp_to_string 0 e1 ^ ", fn () => " ^
        exp_to_string 0 e2 ^ ")"
    | Ast.SeqFromList es -> spaces ^
        "Seq.fromList [" ^ Util.list_to_string (exp_to_string 0) es ", " ^ "]"
    | Ast.SeqMap (e1, id, e2) -> spaces ^
        "Seq.map (fn " ^ var_to_string id ^ " => " ^ exp_to_string 0 e1 ^ ") (" ^
        exp_to_string 0 e2 ^ ")"
    | Ast.SeqFilter (e1, id, e2) -> spaces ^
        "Seq.filter (fn " ^ var_to_string id ^ " => " ^ exp_to_string 0 e1 ^ ") (" ^
        exp_to_string 0 e2 ^ ")"
    | Ast.SetFromList es -> spaces ^
        "Set.fromList [" ^ Util.list_to_string (exp_to_string 0) es ", " ^ "]"
    | Ast.Op b -> begin match b with
        | Ast.UNION -> spaces ^ "Set.Union"
        | Ast.INTERSECT -> spaces ^ "Set.Union"
        | _ -> spaces ^ "op" ^ binop_to_string b end
    | Ast.Markede e1 -> exp_to_string n (Mark.data e1)
and call_to_string e1 e2 = match (exp_remove_mark e1, exp_remove_mark e2) with
  | (Ast.Var (id, _), Ast.Tuple es) -> begin match Symbol.name id with
    | "max" ->
        "List.foldl Int.max (valOf (Int.minInt)) [" ^
        Util.list_to_string (exp_to_string 0) es ", " ^ "]"
    | "min" ->
        "List.foldl Int.min (valOf (Int.maxInt)) [" ^
        Util.list_to_string (exp_to_string 0) es ", " ^ "]"
    | _ -> var_to_string id ^ (exp_to_string 0 (Ast.Tuple es))
    end
  | _ -> exp_to_string 0 e1 ^ "(" ^ exp_to_string 0 e2 ^ ")";;

let generate stms =
  let header = "structure Ludwig =\nstruct\nstructure Seq = ArraySequence\n" in
  let footer = "\nend\n" in
  header ^ stms_to_str 0 stms ^ footer;;
