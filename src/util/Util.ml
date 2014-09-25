let list_to_string f l sep =
  let l1 = List.map f l in
  String.concat sep l1;;

let get_dir s =
  let path = Str.full_split (Str.regexp "/") s in
  let foldf split s = match split with
    | Str.Text s1 -> s1 ^ s
    | Str.Delim s1 -> s1 ^ s in
  let rec list_drop_last l = match l with
    | [] -> []
    | [x] -> []
    | x :: xs -> x :: (list_drop_last xs) in
  List.fold_right foldf (list_drop_last path) "";;

let get_fname s =
  let path = Str.full_split (Str.regexp "/") s in
  match List.nth path (List.length path - 1) with
    | Str.Text s1 -> s1
    | Str.Delim _ -> assert false
