let list_to_string f l sep =
  let l1 = List.map f l in
  String.concat sep l1;;

let get_dir s =
  let path = Str.full_split (Str.regexp "/") s in
  let foldf split s = match split with
    | Str.Text s1 -> s1 ^ s
    | Str.Delim s1 -> s1 ^ s in
  List.fold_right foldf path "";;

let get_fname s =
  let path = Str.full_split (Str.regexp "/") s in
  match List.nth path (List.length path - 1) with
    | Str.Text s1 -> s1
    | Str.Delim _ -> assert false
