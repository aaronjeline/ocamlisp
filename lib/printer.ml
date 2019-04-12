open Parser

let rec string_of_exp exp =
  match exp with
  | String s -> "\"" ^ s ^ "\""
  | Symbol s -> s
  | Bool b -> if b then "true" else "false"
  | Int i -> string_of_int i
  | List lst ->
    let contents =
      List.fold_right
        (fun e str -> (string_of_exp e) ^ " " ^ str) lst "" in
    let contents' =
      if contents = "" then contents else
      String.sub contents 0 ((String.length contents) - 1) in
    "(" ^ contents' ^ ")"


