open List
open Parser


type proto = Proto of string * (string list)
type extern = Extern of proto
type func = Func of proto * exp

let proto_name p = match p with
  | Proto (name,_) -> name

let string_of_func f = match f with
  | Func (p,_) -> proto_name p

let string_of_extern e = match e with
  | Extern p -> proto_name p

let all lst f=
  fold_left
    (fun acc n -> (f n) && acc)
    true
    lst

let extract_symbols syms =
  let comb acc next =
    match acc,next with
    | None,_ -> None
    | Some strs, Symbol s -> Some (s::strs)
    | Some _, _ -> None in
  fold_left comb (Some []) syms

let proto lst =
  match lst with
  | List ((Symbol name)::rst) ->
    (match extract_symbols rst with
    | Some args -> Some (Proto (name,args))
    | None -> None)
  | _ -> None

let extern lst =
  match lst with
  | List (Symbol "extern"::List p::[]) ->
    (match proto (List p) with
     | Some p -> Some (Extern p)
     | None -> None)
  | _ -> None

let func lst =
  match lst with
  | List (Symbol "func"::List p::body::[]) ->
    (match proto (List p) with
     | Some p -> Some (Func (p,body))
     | None -> None)
  | _ -> None

let get_funcs exps =
  let comb acc nxt =
    match func nxt with
    | Some f -> f::acc
    | None -> acc in
  rev @@ fold_left comb [] exps

let get_externs exps =
  let comb acc nxt =
    match extern nxt with
    | Some f -> f::acc
    | None -> acc in
  rev @@ fold_left comb [] exps
