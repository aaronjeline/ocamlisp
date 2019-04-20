open List
open Lexer

type exp =
  | List of (exp list)
  | Symbol of string
  | String of string
  | Int of int
  | Bool of bool

let rec string_of_exp e = match e with
  | List e -> "(" ^ (fold_left
               (fun acc nxt -> (string_of_exp nxt) ^ " " ^ acc)
               ")"
               (rev e))
  | Symbol s -> s
  | String s -> s
  | Int i -> string_of_int i
  | Bool b -> string_of_bool b

type parse_result = exp * (token list)

exception ParseE of string

let consume t toks =
  match toks with
  | h::tl ->
    if h = t then tl else raise @@ ParseE ("Expected " ^  (Lexer.string_of_tok t))
  | _ -> raise @@
    ParseE ("Attempted to consume empty token, expected " ^ (Lexer.string_of_tok t))

let rec parse toks =
  let e,rst = read_form toks in
  match rst with
  | Tok_EOF::[] -> e
  | _ -> raise @@ ParseE "No EOF"

and read_form toks: parse_result =
  match hd toks with
  | Tok_LParen -> read_list toks
  | _ -> read_atom toks

and read_list toks: parse_result =
  let rst = consume Tok_LParen toks in
  let rec list_proc tks =
    match hd tks with
    | Tok_RParen -> (List []), tl tks
    | _ -> let form,rst = read_form tks in
      let lst,rst = list_proc rst in
      match lst with
      | List lst -> (List (form::lst)),rst
      | _ -> raise @@ ParseE "Didn't get list from list_proc()"
  in list_proc rst

and read_atom toks: parse_result =
  let atom =
    match hd toks with
    | Tok_Int i -> Int i
    | Tok_Bool b -> Bool b
    | Tok_Sym s -> Symbol s
    | Tok_String s -> String s
    | _ -> raise @@ ParseE "Parse atom given non atom" in
  atom, tl toks



