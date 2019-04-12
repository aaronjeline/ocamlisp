open Str
open String
open Printf

exception TokenizeE of string

type token =
  | Tok_LParen
  | Tok_RParen
  | Tok_Int of int
  | Tok_Sym of string
  | Tok_String of string
  | Tok_Bool of bool
  | Tok_EOF

let string_of_tok t =
  match t with
  | Tok_LParen -> "Tok_LParen"
  | Tok_RParen -> "Tok_RParen"
  | Tok_Int i -> "Int: " ^ (string_of_int i)
  | Tok_Sym s -> "Symbol: " ^ s
  | Tok_String s -> "String : \"" ^ s ^ "\""
  | Tok_Bool b -> "Boolean: " ^ (if b then "true" else "false")
  | Tok_EOF -> "EOF"

let print_toklist toks =
  printf "%s" "[";
  List.iter
    (fun t -> printf "%s, " @@ string_of_tok t) toks;
  printf "%s" "]"


let reg_whitespace = regexp "[ \n\t]+"
let reg_symbol = regexp "[a-zA-Z\\+\\?!@#$%\\^/&\\*=][a-zA-Z0-9+\\?!@/#$%\\^&\\*=]*"
let reg_string = regexp "\".*\""
let reg_num = regexp "-?[0-9]*"
let reg_bool = regexp "true|false"

let bool_len b =
  length @@ if b then "true" else "false"

let strip_quotes s =
  let len = length s in
  sub s 1 (len - 2)

let tokenize str =
  let len = length str in
  let rec lex pos: token list =
    if pos >= len then
      [Tok_EOF]
    else if string_match reg_whitespace str pos then
      lex (pos + 1)
    else if get str pos = '(' then
      Tok_LParen::(lex (pos + 1))
    else if get str pos = ')' then
      Tok_RParen::(lex (pos + 1))
    else if string_match reg_symbol str pos then
      let s = matched_string str in
      (Tok_Sym s) :: (lex @@ pos + (length s))
    else if string_match reg_string str pos then
      let s = matched_string str in
      (Tok_String (strip_quotes s)) :: (lex @@ pos + (length s))
    else if string_match reg_bool str pos then
      let b = if matched_string str = "true" then true else false in
      (Tok_Bool b) :: (lex @@ pos + (bool_len b))
    else if string_match reg_num str pos then
      let s = matched_string str in
      try
        let i = int_of_string s in
        (Tok_Int i)::(lex @@ pos + (length s))
      with _ -> raise @@ TokenizeE (Printf.sprintf "Couldn't convert %s to int" s)
    else raise @@ TokenizeE "Nonvalid token" in
  lex 0
