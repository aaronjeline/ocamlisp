open Lib.Lexer
open Lib.Parser
open Lib.Highlevel
open Lib.Codegen
open Printf
open Llvm

type mode = Lex | Parse | Compile

let print_all lst f =
  List.iter
    (fun e -> printf " %s" @@ f e)
    lst;
  printf "\n"

let () =
  let m = Compile in
  let d = ref true in
  while !d do
    try
      let s = read_line() in
      let s' = sprintf "(%s)" s in
      match m with
      | Lex -> print_all (tokenize s') string_of_tok
      | Parse -> printf "%s\n" @@ string_of_exp (parse (tokenize s'))
      | Compile ->
        let es = match parse (tokenize s') with
          | List es -> es
          | _ -> failwith "parse error" in
        let fs = get_funcs es in
        let exs = get_externs es in
        List.iter
          (fun (Extern p) ->
             dump_value (codegen_proto p))
          exs;
        List.iter
          (fun f -> dump_value (codegen_func f))
          fs;

    with
    | End_of_file -> d := false
    | ParseE s -> printf "Error in parsing: %s\n" s
    | TokenizeE s -> printf "Error in lexing: %s\n" s


  done;
  print_endline ""

