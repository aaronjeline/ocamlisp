open Lib.Lexer
open Lib.Parser
open Lib.Highlevel
open Lib.Codegen
open Printf
open Llvm

type mode = Lex | Parse | Compile

let () =
  let m = Compile in
  let d = ref true in
  while !d do
    try
      let s = read_line() in
      let s' = sprintf "(%s)" s in
      match m with
      | Lex -> List.iter
                 (fun t -> printf " %s" @@ string_of_tok t)
                 (tokenize s'); printf "\n"
      | Parse -> failwith "Unimplemented"
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

