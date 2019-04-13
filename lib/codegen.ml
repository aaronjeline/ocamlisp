open Highlevel
open Parser
open Llvm

exception Error of string

let context = global_context ()
let the_module = create_module context "OCAMLISP Compiler"
let builder = builder context
let named_values:(string, llvalue) Hashtbl.t = Hashtbl.create 10
let double_type = double_type context
let int_type = i32_type context

let builtins = ["+";"-";"*";"<"]

let rec codegen_expr expr =
  match expr with
  | Int i -> const_int int_type i
  | Symbol s ->
    (match Hashtbl.find_opt named_values s with
    | Some r -> r
    | None -> raise @@ Error "Unknown variable")
  | Bool _ -> raise @@ Error "Unimplemented"
      (* SPECIAL FORMS BEGIN *)
  | List (Symbol op::rst) when List.mem op builtins ->
    begin
      match rst with
      | lhs::rhs::[] ->
        let lhs_val = codegen_expr lhs in
        let rhs_val = codegen_expr rhs in
        begin
          match op with
          | "+" -> build_add lhs_val rhs_val "addtmp" builder
          | "-" -> build_sub lhs_val rhs_val "addtmp" builder
          | "*" -> build_mul lhs_val rhs_val "addtmp" builder
          | _ -> failwith "Not builtin!"
        end
      | _ -> failwith "Bad operation"
    end
    (* SPECIAL FORMS https://github.com/syl20bnr/spacemacs/pull/11985END*)
  | List (Symbol s::args) -> (* Function call *)
    let callee =
      match lookup_function s the_module with
      | Some callee -> callee
      | None -> raise @@ Error "unkown function"
    in
    let params = params callee in
    if List.length args <> Array.length params then raise @@ Error "arglength"
    else let args = Array.of_list @@ List.map codegen_expr args in
      build_call callee args "calltmp" builder
  | List _ -> raise @@ Error "bad call"
  | String _ -> raise @@ Error "Unsupported"


let codegen_proto p = match p with
  | Proto (name, args) ->
    let args = Array.of_list args in
    let ints = Array.make (Array.length args) int_type in
    let ft = function_type int_type ints in
    let f =
      match lookup_function name the_module with
      | None -> declare_function name ft the_module
      | Some f ->
        if Array.length (basic_blocks f) == 0 then () else
          raise @@ Error "Redefinition of function";
        if Array.length (params f) == Array.length args then () else
          raise @@ Error "redef of function w/ args";
        f
    in
    Array.iteri (fun i a ->
        let n = args.(i) in
        set_value_name n a;
        Hashtbl.add named_values n a;)
      (params f);
    f


let codegen_func f = match f with
  | Func (proto,body) ->
    Hashtbl.clear named_values;
    let the_function = codegen_proto proto in
    let bb = append_block context "entry" the_function in
    position_at_end bb builder;

    try
      let ret_val = codegen_expr body in
      let _ = build_ret ret_val builder in
      Llvm_analysis.assert_valid_function the_function;
      the_function
    with e -> delete_function the_function; raise e
