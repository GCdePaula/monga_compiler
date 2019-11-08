(* open Printf *)
open Src
open Llvm
open Base
open Stdio

(*
let gen_hello () =
(*
  let context = global_context () in
  let the_module = create_module context "my_mod" in
  let builder = builder context in

  let void_type = void_type context in
  let i32_type = i32_type context in

  let format_str = define_global "" (const_stringz context "%s\n") the_module in
  let hello_string = define_global "" (const_stringz context "Hello, World!") the_module in

  let i8_ptr_type = pointer_type (i8_type context) in
  let printf_type = var_arg_function_type i32_type [|i8_ptr_type|] in
  let printf = declare_function "printf" printf_type the_module in

  let ft = function_type void_type [||] in
  let f = declare_function "main" ft the_module in
  let bb = append_block context "entry" f in
  let _ = position_at_end bb builder in

  let s2 = build_global_stringptr "bananas" "" builder in

  let _ = build_call printf [| s2 |] "" builder in

  let _ = build_ret_void builder in
*)
  (* let the_execution_engine = Llvm_executionengine.create the_module in *)
  (* let _ = ExecutionEngine.run_function f [||] the_execution_engine in *)

  let llctx = global_context () in
  let llm = create_module llctx "mymodule" in

  let i8_t = i8_type llctx in
  let i32_t = i32_type llctx in
  let fty = function_type i32_t [| |] in

  let f = define_function "main" fty llm in
  let llbuilder = builder_at_end llctx (entry_block f) in

  let printf_ty = var_arg_function_type i32_t [| pointer_type i8_t |] in
  let printf = declare_function "printf" printf_ty llm in

  let s = build_global_stringptr "Hello, world!\n" "" llbuilder in

  (* try commenting these two lines and compare the result *)
  let zero = const_int i32_t 0 in
  let s = build_in_bounds_gep s [| zero |] "" llbuilder in

  let _ = build_call printf [| s |] "" llbuilder in
  let _ = build_ret (const_int i32_t 0) llbuilder in

  llm

*)

let build_typed_tree file_name =
  let cin = open_in file_name in
  let lexbuf = Lexing.from_channel cin in

  let untyped_ast = Parser.program Lexer.monga_lexer lexbuf in
  TypeCheck.build_typed_tree untyped_ast


let build_llvm_module file_name =
  match build_typed_tree file_name with
  | Ok typed_tree ->
    CodeGen.gen_code typed_tree
  | Error _ ->
    raise (Failure "Failed to build typed tree")

let test file_name =
  printf "BUILDING FILE %s\n" file_name;

  let llm = build_llvm_module file_name in
  print_string (string_of_llmodule llm);

  let open Ctypes in
  let open Foreign in

  try
    let exe_engine = Llvm_executionengine.create llm in
    let c_t = funptr (void @-> returning int32_t) in
    let main_func = Llvm_executionengine.get_function_address "main" c_t exe_engine in
    let _ = main_func () in
    ()
  with _ -> print_string "ERROR EXECUTING CODE\n";

  printf "DONE %s\n\n" file_name


let main () =
  let _ = Llvm_executionengine.initialize () in
  test "inputs/test1.in"

let _ = Backtrace.Exn.with_recording true ~f:main
