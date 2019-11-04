
open Llvm
open Llvm_executionengine

let main () =
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
  print_string (string_of_llmodule llm);
  let _ = initialize () in
  let the_execution_engine = Llvm_executionengine.create llm in
  let open Ctypes in
  let open Foreign in
  let c_t = funptr (void @-> returning int32_t) in
  let mf = get_function_address "main" c_t the_execution_engine in
  let _ = mf () in
  ()
  (* let _ = ExecutionEngine.run_function f [||] the_execution_engine in *)


let _ = Printexc.print main ()
