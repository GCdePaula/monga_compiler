open Base

(* Float numerals might lose precision *)
val string_of_tk : Parser.token -> string
val print_tk : Parser.token -> unit

(* Print untyped ast *)
val print_program : UntypedAst.untyped_tree -> unit

(* print typed ast *)
val print_typed_program : TypedAst.typed_tree -> unit

val string_of_type_error : TypedAst.error -> string
val print_type_error : TypedAst.error -> unit
val print_type_error_list : TypedAst.error list -> unit

