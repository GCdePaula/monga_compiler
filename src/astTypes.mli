
type monga_type =
  | Int
  | Float
  | Bool
  | Char
  | Array of monga_type

type id = string

type monga_variable = {
  name: id;
  t: monga_type
}

type monga_function_type = {
  parameters: monga_variable list;
  ret_type: monga_type option;
}

type location = Lexing.position * Lexing.position
