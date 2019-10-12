
type monga_type =
  | Int
  | Float
  | Bool
  | Char
  | Array of monga_type

type name = string

type monga_variable = {
  id: name;
  t: monga_type
}

