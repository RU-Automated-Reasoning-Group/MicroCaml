exception InvalidInputException of string

type token =
  | Tok_RParen
  | Tok_LParen
  | Tok_Equal
  | Tok_NotEqual
  | Tok_Greater
  | Tok_Less
  | Tok_GreaterEqual
  | Tok_LessEqual
  | Tok_Or
  | Tok_And
  | Tok_Not
  | Tok_If
  | Tok_Then
  | Tok_Else
  | Tok_Add
  | Tok_Sub
  | Tok_Mult
  | Tok_Div
  | Tok_Concat
  | Tok_Let
  | Tok_Rec
  | Tok_In
  | Tok_Def
  | Tok_Fun
  | Tok_Arrow
  | Tok_Int of int
  | Tok_Bool of bool
  | Tok_String of string
  | Tok_ID of string
  | Tok_DoubleSemi

let string_of_token token =
  match token with
  | Tok_RParen -> "("
  | Tok_LParen -> ")"
  | Tok_Equal -> "="
  | Tok_NotEqual -> "<>"
  | Tok_Greater -> ">"
  | Tok_Less -> "<"
  | Tok_GreaterEqual -> ">="
  | Tok_LessEqual -> "<="
  | Tok_Or -> "or"
  | Tok_And -> "and"
  | Tok_Not -> "not"
  | Tok_If -> "if"
  | Tok_Then -> "then"
  | Tok_Else -> "else"
  | Tok_Add -> "+"
  | Tok_Sub -> "-"
  | Tok_Mult -> "*"
  | Tok_Div -> "/"
  | Tok_Concat -> "^"
  | Tok_Let -> "let"
  | Tok_Rec -> "rec"
  | Tok_In -> "in"
  | Tok_Def -> "def"
  | Tok_Fun -> "fun"
  | Tok_Arrow -> "->"
  | Tok_Int i -> string_of_int i
  | Tok_Bool b -> string_of_bool b
  | Tok_String s -> "\"" ^ s ^ "\""
  | Tok_ID id -> id
  | Tok_DoubleSemi -> ";"

let string_of_token_list toks =
  List.fold_left (fun acc tok -> acc ^ (string_of_token tok) ^ " ") "" toks
