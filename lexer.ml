open TokenTypes
open String

(*type token =
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
  | Tok_DoubleSemi*)


(* We provide the regular expressions that may be useful to your code *)

let re_rparen = Str.regexp ")";;
let re_lparen = Str.regexp "(";;
let re_equal = Str.regexp "=";;
let re_not_equal = Str.regexp "<>";;
let re_greater = Str.regexp ">";;
let re_less = Str.regexp "<";;
let re_greater_equal = Str.regexp ">=";;
let re_less_equal = Str.regexp "<=";;
let re_or = Str.regexp "||";;
let re_and = Str.regexp "&&";;
let re_not = Str.regexp "not";;
let re_if = Str.regexp "if";;
let re_then = Str.regexp "then";;
let re_else = Str.regexp "else";;
let re_add = Str.regexp "+";;
let re_sub = Str.regexp "-";;
let re_mult = Str.regexp "*";;
let re_div = Str.regexp "/"
let re_concat = Str.regexp "\\^";;
let re_let = Str.regexp "let";;
let re_rec = Str.regexp "rec";;
let re_in = Str.regexp "in";;
let re_def = Str.regexp "def";;
let re_fun = Str.regexp "fun";;
let re_arrow = Str.regexp "->";;
let re_pos_int = Str.regexp "[0-9]+";;
let re_neg_int = Str.regexp "(-[0-9]+)";;
let re_true = Str.regexp "true";;
let re_false = Str.regexp "false";;
let re_string = Str.regexp "\"[^\"]*\"";;
let re_id = Str.regexp "[a-zA-Z][a-zA-Z0-9]*";;
let re_double_semi = Str.regexp ";;";;
let re_whitespace = Str.regexp "[ \t\n]+";;

(* Part 1: Lexer - IMPLEMENT YOUR CODE BELOW *)

let tokenize input = raise (InvalidInputException "")
