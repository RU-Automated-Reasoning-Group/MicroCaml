type op = Add | Sub | Mult | Div | Concat | Greater | Less | GreaterEqual | LessEqual | Equal | NotEqual | Or | And

type var = string

type expr =
  | Int of int
  | Bool of bool
  | String of string
  | ID of var
  | Fun of var * expr
  | Not of expr
  | Binop of op * expr * expr
  | If of expr * expr * expr
  | FunctionCall of expr * expr
  | Let of var * bool * expr * expr
  (* | Closure of environment * var * expr *)

module CharMap = Map.Make(String)

type genericMap = int CharMap.t

type typeScheme =
    | TNum
    | TBool
    | TStr
    | T of string
    | TFun of typeScheme * typeScheme


(* annotated expr -> expr with types *)
type aexpr =
  | AInt of int * typeScheme
  | ABool of bool * typeScheme
  | AString of string * typeScheme
  | AID of var * typeScheme
  | AFun of var * aexpr * typeScheme
  | ANot of aexpr * typeScheme
  | ABinop of op * aexpr * aexpr * typeScheme
  | AIf of aexpr * aexpr * aexpr * typeScheme
  | AFunctionCall of aexpr * aexpr * typeScheme
  | ALet of var * bool * aexpr * aexpr * typeScheme
;;

(********************************************)
(***** to_string functions for debugging ****)
(********************************************)

(* operator to string *)
let string_of_op (op: op) =
  match op with
  | Add -> "+" | Sub -> "-" | Mult -> "*" | Div -> "/" | Concat -> "^"
  | Greater -> ">" | Less -> "<" | GreaterEqual -> ">=" | LessEqual -> "<=" | Equal -> "=" | NotEqual -> "<>"
  | Or -> "||" | And -> "&&"
;;

(* type to string *)
let string_of_type (t: typeScheme) =
  let rec aux (t: typeScheme) =
    match t with
    | TNum -> "int"
    | TBool -> "bool"
    | TStr -> "string"
    | T(x) -> x
    | TFun(t1, t2) -> let st1 = aux t1  in
      let st2 = aux t2 in
      (Printf.sprintf "(%s -> %s)" st1 st2) in
  let s = aux t in s
;;

(* annotated expression to string *)
let rec string_of_aexpr (ae: aexpr): string =
  match ae with
  | AInt(x, t)  -> Printf.sprintf "(%s: %s)" (string_of_int x) (string_of_type t)
  | ABool(b, t) -> Printf.sprintf "(%s: %s)" (string_of_bool b) (string_of_type t)
  | AString(s, t) -> Printf.sprintf "(%s: %s)" s (string_of_type t)
  | AID(x, t) -> Printf.sprintf "(%s: %s)" x (string_of_type t)
  | AFun(id, ae, t) ->
    let s1 = string_of_aexpr ae in
    let st = string_of_type t in
    Printf.sprintf "(fun %s -> %s): %s" id s1 st
  | ANot (e, t) ->
    let s1 = string_of_aexpr ae in
    let st = string_of_type t in
    Printf.sprintf "not (%s): %s" s1 st
  | ABinop(op, e1, e2, t) ->
    let s1 = string_of_aexpr e1 in let s2 = string_of_aexpr e2 in
    let sop = string_of_op op in let st = string_of_type t in
    Printf.sprintf "(%s %s %s: %s)" s1 sop s2 st
  | AIf (e1, e2, e3, t) ->
    let s1 = string_of_aexpr e1 in
    let s2 = string_of_aexpr e2 in
    let s3 = string_of_aexpr e3 in
    let st = string_of_type t in
    Printf.sprintf "(If %s then %s else %s): %s" s1 s2 s3 st
  | AFunctionCall(e1, e2, t) ->
    let s1 = string_of_aexpr e1 and
    s2 = string_of_aexpr e2 and st = string_of_type t in
    Printf.sprintf "(%s %s): %s" s1 s2 st
  | ALet (id, b, e1, e2, t) ->
    let s1 = string_of_aexpr e1 and
    s2 = string_of_aexpr e2 and st = string_of_type t in
    if b then
      Printf.sprintf "(let rec %s = %s in %s): %s" id s1 s2 st
    else
      Printf.sprintf "(let %s = %s in %s): %s" id s1 s2 st
;;

(* expr to string *)
let rec string_of_expr (e: expr): string =
  match e with
  | Int(x) -> string_of_int x
  | Bool(b) -> string_of_bool b
  | String(s) -> s
  | ID(s) -> s
  | Fun(id, e) ->
    let s1 = string_of_expr e in Printf.sprintf "(fun %s -> %s)" id s1
  | Not(e) ->
    let s1 = string_of_expr e in Printf.sprintf "(not %s)" s1
  | Binop(op, e1, e2) ->
    let s1 = string_of_expr e1 and s2 = string_of_expr e2 in
    let sop = string_of_op op in
    Printf.sprintf "(%s %s %s)" s1 sop s2
  | If (e1, e2, e3) ->
    let s1 = string_of_expr e1 in
    let s2 = string_of_expr e2 in
    let s3 = string_of_expr e3 in
    Printf.sprintf "(If %s then %s else %s)" s1 s2 s3
  | FunctionCall(e1, e2) ->
    let s1 = string_of_expr e1 and s2 = string_of_expr e2 in
    Printf.sprintf "(%s %s)" s1 s2
  | Let (id, b, e1, e2) ->
    let s1 = string_of_expr e1 and
    s2 = string_of_expr e2 in
    if b then
      Printf.sprintf "(let rec %s = %s in %s)" id s1 s2
    else
      Printf.sprintf "(let %s = %s in %s)" id s1 s2
;;

(********************************************)
(********* Pretty printer of types **********)
(* A type scheme (a -> b -> c) would be     *
 * printed as the OCaml type'a -> 'b -> 'c' *)
(********************************************)

let pp_string_of_type (t: typeScheme) =
  let rec aux (t: typeScheme) (chr: int) (map: genericMap) =
    match t with
    | TNum -> "int", chr, map
    | TBool -> "bool", chr, map
    | TStr -> "string", chr, map
    | T(x) ->
      let gen_chr, new_chr, new_map = if CharMap.mem x map
        then Char.escaped (Char.chr (CharMap.find x map)), chr, map
        else
          let c = Char.escaped (Char.chr chr) in
          c, (chr + 1), CharMap.add x chr map
      in
      Printf.sprintf "'%s" gen_chr, new_chr, new_map
    | TFun(t1, t2) -> let (st1, c1, m1) = aux t1 chr map in
      let (st2, c2, m2) = aux t2 c1 m1 in
      (Printf.sprintf "(%s -> %s)" st1 st2), c2, m2 in
  let s, _, _ = aux t 97 CharMap.empty in s
;;

let rec pp_string_of_aexpr (ae: aexpr): string =
  match ae with
  | AInt(x, t)  -> Printf.sprintf "(%s: %s)" (string_of_int x) (pp_string_of_type t)
  | ABool(b, t) -> Printf.sprintf "(%s: %s)" (string_of_bool b) (pp_string_of_type t)
  | AString(s, t) -> Printf.sprintf "(%s: %s)" s (pp_string_of_type t)
  | AID(x, t) -> Printf.sprintf "(%s: %s)" x (pp_string_of_type t)
  | AFun(id, ae, t) ->
    let s1 = pp_string_of_aexpr ae in
    let st = pp_string_of_type t in
    Printf.sprintf "(fun %s -> %s): %s" id s1 st
  | ANot (e, t) ->
    let s1 = pp_string_of_aexpr ae in
    let st = pp_string_of_type t in
    Printf.sprintf "not (%s): %s" s1 st
  | ABinop(op, e1, e2, t) ->
    let s1 = pp_string_of_aexpr e1 in let s2 = pp_string_of_aexpr e2 in
    let sop = string_of_op op in let st = pp_string_of_type t in
    Printf.sprintf "(%s %s %s: %s)" s1 sop s2 st
  | AIf (e1, e2, e3, t) ->
    let s1 = pp_string_of_aexpr e1 in
    let s2 = pp_string_of_aexpr e2 in
    let s3 = pp_string_of_aexpr e3 in
    let st = pp_string_of_type t in
    Printf.sprintf "(If %s then %s else %s): %s" s1 s2 s3 st
  | AFunctionCall(e1, e2, t) ->
    let s1 = pp_string_of_aexpr e1 and
    s2 = pp_string_of_aexpr e2 and st = pp_string_of_type t in
    Printf.sprintf "(%s %s): %s" s1 s2 st
  | ALet (id, b, e1, e2, t) ->
    let s1 = pp_string_of_aexpr e1 and
    s2 = pp_string_of_aexpr e2 and st = pp_string_of_type t in
    if b then
      Printf.sprintf "(let rec %s = %s in %s): %s" id s1 s2 st
    else
      Printf.sprintf "(let %s = %s in %s): %s" id s1 s2 st
;;
