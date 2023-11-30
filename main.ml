open Lexer
open Parser
open Infer
open MicroCamlTypes

(* Simple Expression Tests: *)
let public_expr_simple_equal _ =
  let result = ([], Binop (Equal, (Int 1), (Int 1))) in
  let student = "1 = 1" |> tokenize |> parse_expr in
  assert (student = result)

let public_expr_simple_equal_type _ =
  (* 1 = 1  *)
  let prog = (Binop (Equal, (Int 1), (Int 1))) in
  let result = TBool in
  let student = infer prog in
  assert (student = result)

let public_expr_simple_concat _ =
  let result = ([], Binop (Concat, (String "Hello"), (String " World!"))) in
  let student = "\"Hello\" ^ \" World!\"" |> tokenize |> parse_expr in
  assert (student = result)

let public_expr_simple_concat_type _ =
  (* "\"Hello\" ^ \" World!\"" *)
  let prog = (Binop (Concat, (String "Hello"), (String " World!"))) in
  let result = TStr in
  let student = infer prog in
  assert (student = result)

let public_expr_simple_div _ =
  let result =  ([], Binop (Div, (Int 15), (Int 3))) in
  let student = "15 / 3" |> tokenize |> parse_expr in
  assert (student = result)

let public_expr_simple_div_type _ =
  (* "15 / 3" *)
  let prog =  (Binop (Div, (Int 15), (Int 3))) in
  let result = TNum in
  let student = infer prog in
  assert (student = result)

let public_expr_simple_mult _ =
  let result = ([], Binop (Mult, (Int 5), (Int 3))) in
  let student = "5 * 3" |> tokenize |> parse_expr in
  assert (student = result)

let public_expr_simple_mult_type _ =
  (* "5 * 3" *)
  let prog = (Binop (Mult, (Int 5), (Int 3))) in
  let result = TNum in
  let student = infer prog in
  assert (student = result)

let public_expr_simple_sub _ =
  let result = ([], Binop (Sub, (Int 3), (Int 2))) in
  let student = "3 - 2" |> tokenize |> parse_expr in
  assert (student = result)

let public_expr_simple_sub_type _ =
  (* "3 - 2" *)
  let prog = (Binop (Sub, (Int 3), (Int 2))) in
  let result = TNum in
  let student = infer prog in
  assert (student = result)

let public_expr_simple_sum _ =
  let result = ([], Binop (Add, (Int 1), (Int 2))) in
  let student = "1 + 2" |> tokenize |> parse_expr in
  assert (student = result)

let public_expr_simple_sum_type _ =
  (* "1 + 2" *)
  let prog = (Binop (Add, (Int 1), (Int 2))) in
  let result = TNum in
  let student = infer prog in
  assert (student = result)

let public_expr_single_and _ =
  let result = ([], Binop (And, (Bool true), (Bool true))) in
  let student = "true && true" |> tokenize |> parse_expr in
  assert (student = result)

let public_expr_single_and_type _ =
  (* "true && true" *)
  let prog = (Binop (And, (Bool true), (Bool true))) in
  let result = TBool in
  let student = infer prog in
  assert (student = result)

let public_expr_single_bool _ =
  let result = ([], (Bool false)) in
  let student = "false" |> tokenize |> parse_expr in
  assert (student = result)

let public_expr_single_bool_type _ =
  (* "false" *)
  let prog = ((Bool false)) in
  let result = TBool in
  let student = infer prog in
  assert (student = result)

let public_expr_single_fun _ =
  let result = ([], Fun ("x", Binop (Add, ID "x", (Int 1)))) in
  let student = "fun x -> x + 1" |> tokenize |> parse_expr in
  assert (student = result)

let public_expr_single_fun_type _ =
  (* fun x -> x + 1 *)
  let prog = (Fun ("x", Binop (Add, ID "x", (Int 1)))) in
  let result = TFun(TNum, TNum) in
  let student = infer prog in
  assert (student = result)

let public_expr_single_if _ =
  let result = ([], If (Binop (Equal, (Int 1), (Int 2)), (Bool false),
   (Bool true))) in
  let student = "if 1 = 2 then false else true" |> tokenize |> parse_expr in
  assert (student = result)

let public_expr_single_if_type _ =
  (* if 1 = 2 then false else true *)
  let prog = (If (Binop (Equal, (Int 1), (Int 2)), (Bool false),
   (Bool true))) in
  let result = TBool in
  let student = infer prog in
  assert (student = result)

let public_expr_single_let _ =
  let result = ([], Let ("x", false, (Int 42), ID "x")) in
  let student = "let x = 42 in x" |> tokenize |> parse_expr in
  assert (student = result)

let public_expr_single_let_type _ =
  (* let x = 42 in x *)
  let prog = (Let ("x", false, (Int 42), ID "x")) in
  let result = TNum in
  let student = infer prog in
  assert (student = result)

let public_expr_single_notequal _ =
  let result = ([], Binop (NotEqual, (Int 1), (Int 2))) in
  let student = "1 <> 2" |> tokenize |> parse_expr in
  assert (student = result)

let public_expr_single_notequal_type _ =
  (* "1 <> 2" *)
  let prog = (Binop (NotEqual, (Int 1), (Int 2))) in
  let result = TBool in
  let student = infer prog in
  assert (student = result)

let public_expr_single_not _ =
  let result = ([], Not ((Bool true))) in
  let student = "not true" |> tokenize |> parse_expr in
  assert (student = result)

let public_expr_single_not_type _ =
  (* "not true" *)
  let prog = (Not ((Bool true))) in
  let result = TBool in
  let student = infer prog in
  assert (student = result)

let public_expr_single_number _ =
  let result = ([], (Int 42)) in
  let student = "42" |> tokenize |> parse_expr in
  assert (student = result)

let public_expr_single_number_type _ =
  (* "42" *)
  let prog = ((Int 42)) in
  let result = TNum in
  let student = infer prog in
  assert (student = result)

let public_expr_single_or _ =
  let result = ([], Binop (Or, (Bool true), (Bool false))) in
  let student = "true || false" |> tokenize |> parse_expr in
  assert (student = result)

let public_expr_single_or_type _ =
  (* "true || false" *)
  let prog = (Binop (Or, (Bool true), (Bool false))) in
  let result = TBool in
  let student = infer prog in
  assert (student = result)

let public_expr_single_string _ =
  let result = ([], (String "Hello World!")) in
  let student = "\"Hello World!\"" |> tokenize |> parse_expr in
  assert (student = result)

let public_expr_single_string_type _ =
  (* "\"Hello World!\""  *)
  let prog = ((String "Hello World!")) in
  let result = TStr in
  let student = infer prog in
  assert (student = result)

(* Complex Expressions *)
let public_expr_add1 _ =
  let result = ([],
    Let ("add1", false, Fun ("x", Binop (Add, ID "x", (Int 1))), ID "add1")) in
  let student = "let add1 = fun x -> x + 1 in add1" |> tokenize |> parse_expr in
  assert (student = result)

let public_expr_add1_type _ =
  (* let add1 = fun x -> x + 1 in add1 *)
  let prog = (Let ("add1", false, Fun ("x", Binop (Add, ID "x", (Int 1))), ID "add1")) in
  let result = TFun (TNum, TNum) in
  let student = infer prog in
  assert (student = result)

let public_expr_apply _ =
  let result = ([], Let ("apply", false, Fun ("x", Fun ("y", FunctionCall (ID "x", ID "y"))),
   Let ("add1", false, Fun ("z", Binop (Add, ID "z", (Int 1))),
    FunctionCall (FunctionCall (ID "apply", ID "add1"), (Int 5))))) in
  let student = "let apply = fun x -> fun y -> x y in let add1 = fun z -> z + 1 in (apply add1) 5" |> tokenize |> parse_expr in
  assert (student = result)

let public_expr_apply_type _ =
  (* let apply = fun x -> fun y -> x y in let add1 = fun z -> z + 1 in (apply add1) 5 *)
  let prog = (Let ("apply", false, Fun ("x", Fun ("y", FunctionCall (ID "x", ID "y"))),
   Let ("add1", false, Fun ("z", Binop (Add, ID "z", (Int 1))),
    FunctionCall (FunctionCall (ID "apply", ID "add1"), (Int 5))))) in
  let result = TNum in
  let student = infer prog in
  assert (student = result)

let public_expr_double_fun _ =
  let result = ([], Fun ("x", Fun ("y", Binop (Add, ID "x", ID "y")))) in
  let student = "fun x -> fun y -> x + y" |> tokenize |> parse_expr in
  assert (student = result)

let public_expr_double_fun_type _ =
  (* fun x -> fun y -> x + y *)
  let prog = (Fun ("x", Fun ("y", Binop (Add, ID "x", ID "y")))) in
  let result = TFun(TNum, TFun(TNum, TNum)) in
  let student = infer prog in
  assert (student = result)

let public_expr_let_if _ =
  let result = ([],
  Let ("sanity", false,
   If (Binop (Equal, (Int 1), (Int 1)), (Bool true),
    (Bool false)),
   ID "sanity")) in
  let student = "let sanity = if 1 = 1 then true else false in sanity" |> tokenize |> parse_expr in
  assert (student = result)

let public_expr_let_if_type _ =
  (* let sanity = if 1 = 1 then true else false in sanity *)
  let prog = (Let ("sanity", false,
   If (Binop (Equal, (Int 1), (Int 1)), (Bool true),
    (Bool false)),
   ID "sanity")) in
  let result = TBool in
  let student = infer prog in
  assert (student = result)

let public_expr_let_fun _ =
  let result = ([], Let ("abc", false, Fun ("a", Binop (Add, ID "a", (Int 1))),
    FunctionCall (ID "abc", (Int 1)))) in
  let student = "let abc = fun a -> a + 1 in abc 1" |> tokenize |> parse_expr in
  assert (student = result)

let public_expr_let_fun_type _ =
  (* let abc = fun a -> a + 1 in abc 1 *)
  let prog = (Let ("abc", false, Fun ("a", Binop (Add, ID "a", (Int 1))),
    FunctionCall (ID "abc", (Int 1)))) in
  let result = TNum in
  let student = infer prog in
  assert (student = result)

let public_expr_minus_one _ =
  let result = ([], Let ("x", false, Binop (Sub, (Int 1), (Int 1)), ID "x")) in
  let student = "let x = 1-1 in x" |> tokenize |> parse_expr in
  assert (student = result)

let public_expr_minus_one_type _ =
  (* let x = 1-1 in x *)
  let prog = (Let ("x", false, Binop (Sub, (Int 1), (Int 1)), ID "x")) in
  let result = TNum in
  let student = infer prog in
  assert (student = result)

let public_expr_nested_let _ =
  let result = ([],
  Let ("a", false, (Int 1),
   Let ("b", false, (Int 2), Let ("c", false, (Int 3),
     Let ("d", false, (Int 4),
      Let ("e", false, (Int 5),
       Let ("f", false, (Int 6),
        Let ("g", false, (Int 7),
         Let ("h", false, (Int 8),
          Let ("i", false, (Int 9),
           Let ("j", false, (Int 10),
            Binop (Add, ID "a",
             Binop (Add, ID "b",
              Binop (Add, ID "c",
               Binop (Add, ID "d",
                Binop (Add, ID "e",
                 Binop (Add, ID "f", Binop (Add, ID "g", ID "h")))))))))))))))))) in
  let student = "let a = 1 in let b = 2 in let c = 3 in let d = 4 in let e = 5 in let f = 6 in let g = 7 in let h = 8 in let i = 9 in let j = 10 in a+b+c+d+e+f+g+h" |> tokenize |> parse_expr in
  assert (student = result)

let public_expr_nested_let_type _ =
  (* let a = 1 in let b = 2 in let c = 3 in let d = 4 in let e = 5 in let f = 6 in let g = 7 in let h = 8 in let i = 9 in let j = 10 in a+b+c+d+e+f+g+h *)
  let prog = (
  Let ("a", false, (Int 1),
   Let ("b", false, (Int 2), Let ("c", false, (Int 3),
     Let ("d", false, (Int 4),
      Let ("e", false, (Int 5),
       Let ("f", false, (Int 6),
        Let ("g", false, (Int 7),
         Let ("h", false, (Int 8),
          Let ("i", false, (Int 9),
           Let ("j", false, (Int 10),
            Binop (Add, ID "a",
             Binop (Add, ID "b",
              Binop (Add, ID "c",
               Binop (Add, ID "d",
                Binop (Add, ID "e",
                 Binop (Add, ID "f", Binop (Add, ID "g", ID "h")))))))))))))))))) in
  let result = TNum in
  let student = infer prog in
  assert (student = result)

let public_expr_sub1 _ =
  let result = ([], Let ("sub1", false, Fun ("x", Binop (Sub, ID "x", (Int 1))), ID "sub1")) in
  let student = "let sub1 = fun x -> x - 1 in sub1" |> tokenize |> parse_expr in
  assert (student = result)

let public_expr_sub1_type _ =
  (* let sub1 = fun x -> x - 1 in sub1 *)
  let prog = (Let ("sub1", false, Fun ("x", Binop (Sub, ID "x", (Int 1))), ID "sub1")) in
  let result = TFun(TNum, TNum) in
  let student = infer prog in
  assert (student = result)

let public_expr_fact _ =
  let result = ([], Let ("f", true,
                  Fun("x", If (Binop(LessEqual, ID "x", Int 0), Int 1, Binop(Mult, ID "x", FunctionCall(ID "f", Binop(Sub, ID "x", Int 1))))),
                  FunctionCall(ID "f", Int 5))) in
  let student = "let rec f = fun x -> if x <= 0 then 1 else x * f(x-1) in f 5" |> tokenize |> parse_expr in
  assert (student = result)

let public_expr_fact_type _ =
  let prog = (Let ("f", true,
                  Fun("x", If (Binop(NotEqual, ID "x", Int 0), Int 1, Binop(Mult, ID "x", FunctionCall(ID "f", Binop(Sub, ID "x", Int 1))))),
                  FunctionCall(ID "f", Int 5))) in
  let result = TNum in
  let student = infer prog in
  assert (student = result)

(* Higher-order functions *)
let public_expr_ho _ =
  let result = ([], Fun("x", FunctionCall(ID "x", Int 1))) in
  let student = "fun x -> x 1" |> tokenize |> parse_expr in
  assert (student = result)

let public_expr_ho2 _ =
  let result = ([], Fun("a", Fun("b", FunctionCall(ID "a", FunctionCall(ID "a", ID "b"))))) in
  let student = "fun a -> (fun b -> a(a b))" |> tokenize |> parse_expr in
  assert (student = result)

let public_expr_ho3 _ =
  let result = ([], Fun("c", Fun("d", Fun ("e", FunctionCall(FunctionCall(ID "e", ID "c"), ID "d"))))) in
  let student = "fun c -> (fun d -> (fun e -> (e c) d))" |> tokenize |> parse_expr in
  assert (student = result)

let public_expr_ho_type _ =
  (* fun x -> x 1 *)
  let prog = (Fun("x", FunctionCall(ID "x", Int 1))) in
  let a = gen_new_type() in
  (* (int -> 'a) -> 'a *)
  let result = pp_string_of_type (TFun(TFun(TNum, a), a)) in
  let student = pp_string_of_type (infer prog) in
  assert (student = result)

let public_expr_ho2_type _ =
  (* fun a -> (fun b -> a(a b)) *)
  let prog = (Fun("a", Fun("b", FunctionCall(ID "a", FunctionCall(ID "a", ID "b"))))) in
  let a = gen_new_type() in
  (* ('a -> 'a) -> 'a -> 'a *)
  let result = pp_string_of_type (TFun(TFun(a, a), TFun(a, a))) in
  let student = pp_string_of_type (infer prog) in
  assert (student = result)

let public_expr_ho3_type _ =
  (* fun c -> (fun d -> (fun e -> e c d)) *)
  let prog = (Fun("c", Fun("d", Fun ("e", FunctionCall(FunctionCall(ID "e", ID "c"), ID "d"))))) in
  let a = gen_new_type() in
  let b = gen_new_type() in
  let c = gen_new_type() in
  (* 'a -> 'b -> ('a -> 'b -> 'c) -> 'c *)
  let result = pp_string_of_type (TFun(a, TFun(b, TFun(TFun(a, TFun(b, c)), c)))) in
  let student = pp_string_of_type (infer prog) in
  assert (student = result)

let public_expr_hoapp _ =
  let result = ([], FunctionCall(Fun("x", FunctionCall(ID "x", Int 1)), Fun("x", Binop(Add, ID "x", Int 1)))) in
  let student = "(fun x -> x 1) (fun x -> x + 1)" |> tokenize |> parse_expr in
  assert (student = result)

let public_expr_hoapp2 _ =
  let result = ([], FunctionCall (Fun("a", Fun("b", FunctionCall(ID "a", FunctionCall(ID "a", ID "b")))), Fun("c", Fun("d", Fun ("e", FunctionCall(FunctionCall(ID "e", ID "c"), ID "d")))))) in
  let student = "(fun a -> (fun b -> a(a b))) (fun c -> (fun d -> (fun e -> (e c) d)))" |> tokenize |> parse_expr in
  assert (student = result)

let public_expr_hoapp_type _ =
  (* (fun x -> x 1) (fun x -> x + 1) *)
  let prog = (FunctionCall(Fun("x", FunctionCall(ID "x", Int 1)), Fun("x", Binop(Add, ID "x", Int 1)))) in
  let result = TNum in
  let student = infer prog in
  assert (student = result)

let public_expr_hoapp2_type _ =
  (* (fun a -> (fun b -> a(a b))) (fun c -> (fun d -> (fun e -> e c d))) *)
  let prog = FunctionCall (Fun("a", Fun("b", FunctionCall(ID "a", FunctionCall(ID "a", ID "b")))), Fun("c", Fun("d", Fun ("e", FunctionCall(FunctionCall(ID "e", ID "c"), ID "d"))))) in
  try
    let _ = infer prog in
    assert false
  with OccursCheckException -> (type_variable := (Char.code 'a')) | _ -> (type_variable := (Char.code 'a'); assert (1 = 0))

let public_expr_xx _ =
  let result = ([], Fun("x", FunctionCall(ID "x", ID "x"))) in
  let student = "fun x -> x x" |> tokenize |> parse_expr in
  assert (student = result)

let public_expr_xx_type _ =
  let prog = (Fun("x", FunctionCall(ID "x", ID "x"))) in
  try
    let _ = infer prog in
    assert false
  with OccursCheckException -> (type_variable := (Char.code 'a')) | _ -> (type_variable := (Char.code 'a'); assert (1 = 0))

let public_constraint_solving _ =
  let e = (FunctionCall(Fun("x", FunctionCall(ID "x", Int 1)), Fun("x", Binop(Add, ID "x", Int 1)))) in
  let _, _, constraints = gen [] e in
  let student =  unify constraints in
  let result = [("f", TNum); ("c", TNum); ("d", TNum); ("e", TNum); ("a", TFun(TNum, TNum)); ("b", TNum)] in
  assert (student = result)


(*********************)
(* Testing your code *)
(*********************)

let _ = print_string ("Testing your code ...\n")

let error_count = ref 0

let main () =

  (*********************************)
  (* Test cases for type inference *)
  (*********************************)

  let _ = try public_expr_simple_equal_type()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in
  let _ = try public_expr_simple_concat_type()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in
  let _ = try public_expr_simple_div_type()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in
  let _ =  try public_expr_simple_mult_type()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in
  let _ = try public_expr_simple_sub_type()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in
  let _ = try public_expr_simple_sum_type()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in
  let _ = try public_expr_single_and_type()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in
  let _ = try public_expr_single_bool_type()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in
  let _ = try public_expr_single_fun_type()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in
  let _ = try public_expr_single_if_type()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in
  let _ = try public_expr_single_let_type()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in
  let _ = try public_expr_single_notequal_type()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in
  let _ = try public_expr_single_not_type()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in
  let _ = try public_expr_single_number_type()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in
  let _ = try public_expr_single_or_type()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in
  let _ = try public_expr_single_string_type()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in

  let _ = try public_expr_add1_type()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in
  let _ = try public_expr_apply_type()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in
  let _ = try public_expr_double_fun_type()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in
  let _ = try public_expr_let_if_type()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in
  let _ = try public_expr_let_fun_type()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in
  let _ = try public_expr_minus_one_type()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in
  let _ = try public_expr_nested_let_type()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in
  let _ = try public_expr_sub1_type()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in
  let _ = try public_expr_fact_type()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in

  let _ = try public_expr_ho_type()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in
  let _ = try public_expr_ho2_type()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in
  let _ = try public_expr_ho3_type()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in
  let _ = try public_expr_hoapp_type()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in
  let _ = try public_expr_hoapp2_type()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in
  let _ = try public_expr_xx_type()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in

  let _ = try public_constraint_solving()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in


  (*********************************)
  (**** Test cases for parsing *****)
  (*********************************)

  let _ = try public_expr_simple_equal()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in
  let _ = try public_expr_simple_concat()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in
  let _ = try public_expr_simple_div()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in
  let _ = try public_expr_simple_mult()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in
  let _ = try public_expr_simple_sub()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in
  let _ = try public_expr_simple_sum()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in
  let _ = try public_expr_single_and()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in
  let _ = try public_expr_single_bool()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in
  let _ = try public_expr_single_fun()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in
  let _ = try public_expr_single_if()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in
  let _ = try public_expr_single_let()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in
  let _ = try public_expr_single_notequal()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in
  let _ = try public_expr_single_not()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in
  let _ = try public_expr_single_number()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in
  let _ = try public_expr_single_or()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in
  let _ = try public_expr_single_string()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in

  let _ = try public_expr_add1()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in
  let _ = try public_expr_apply()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in
  let _ = try public_expr_double_fun()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in
  let _ = try public_expr_let_if()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in
  let _ = try public_expr_let_fun()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in
  let _ = try public_expr_minus_one()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in
  let _ = try public_expr_nested_let()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in
  let _ = try public_expr_sub1()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in
  let _ = try public_expr_fact()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in

  let _ = try public_expr_ho()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in
  let _ = try public_expr_ho2()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in
  let _ = try public_expr_ho3()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in
  let _ = try public_expr_hoapp()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in
  let _ = try public_expr_hoapp2()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in
  let _ = try public_expr_xx()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in

  if !error_count = 0 then  Printf.printf ("Passed all testcases.\n")
  else Printf.printf ("%d out of 63 programming questions are incorrect.\n") (!error_count)

let _ = main()
