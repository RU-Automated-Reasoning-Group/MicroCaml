
# MicroCaml Lexer, Parser, and TypeChecker.


## Introduction

Over the course of this project, you will implement the frontend for MicroCaml — a subset of OCaml. The project consists of 3 parts.

In part 1 & 2, you will implement a lexer and a parser for MicroCaml. Your lexer function will convert an input string of MicroCaml into a list of tokens, and your parser function will consume these tokens to produce an abstract symbol tree (AST) for a MicroCaml expression. In part 3, you will implement a type checker (type inferencer) to infer the types for the produced AST.

Here is an example call to the lexer and parser on a MicroCaml expression (as a string):

```ocaml
parse_expr (tokenize "let x = true in x")
```

This will return the AST as the following OCaml value (which we will explain in due course):

```ocaml
 Let ("x", false, (Bool true), ID "x")
```

**Note: We made Part 3 to be independent of Parts 1 and 2. Therefore, you can proceed with implementing the type inference algorithm without concerns about potential errors in your parser.**

### Ground Rules

In your code, you may use any OCaml modules and features we have taught in this class **except imperative OCaml** features like references, mutable records, and arrays.

### Testing & Submitting

Please submit **lexer.ml**, **parser.ml**, and **infer.ml** files to Canvas. To test locally, compile your code with `make` and run `./frontend`.

If you do not have `make`, you can compile the code by `ocamlc -o frontend -I +str str.cma microCamlTypes.ml tokenTypes.ml lexer.mli lexer.ml utils.ml parser.mli parser.ml infer.ml main.ml`.

All tests will be run on direct calls to your code, comparing your return values to the expected return values. Any other output (e.g., for your own debugging) will be ignored. You are free and encouraged to have additional output. For lexer and parser, the only requirement for error handling is that input that cannot be lexed/parsed according to the provided rules should raise an `InvalidInputException`. We recommend using relevant error messages when raising these exceptions, to make debugging easier. We are not requiring intelligent messages that pinpoint an error to help a programmer debug, but as you do this project you might find you see where you could add those. For type inference, please read the instruction on part 3 about the specific requirements for error handling.

You can write your own tests which only test the parser by feeding it a custom token list. For example, to see how the expression `let x = true in x` would be parsed, you can construct the token list manually:

```ocaml
parse_expr [Tok_Let; Tok_ID "x"; Tok_Equal; Tok_Bool true; Tok_In; Tok_ID "x"];;
```

This way, you can work on the parser even if your lexer is not complete yet.

## Part 1: The Lexer (aka Scanner or Tokenizer)

Your parser will take as input a list of tokens; this list is produced by the *lexer* (also called a *scanner*) as a result of processing the input string. Lexing is readily implemented by use of regular expressions, as demonstrated in **[lecture16][lecture16] slides 3-5**. Information about OCaml's regular expressions library can be found in the [`Str` module documentation][str doc]. You aren't required to use it, but you may find it helpful.

Your lexer must be written in [lexer.ml](./lexer.ml). You will need to implement the following function:

#### `tokenize`

- **Type:** `string -> token list`
- **Description:** Converts MicroCaml syntax (given as a string) to a corresponding token list.
- **Examples:**
  ```ocaml
  tokenize "1 + 2" = [Tok_Int 1; Tok_Add; Tok_Int 2]

  tokenize "1 (-1)" = [Tok_Int 1; Tok_Int (-1)]

  tokenize ";;" = [Tok_DoubleSemi]

  tokenize "+ - let def" = [Tok_Add; Tok_Sub; Tok_Let; Tok_Def]

  tokenize "let rec ex = fun x -> x || true;;" =
    [Tok_Let; Tok_Rec; Tok_ID "ex"; Tok_Equal; Tok_Fun; Tok_ID "x"; Tok_Arrow; Tok_ID "x"; Tok_Or; Tok_Bool true; Tok_DoubleSemi]
  ```

The `token` type is defined in [tokenTypes.ml](./tokenTypes.ml).

Notes:
- The lexer input is case sensitive.
- Tokens can be separated by arbitrary amounts of whitespace, which your lexer should discard. Spaces, tabs ('\t') and newlines ('\n') are all considered whitespace.
- When excaping characters with `\` within Ocaml strings/regexp you must use `\\` to escape from the string and regexp.
- If the beginning of a string could match multiple tokens, the **longest** match should be preferred, for example:
  - "let0" should not be lexed as `Tok_Let` followed by `Tok_Int 0`, but as `Tok_ID("let0")`, since it is an identifier.
  - "314dlet" should be tokenized as `[Tok_Int 314; Tok_ID "dlet"]`. Arbitrary amounts of whitespace also includes no whitespace.
  - "(-1)" should not be lexed as `[Tok_LParen; Tok_Sub; Tok_Int(1); Tok_LParen]` but as `Tok_Int(-1)`. (This is further explained below)
- There is no "end" token  -- when you reach the end of the input, you are done lexing.

Most tokens only exist in one form (for example, the only way for `Tok_Concat` to appear in the program is as `^` and the only way for `Tok_Let` to appear in the program is as `let`). However, a few tokens have more complex rules. The regular expressions for these more complex rules are provided here:

- `Tok_Bool of bool`: The value will be set to `true` on the input string "true" and `false` on the input string "false".
  - *Regular Expression*: `true|false`
- `Tok_Int of int`: Valid ints may be positive or negative and consist of 1 or more digits. **Negative integers must be surrounded by parentheses** (without extra whitespace) to differentiate from subtraction (examples below). You may find the functions `int_of_string` and `String.sub` useful in lexing this token type.
  - *Regular Expression*: `[0-9]+` OR `(-[0-9]+)`
  - *Examples of int parenthesization*:
    - `tokenize "x -1" = [Tok_ID "x"; Tok_Sub; Tok_Int 1]`
    - `tokenize "x (-1)" = [Tok_ID "x"; Tok_Int (-1)]`
- `Tok_String of string`: Valid string will always be surrounded by `""` and **should accept any character except quotes** within them (as well as nothing). You have to "sanitize" the matched string to remove surrounding escaped quotes.
  - *Regular Expression*: `\"[^\"]*\"`
  - *Examples*:
    - `tokenize "314" = [Tok_Int 314]`
    - `tokenize "\"314\"" = [Tok_String "314"]`
    - `tokenize "\"\"\"" (* InvalidInputException *)`
- `Tok_ID of string`: Valid IDs must start with a letter and can be followed by any number of letters or numbers. **Note: Keywords may be substrings of IDs**.
  - *Regular Expression*: `[a-zA-Z][a-zA-Z0-9]*`
  - *Valid examples*:
    - "a"
    - "ABC"
    - "a1b2c3DEF6"
    - "fun1"
    - "ifthenelse"

MicroCaml syntax with its corresponding token is shown below, excluding the four literal token types specified above.

Token Name | Lexical Representation
--- | ---
`Tok_LParen` | `(`
`Tok_RParen` | `)`
`Tok_Equal` | `=`
`Tok_NotEqual` | `<>`
`Tok_Greater` | `>`
`Tok_Less` | `<`
`Tok_GreaterEqual` | `>=`
`Tok_LessEqual` | `<=`
`Tok_Or` | `\|\|`
`Tok_And` | `&&`
`Tok_Not` | `not`
`Tok_If` | `if`
`Tok_Then` | `then`
`Tok_Else` | `else`
`Tok_Add` | `+`
`Tok_Sub` | `-`
`Tok_Mult` | `*`
`Tok_Div` | `/`
`Tok_Concat` | `^`
`Tok_Let` | `let`
`Tok_Def` | `def`
`Tok_In` | `in`
`Tok_Rec` | `rec`
`Tok_Fun` | `fun`
`Tok_Arrow` | `->`
`Tok_DoubleSemi` | `;;`

Notes:
- Your lexing code will feed the tokens into your parser, so a broken lexer can cause you to fail tests related to parsing.
- In grammars given below, the syntax matching tokens (lexical representation) is used instead of the token name. For example, the grammars below will use `(` instead of `Tok_LParen`.

## Part 2: Parsing MicroCaml Expressions

In this part, you will implement `parse_expr`, which takes a stream of tokens and outputs as AST for the input expression of type `expr`. Put all of your parser code in [parser.ml](./parser.ml) in accordance with the signature found in [parser.mli](./parser.mli).

We present a quick overview of `parse_expr` first, then the definition of AST types it should return, and finally the grammar it should parse.

### `parse_expr`
- **Type:** `token list -> token list * expr`
- **Description:** Takes a list of tokens and returns an AST representing the MicroCaml expression corresponding to the given tokens, along with any tokens left in the token list.
- **Exceptions:** Raise `InvalidInputException` if the input fails to parse i.e does not match the MicroCaml expression grammar.
- **Examples** (more below):
  ```ocaml
  parse_expr [Tok_Int(1); Tok_Add; Tok_Int(2)] =  Binop (Add, (Int 1), (Int 2))

  parse_expr [Tok_Int(1)] = (Int 1)

  parse_expr [Tok_Let; Tok_ID("x"); Tok_Equal; Tok_Bool(true); Tok_In; Tok_ID("x")] =
  Let ("x", false, (Bool true), ID "x")

  parse_expr [Tok_DoubleSemi] (* raises InvalidInputException *)
  ```

You will likely want to implement your parser using the the `lookahead` and `match_tok` functions that we have provided; more about them is at the end of this README.

### AST and Grammar for `parse_expr`

Below is the AST type `expr`, which is returned by `parse_expr`. **Note** that the `environment` and `Closure of environment * var * expr` parts are only relevant to part 3, so you can ignore them for now.

```ocaml
type op = Add | Sub | Mult | Div | Concat | Greater | Less | GreaterEqual | LessEqual | Equal | NotEqual | Or | And

type var = string

type expr =
  | Int of int
  | Bool of bool
  | String of string
  | ID of var
  | Fun of var * expr (* an anonymous function: var is the parameter and expr is the body *)
  | Not of expr
  | Binop of op * expr * expr
  | If of expr * expr * expr
  | FunctionCall of expr * expr
  | Let of var * bool * expr * expr (* bool determines whether var is recursive *)
```

The CFG below describes the language of MicroCaml expressions. This CFG is right-recursive, so something like `1 + 2 + 3` will parse as `Add (Int 1, Add (Int 2, Int 3))`, essentially implying parentheses in the form `(1 + (2 + 3))`. In the given CFG note that all non-terminals are capitalized, all syntax literals (terminals) are formatted `as non-italicized code` and will come in to the parser as tokens from your lexer. Variant token types (i.e. `Tok_Bool`, `Tok_Int`, `Tok_String` and `Tok_ID`) will be printed *`as italicized code`*.

- Expr -> LetExpr | IfExpr | FunctionExpr | OrExpr
- LetExpr -> `let` Recursion *`Tok_ID`* `=` Expr `in` Expr
  -	Recursion -> `rec` | ε
- FunctionExpr -> `fun` *`Tok_ID`* `->` Expr
- IfExpr -> `if` Expr `then` Expr `else` Expr
- OrExpr -> AndExpr `||` OrExpr | AndExpr
- AndExpr -> EqualityExpr `&&` AndExpr | EqualityExpr
- EqualityExpr -> RelationalExpr EqualityOperator EqualityExpr | RelationalExpr
  - EqualityOperator -> `=` | `<>`
- RelationalExpr -> AdditiveExpr RelationalOperator RelationalExpr | AdditiveExpr
  - RelationalOperator -> `<` | `>` | `<=` | `>=`
- AdditiveExpr -> MultiplicativeExpr AdditiveOperator AdditiveExpr | MultiplicativeExpr
  - AdditiveOperator -> `+` | `-`
- MultiplicativeExpr -> ConcatExpr MultiplicativeOperator MultiplicativeExpr | ConcatExpr
  - MultiplicativeOperator -> `*` | `/`
- ConcatExpr -> UnaryExpr `^` ConcatExpr | UnaryExpr
- UnaryExpr -> `not` UnaryExpr | FunctionCallExpr
- FunctionCallExpr -> PrimaryExpr PrimaryExpr | PrimaryExpr
- PrimaryExpr -> *`Tok_Int`* | *`Tok_Bool`* | *`Tok_String`* | *`Tok_ID`* | `(` Expr `)`

Notice that this grammar is not actually quite compatible with recursive descent parsing. In particular, the first sets of the productions of many of the non-terminals overlap. For example:

- OrExpr -> AndExpr `||` OrExpr | AndExpr

defines two productions for nonterminal OrExpr, separated by |. Notice that both productions starting with AndExpr, so we can't use the lookahead (via FIRST sets) to determine which one to take. This is clear when we rewrite the two productions thus:

- OrExpr -> AndExpr `||` OrExpr
- OrExpr -> AndExpr

When my parser is handling OrExpr, which production should it use? From the above, you cannot tell. The solution is: **You need to refactor the grammar, as shown in [lecture16][lecture16]** slides 35-37.

To illustrate `parse_expr` in action, we show several examples of input and their output AST.

### Example 1: Basic math

**Input:**
```ocaml
(1 + 2 + 3) / 3
```

**Output (after lexing and parsing):**
```ocaml
Binop (Div,
  Binop (Add, (Int 1), Binop (Add, (Int 2), (Int 3))),
  (Int 3))
```

In other words, if we run `parse_expr (tokenize "(1 + 2 + 3) / 3")` it will return the AST above.

### Example 2: `let` expressions

**Input:**
```ocaml
let x = 2 * 3 / 5 + 4 in x - 5
```

**Output (after lexing and parsing):**
```ocaml
Let ("x", false,
  Binop (Add,
    Binop (Mult, (Int 2), Binop (Div, (Int 3), (Int 5))),
    (Int 4)),
  Binop (Sub, ID "x", (Int 5)))
```

### Example 3: `if then ... else ...`

**Input:**
```ocaml
let x = 3 in if not true then x > 3 else x < 3
```

**Output (after lexing and parsing):**
```ocaml
Let ("x", false, (Int 3),
  If (Not ((Bool true)), Binop (Greater, ID "x", (Int 3)),
   Binop (Less, ID "x", (Int 3))))
```

### Example 4: Anonymous functions

**Input:**
```ocaml
let rec f = fun x -> x ^ 1 in f 1
```

**Output (after lexing and parsing):**
```ocaml
Let ("f", true, Fun ("x", Binop (Concat, ID "x", (Int 1))),
  FunctionCall (ID "f", (Int 1)))
```

Keep in mind that the parser is not responsible for finding type errors. This is the job of type inference (part 3). For example, while the AST for `1 1` should be parsed as `FunctionCall ((Int 1), (Int 1))`; if it is checked by type inference, it will at that time be flagged as a type error.

### Example 5: Recursive anonymous functions

Notice how the AST for `let` expressions uses a `bool` flag to determine whether a function is recursive or not. When a recursive anonymous function `let rec f = fun x -> ... in ...` is defined, `f` will bind to `fun x -> ...` when evaluating the function.

**Input:**
```ocaml
let rec f = fun x -> f (x*x) in f 2
```

**Output (after lexing and parsing):**
```ocaml
Let ("f", true,
  Fun ("x", FunctionCall (ID "f", Binop (Mult, ID "x", ID "x"))),
  FunctionCall (ID "f", (Int 2))))
```

### Example 6: Currying

We will **ONLY** be currying to create multivariable functions as well as passing multiple arguments to them. Here is an example:

**Input:**
```ocaml
let f = fun x -> fun y -> x + y in (f 1) 2
```

**Output (after lexing and parsing):**
```ocaml
Let ("f", false,
  Fun ("x", Fun ("y", Binop (Add, ID "x", ID "y"))),
  FunctionCall (FunctionCall (ID "f", (Int 1)), (Int 2)))
```

### Provided functions

We have provided some helper functions in the `parser.ml` file. You are not required to use these, but they are recommended.

#### `match_token`
- **Type:** `token list -> token -> token list`
- **Description:** Takes the list of tokens and a single token as arguments, and returns a new token list with the first token removed IF the first token matches the second argument.
- **Exceptions:** Raise `InvalidInputException` if the first token does not match the second argument to the function.

#### `match_many`
- **Type:** `token list -> token list -> token list`
- **Description:** An extension of `match_token` that matches a sequence of tokens given as the second token list and returns a new token list with that matches each token in the order in which they appear in the sequence. For example, `match_many toks [Tok_Let]` is equivalent to `match_token toks Tok_Let`.
- **Exceptions:** Raise `InvalidInputException` if the tokens do not match.

#### `lookahead`
- **Type:** `token list -> token option`
- **Description:** Returns the top token in the list of tokens as an option, returning `None` if the token list is empty. **In constructing your parser, the lack of lookahead token (None) is fine for the epsilon case.**

#### `lookahead_many`
- **Type:** `token list -> int -> token option`
- **Description:** An extension of `lookahead` that returns token at the nth index in the list of tokens as an option, returning `None` if the token list is empty at the given index or the index is negative. For example, `lookahead_many toks 0` is equivalent to `lookahead toks`.

## Part 3: Type Inference

In this part, you will implement type inference. Put all of your type inference code in [infer.ml](./infer.ml).

### Typing Constraint Generation

As stated in the [lecture13][lecture13], type inference consists of three steps: (1) Annotate (2) Generate Constraints (3) Solve Constraints. In this part of the project, we will implement a function `gen` for both annotation and constraint generation:
```ocaml
let rec gen (env: environment) (e: expr): aexpr * typeScheme * (typeScheme * typeScheme) list = ...
```
which takes a typing environment `env` and an AST for an input expression of type `expr` and outputs an annotated expression of type `aexpr` that holds the type information for the (sub-expressions) of the given expression `e`, the type of `e` which is represented as a `typeScheme`, and a list of typing constraints `(typeScheme * typeScheme) list`. We provide detailed information about `environment`, `aexpr`, `typeScheme` below.

We defined the typing environment in [infer.ml](./infer.ml):
```ocaml
type environment = (var * typeScheme) list
```
An environment is a map that holds the type information `typeScheme` of any variables `var` currently in scope. As an example, consider an expression `let x = 5 in let y = 10 in x + y`. The typing environment of the expression `x+y` is a list `[(y:int); (x: int)]` that maps both `x` and `y` to the `int` type.

We defined the data structure for `typeScheme` in [microCamlTypes.ml](./microCamlTypes.ml):
```ocaml
type typeScheme =
    | TNum                                  (int type)
    | TBool                                 (bool type)
    | TStr                                  (string type)
    | T of string                           (type variable for an unknown type)
    | TFun of typeScheme * typeScheme       (function type)
```
More information about type schemes can be found in the [lecture13][lecture13] slides. As an example, consider a function `fun x -> x 1`. Its OCaml type is `(int -> 'a) -> 'a` (the higher order function takes a function `x` as input and returns the results of applying the function `x` to 1). In our project, this type is written as  `TFun(TFun(TNum, T "'a"), T "'a")`.

We defined the data structure for an expression annotated with types as `aexpr` in [microCamlTypes.ml](./microCamlTypes.ml):
```ocaml
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
```
It follows `type expr` defined above except that any `aexpr` expression must be paired with a type scheme which holds the type information of the expression. For example, `5` should be annotated as `AInt (5, TNum)`. As another example, consider the anonymous function `fun x -> x 1`. As we initially have no idea of the type of the function parameter `x`, we use a type scheme `T "a"` to represent the unknown type of `x`. Similarly, we do not know the type of the return of the anonymous function so we use a type scheme `T "b"` to represent this unknown type. Observe that `x` is applied to `1` in the body of the anonymous function. As we have no knowledge about the return type of the function `x`, we once again annotate `x 1` a type scheme `T "c"`. Thus, the annotated expression is:
```ocaml
(fun x -> ((x: a) (1: int)): c): (a -> b)
```
or in OCaml code:
```ocaml
AFun("x", AFunctionCall(AID("x", T "a"), AInt(1, TNum), T "c"), TFun(T "a", T "b"))
```
It is important to note that the annotation is recursive as every subexpression is also annotated in the above example.

The third element in the return of `gen env e` is a list of typing constraints `(typeScheme * typeScheme) list` collected by recursively traversing the input expression `e` under the typing environment `env`. For the anonymous function `fun x -> x 1`, the following typing constraints should be generated:
```
a = (int -> c)
c = b
```
or in OCaml code:
```ocaml
[(T "a", TFun(TNum, T "c"));
 (T "c", T "b")]
```
In the first constraint, `T "a"` is the annotated type of `x` (see above). In the body of the anonymous function, `x` is used a function that applies to an integer (`TNum`) and the function application is annotated with `T "c"`. We constrain `T "a"` same as `TFun(TNum, T "c")`. The second constraint is derived from the fact that the result of the function application `x 1`, annotated with type `T "c"`, is used as the return value of the anonymous function, annotated with return type `T "b"`, so we constrain `T "c"` same as `T "b"`.

Formally, the function `gen` should implement the following typing rules to collect the typing constraints. The rules are (recursively) defined in the shape of `G |- u ==> e, t, q` where `G` is a typing environment (initialized to []), `u` is an unannotated expression of type `expr`, `e` is an annotated expression of type `aexpr`, `t` is the annotated type of `u`, and `q` is the list of typing constraints for `u` that must be solved. In other words, the following typing rules jointly define `gen G u = e t q`.

1. Typing the integers.
```
------------------------------
G |- n ==> (n: int), int, []
```

For example, `gen [] (Int 5) = AInt (5, TNum), TNum, []`. No typing constraint is generated.

2. Typing the Booleans.
```
------------------------------
G |- b ==> (b: bool), bool, []
```

For example, `gen [] (Bool true) = ABool (true, TBool), TBool, []`. No typing constraint is generated.

3. Typing the strings.
```
----------------------------------
G |- s ==> (s: string), string, []
```

For example, `gen [] (String "CS314") = AString ("CS314", TStr), TStr, []`. No typing constraint is generated.

4. Typing the variables (identifiers).
```
----------------------------------------------
G |- x ==> (x: t), t, []   (if G(x) = t)
```

For example, `gen [("x", TNum); ("y", TNum)] (ID "x") = AID ("x", TNum), TNum, []`. No typing constraint is generated.

If a variable does not appear in its typing environment, raise `UndefinedVar` exception. `gen [] (ID "x")` raises `UndefinedVar` because `x` is undefined in the typing environment.

5. Typing the function definitions.
```
G; x: a |- u ==> e, t, q               (for fresh a, b)
--------------------------------------------------------------------  
G |- (fun x -> u) ==> (fun x -> e : (a -> b)), a -> b, q @ [(t, b)]
```

See the above example on the anonymous function `fun x -> x 1` for how this rule executes. We have `gen [] (Fun ("x", FunctionCall (ID "x", Int 1))) = AFun("x", AFunctionCall(AID("x", T "a"), AInt(1, TNum), T "c"), TFun(T "a", T "b")), TFun(T "a", T "b"), [(T "a", TFun(TNum, T "c")); (T "c", T "b")]`. The typing constraints `[a = (int -> c); c = b]` are explained above.


6. Typing the negation of Boolean expressions.

```
G |- u ==> e, t, q
---------------------------------------------------
G |- not u ==> (not e: bool), bool, q @ [(t, bool)]   
```

For example, `gen [] (Not (Bool true)) = ANot (ABool (true, TBool), TBool), TBool, [(TBool, TBool)]`. The typing constraint requires the annotated type `t` for `u` (in this example `TBool`) same as `TBool`.

7. Typing binary operations.

```
G |- u1 ==> e1, t1, q1   G |- u2 ==> e2, t2, q2
-----------------------------------------------------------------------------------
G |- u1 ^ u2 ==>  (e1 ^ e2: string), string, q1 @ q2 @ [(t1, string), (t2, string)]
```

We constrain the annotated types for `u1` and `u2` in `u1 ^ u2` as string.

```
G |- u1 ==> e1, t1, q1   G |- u2 ==> e2, t2, q2
-----------------------------------------------------------------------
G |- u1 + u2 ==>  (e1 + e2: int), int, q1 @ q2 @ [(t1, int), (t2, int)]
```

We constrain the annotated types for `u1` and `u2` in `u1 + u2` as int.

```
G |- u1 ==> e1, t1, q1   G |- u2 ==> e2, t2, q2
-------------------------------------------------------------
G |- u1 < u2 ==>  (e1 < e2: bool), bool, q1 @ q2 @ [(t1, t2)]
```

We constrain the annotated type for `u1` and `u2` in `u1 < u2` to be the same. For comparison operators ("<", ">", "=", "<=", ">="), we assume they are generic meaning that they can take values of arbitrary types, provided that the operands have the same type.

8. Typing if expressions.

```
G |- u1 ==> e1, t1, q1   G |= u2 ==> e2, t2, q2   G |- u3 ==> e3, t3, q3
-------------------------------------------------------------------------------------------------------
G |- (if u1 then u2 else u3) ==> (if e1 then e2 else e3: t2), t2, q1 @ q2 @ q3 @ [(t1, bool); (t2, t3)]
```

We constrain the annotated type for `u1` as bool and the annotated types for `u2` and `u3` the same.

9. Typing function applications.

```
G |- u1 ==> e1, t1, q1
G |- u2 ==> e2, t2, q2                 (for fresh a)
--------------------------------------------------------
G |- u1 u2 ==> (e1 e2: a), a, q1 @ q2 @ [(t1 = t2 -> a)]
```

See the above example on the anonymous function `fun x -> x 1` for how this rule executes. We have `gen [("x", T "a")] (FunctionCall (ID "x", Int 1)) = AFunctionCall(AID("x", T "a"), AInt(1, TNum), T "c"), T "c", [(T "a", TFun(TNum, T "c"))]`. Here as we have no knowledge about the return type of the function `x`, we annotate `x 1` a type scheme `T "c"`.  As `x` is used a function that applies to an integer (`TNum`), we constrain `T "a"` (the type of `x` in the typing environment) as `TFun(TNum, T "c")`. Informally, the constraint is `a = (int -> c)`.

10. Typing let expressions.

```
G |- u1 ==> e1, t1, q1   G; x: t1 |- u2 ==> e2, t2, q2
---------------------------------------------------------------
G |- (let x = u1 in u2) ==> (let x = e1 in e2: t2), t2, q1 @ q2  
```

```
G; f: a |- u1 ==> e1, t1, q1   G; x: t1 |- u2 ==> e2, t2, q2  (for fresh a)
-----------------------------------------------------------------------------------
G |- (let rec f = u1 in u2) ==> (let rec f = e1 in e2: t2), t2, q1 @ [(a, t1)] @ q2  
```

The second rule above is for typing recursive functions e.g. `let rec f = fun x -> if x <= 0 then 1 else x * f(x-1) in f 5`. When type checking for `u1`, it is important to add to the type environment a type for `f`. This is because `f` can be recursively used in `u1`. Since we do not know the type of `f` initially, we annotate it with an unknown type `a` and build constraints over it. When type checking `u2`, it is also necessary to extend the typing environment with `(x: t1)` as `x` may be used in `u2`.

### Typing Constraint Solver

Given the typing constraints returned by `gen`, the unification algorithm (defined in [infer.ml](./infer.ml))
```ocaml
let rec unify (constraints: (typeScheme * typeScheme) list) : substitutions = ...
```
solves a given set of typing constraints obtained from the `gen` method and returns the solution of type `substitutions`.

In [infer.ml](./infer.ml), we define the type `substitutions` as:
```ocaml
type substitutions = (string * typeScheme) list
```

As an example, consider the typing constraints generated for the program `fun x -> x 1` (explained above):
```
a = (int -> c)
c = b
```
or in OCaml code:
```ocaml
[(T "a", TFun(TNum, T "c"));
 (T "c", T "b")]
```
The solution to the typing constraints is
```
[("a", TFun(TNum, T"b")); ("c", T "b")]
```
Intuitively, this solution is a substitution `[int->b/a, b/c]`. Applying this substitution to the typing constraints mentioned above would render the left-hand side and right-hand side of each constraint equivalent. With this solution, we have the type of `fun x -> x 1` as `(int -> b) -> b`. The pretty printer.

We have already provided you a working implementation of `unify` in [infer.ml](./infer.ml). Please read the comment associated with `unify` in the code to understand how it executes. The implementation is similar to the pseudocode presented in [lecture13][lecture13]. Reviewing the examples covered in [lecture13][lecture13] would be beneficial.

However, there are two issues with this implementation that you need to resolve.

First, the solution (`substitutions`) found by the existing `unify` code is suboptimal. For example, consider the program `(fun x -> x 1) (fun x -> x + 1)`. Evaluating this program yields 2. This expression is of type `int`. Applying `gen` to this program should generate the following annotated program:
```ocaml
((fun x -> ((x: a) (1: int)): c): (a -> b) (fun x -> ((x: d) + (1: int): int)): (d -> e)): f
```
where `a`, `b`, `c`, `d`, `e`, `f` and `g` are unknown type variables to be solved, and generate the following typing constraints over the type variables:
```
a = (int -> c)
c = b
d = int
int = int
int = e
(a -> b) = ((d -> e) -> f)
```

Applying `unify` to these constraints would render the following substitutions as the solution.
```
f: int
c: f
d: int
e: int
a: (d -> e)
b: f
```
The solution figures out that `f` should be mapped to `int`. Thus, we are able to conclude that `(fun x -> x 1) (fun x -> x + 1)` (annotated by `f`) is of type `int`. The solution is, however, suboptimal because it does fully infer the types of all the subexpressions. For example, `(fun x -> x 1)` was annotated by `gen` as `a -> b`. The solution does not directly map `a` and `b` to a meaningful concrete type.

Your task is to modify the `unify` implementation so it can derive the following ideal solution:
```
f: int
c: int
d: int
e: int
a: (int -> int)
b: int
```

The second issue in the existing `unify` implementation is that it does not consider occurs check. In [lecture13][lecture13], we discussed that the program `fun x -> x x` should be rejected by the type checker. Applying `gen` to this program should generate the following annotated program:
```ocaml
(fun x -> ((x: a) (x: a)): c): (a -> b)
```
where `a`, `b` and `c` are unknown type variables to be solved, and generate the following typing constraints over the type variables:
```
a = (a -> c)
c = b
```
The typing constraints are unsolvable because of the constraint `a = (a -> c)`. The type variable `a` occurs on both sides of the constraint.

Your task is to modify the `unify` implementation to conduct occurs check so as to reject a program like `fun x -> x x` by raising the `OccursCheckException` exception in `unify` when a typing constraint like `a = (a -> c)` presents.

### Provided functions

#### `gen_new_type`
- **Type:** `unit -> typeScheme`
- **Description:** Returns T(string) as a new unknown type placeholder. Every call to `gen_new_type` generates a fresh type variable. For example, the program `let a = gen_new_type() in let b = gen_new_type in (a, b)` returns two type variables `T "a"` and `T "b"`. This function is particularly useful for implementing the typing rules. The function is defined in [infer.ml](./infer.ml).

#### `string_of_op`, `string_of_type`, `string_of_aexpr`, `string_of_expr`
- **Description:** Returns a string representation of a given operator `op`, type `typeScheme`, annotated expression `aexpr`, and unannotated expression `expr`. These functions are defined [microCamlTypes.ml](./microCamlTypes.ml) and would be very useful for debugging your implementation.

#### `pp_string_of_type`, `pp_string_of_aexpr`
- **Description:** Pretty printers for returning a string representation of a given type `typeScheme` and annotated expression `aexpr`. These functions outputs types in the OCaml style. For exmple, consider the expression `fun x -> x 1`:
```ocaml
let e = (Fun("x", FunctionCall(ID "x", Int 1))) in
let annotated_expr, t, constraints = gen env e in
let subs = unify constraints in
let annotated_expr = apply_expr subs annotated_expr in
let _ = print_string (pp_string_of_aexpr annotated_expr) in
print_string (pp_string_of_type t
```
The type of `fun x -> x 1` would be printed as the OCaml type `((int -> 'a) -> 'a)`. These functions are defined [microCamlTypes.ml](./microCamlTypes.ml) and would be useful to present the type inference result back to the programmer.

#### `string_of_constraints`, `string_of_subs`
- **Description:** These functions return the string representation of typing constraints and solutions. They can be helpful for debugging. The functions are defiend in [infer.ml](./infer.ml).

#### `infer`
- **Type:**  `expr -> typeScheme`
- **Description:** This function is what we will use to test your code for type inference. Formally, `infer e` invokes the `gen` function to collect the typing constraints from the given expression `e`, solves the typing constraints using the `unify` function, and finally returns the inferred type of `e`. The function is defined in [infer.ml](./infer.ml).

```ocaml
let e = (Fun("x", FunctionCall(ID "x", Int 1))) in
let t = infer e in
pp_string_of_type (t)   (* ((int -> 'a) -> 'a) *)
```
The above program prints out the OCaml type of `fun x -> x 1`.



[str doc]: https://caml.inria.fr/pub/docs/manual-ocaml/libref/Str.html
[lecture16]: https://github.com/RU-Automated-Reasoning-Group/MicroCaml/blob/main/16-parsing.pdf
[lecture13]: https://github.com/RU-Automated-Reasoning-Group/MicroCaml/blob/main/13-type-inference.pdf
