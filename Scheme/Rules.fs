module Scheme.Rules
open System
open System.Collections.Generic
open Scheme
open Scheme.ActivePatterns

/// Construct a (begin ...) block from a list of exprs
let Begin xs = ProperList (Sym "begin" :: xs)

/// Quote an expr
let Quote xs = ProperList [Sym "quote"; xs]

/// Unquote an expr
let Unquote xs = ProperList [Sym "unquote"; xs]

let translation f : EvalRule = fun eval env args ->
  f args |> eval env

let evalBegin : EvalRule = fun eval env args ->
  List.fold (fun _ b -> eval env b) Nil args

let evalIf : EvalRule = fun eval env (Args3 (Eval eval env cond,
                                             left, right)) ->
  match cond with
  | IsTrue -> eval env left
  | IsFalse -> eval env right

let evalLambda : EvalRule =
  fun eval env (Args1OrMore(argFormat, Body body)) ->
    match argFormat with
    | ProperImproperList(SymList args, None) ->
      Lambda(env, args, None, body)
    | ProperImproperList(SymList args, Some (Sym rest)) ->
      Lambda(env, args, Some rest, body)
    | ProperImproperList(_, _) ->
      failwithf "Incorrect argument format. Must be a list of symbols %s"
                "or a list of symbols dot another symbol."

let rec evalDefine : EvalRule = fun eval env args ->
  match args with
  | [Sym var; Eval eval env value] ->
    Env.var var value env
    Nil
  | Cons(Sym func, args) :: Body body ->
    let defineArgs : Code Expr list =
      [Sym func; dataToCode (evalLambda eval env [args; body])]
    evalDefine eval env defineArgs
  | _ -> failwithf "Must be in the form of (define var value) %s"
                   "or (define (func args...) body...)."

let evalDefineMacro : EvalRule = fun eval env args ->
  match args with
  | Cons(Sym name, args) :: Body body ->
    let (LambdaOnly(e, a, d, b)) = evalLambda eval env [args; body]
    let macro = Macro(e, a, d, b)
    Env.var name macro env
    Nil
  | _ -> failwith "Invalid define-macro form."

let evalDefineSyntax : EvalRule =
  fun eval env (Args1 (ConsOnly(SymOnly name, rest))) ->
    match name with
    | "macro-transformer" -> evalDefineMacro eval env rest
    | "syntax-rules" | _ ->
      failwithf "Unsupported syntax transformer: %s" name

let evalApply : EvalRule =
  fun eval env (Args2(f, Eval eval env (ProperListOnly xs))) ->
    eval env (Cons(f, dataToCode (list (List.map Quote xs))))

let evalSet : EvalRule = fun eval env (Args2 (SymOnly var,
                                              Eval eval env value)) ->
  Env.set var value env
  Nil

let evalUnset : EvalRule = fun eval env (Args1 (SymOnly var)) ->
  Env.delete var env |> ignore
  Nil

let evalQuote : EvalRule = fun eval env (Args1 x) -> codeToData x

let evalQuasiquote : EvalRule = fun eval env (Args1 x) ->
  let rec append (ProperListOnly xs) ys =
    List.foldBack (fun a b -> Cons(a, b)) xs ys

  let rec evalQQ n x =
    match x with
    | ProperList [Sym "quasiquote"; y] ->
      list [Sym "quasiquote"; evalQQ (n + 1) y]
    | ProperList [Sym "unquote"; y] ->
      if n = 0 then eval env y
      else list [Sym "unquote"; evalQQ (n - 1) y]
    | ProperList [Sym "unquote-splicing"; y] ->
      list [Sym "unquote-splicing"; evalQQ (if n = 0 then 0 else n - 1) y]
    | Cons(ProperList [Sym "unquote-splicing"; y], z) ->
      if n = 0 then append (eval env y) (evalQQ 0 z)
      else Cons(list [Sym "unquote-splicing"; evalQQ (n - 1) y], evalQQ n z)
    | Cons(a, b) ->
      Cons(evalQQ n a, evalQQ n b)
    | _ -> codeToData x
  evalQQ 0 x

let (|CondList|) xs =
  let rec parse cs xs =
    match xs with
    | [] -> cs, None
    | (ProperListOnly (Args2(a, b))) :: xs ->
      match a, xs with
      | Sym "else", [] -> cs, Some b
      | Sym "else", _ -> failwith "else must be the last clause."
      | _ -> parse (cs @ [a, b]) xs
  parse [] xs

let evalError name : EvalRule = fun eval env args ->
  failwithf "Unexpected %s." name

let if' a b c = list [Sym "if"; a; b; c]
let lambda args body = list [Sym "lambda"; args; body]

let rec shortCircuit f f1 a (args : Code Expr list) : Data Expr =
  match args with
  | [] -> a
  | [x] -> f1 x
  | x :: xs -> f x (fun() -> shortCircuit f f1 a xs)

let evalAnd : EvalRule = fun eval env args ->
  shortCircuit (fun (Eval eval env a) b ->
    match a with
    | IsTrue -> b()
    | IsFalse -> False) (eval env) True args

let evalOr : EvalRule = fun eval env args ->
  shortCircuit (fun (Eval eval env a) b ->
    match a with
    | IsTrue -> a
    | IsFalse -> b()) (eval env) False args

let evalCond : EvalRule =
  translation <| fun (CondList (cases, els)) ->
  let elsePart =
    match els with
    | None -> Quote Nil
    | Some x -> x
  List.foldBack (fun (a, b) c -> if' a b c) cases elsePart

let evalCase : EvalRule =
  fun eval env (Args1OrMore(Eval eval env value, CondList(cases, els))) ->
  let rec evalCases cases =
    match cases with
    | [] ->
      match els with
      | None -> Nil
      | Some expr -> eval env expr
    | (ProperListOnly cases, expr) :: rest ->
      if cases
         |> List.tryFind (codeToData >> (=) value)
         |> Option.isSome
      then
        eval env expr
      else
        evalCases rest
  evalCases cases

let (|BindingList|) (ProperListOnly pairs) =
  pairs
  |> List.map (
    function
    | ProperList [ Sym name; value ] -> name, value
    | _ -> failwith "Invalid binding list.")

let evalLet : EvalRule =
  translation <| fun (Args1OrMore(BindingList bindings, Body body)) ->
  let vars, vals = List.unzip bindings
  Cons(lambda (list (List.map Sym vars)) body,
       list vals)

let evalLetStar : EvalRule =
  translation <| fun (Args1OrMore(BindingList bindings, Body body)) ->
  List.foldBack (fun (var, expr) body ->
    Cons(lambda (list [Sym var]) body, list [expr])
  ) bindings body

let evalLetRec : EvalRule =
  translation <| fun (Args1OrMore(BindingList bindings, Body body)) ->
  let vars, vals = List.unzip bindings
  let placeholders = List.map (fun _ -> Quote Nil) bindings
  let assignments =
    bindings
    |> List.map (fun (var, value) -> list [Sym "set!"; Sym var; value])
    |> Begin
  let body' = Begin [assignments; body]
  Cons(lambda (list (List.map Sym vars)) body',
       list placeholders)

let evalLocal : EvalRule =
  fun eval env (Args1OrMore(ProperListOnly defs, Body body)) ->
  let local = Env.extend Map.empty env
  for def in defs do
    match def with
    | ProperList (Sym "define" :: rest) ->
      evalDefine eval local rest |> ignore
    | ProperList (Sym "define-macro" :: rest) ->
      evalDefineMacro eval local rest |> ignore
    | _ -> failwith "Not a define in local body"
  eval local body

let evalDelay : EvalRule =
  translation <| fun (Args1(expr)) ->
  list [Sym "make-promise"; lambda Nil expr]


let standardRules =
  Map.ofList [
    "begin", evalBegin
    "delay", evalDelay

    "if", evalIf
    "cond", evalCond
    "case", evalCase

    "and", evalAnd
    "or", evalOr

    "lambda", evalLambda
    "define", evalDefine
    "*apply", evalApply
    "define-macro", evalDefineMacro
    "define-syntax", evalDefineSyntax

    "set!", evalSet
    "unset!", evalUnset

    "let", evalLet
    "let*", evalLetStar
    "letrec", evalLetRec
    "local", evalLocal

    "quote", evalQuote
    "quasiquote", evalQuasiquote

    "unquote", evalError "unquote"
    "unquote-splicing", evalError "unquote-splicing"
    "else", evalError "else"
  ]

