module Scheme.Rules
open System;
open System.Collections.Generic;
open Scheme
open Scheme.ActivePatterns

let evalBegin : EvalRule = fun eval env args ->
  List.fold (fun _ b -> eval env b) Nil args

let evalIf : EvalRule = fun eval env (Args3 (Eval eval env cond,
                                             left, right)) ->
  match cond with
  | IsTrue -> eval env left
  | IsFalse -> eval env right

let evalLambda : EvalRule =
  fun eval env (ConsOnly(argFormat, Body body)) ->
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

let evalSet : EvalRule = fun eval env (Args2 (SymOnly var,
                                              Eval eval env value)) ->
  Env.set var value env
  Nil

let evalQuote : EvalRule = fun eval env (Args1 x) -> codeToData x

let evalQuasiquote : EvalRule = fun eval env (Args1 x) ->
  let rec evalQQ x =
    match x with
    | ProperList [Sym "unquote"; y] ->
      eval env y
    | ProperList xs ->
      list (List.map evalQQ xs)
    | _ -> codeToData x
  evalQQ x

let translation f : EvalRule = fun eval env args ->
  f args |> eval env

let if' a b c = list [Sym "if"; a; b; c]
let lambda args body = list [Sym "lambda"; args; body]

let evalAnd : EvalRule = translation <| fun xs ->
  List.foldBack (fun a b -> if' a b False) xs True

let evalOr : EvalRule = translation <| fun xs ->
  List.fold (fun a b -> if' a True b) False xs

let evalLet : EvalRule =
  translation <| fun (ConsOnly(ProperListOnly pairs, Body body)) ->
  let vars, vals =
    pairs
    |> List.map (
      function
      | ProperList [ Sym name; value ] -> name, value
      | _ -> failwith "Invalid let form.")
    |> List.unzip
  Cons(lambda (list (List.map Sym vars)) body,
       list vals)

let evalLocal : EvalRule = fun eval env (ConsOnly(ProperListOnly defs, Body body)) ->
  let local = Env.extend Map.empty env
  for def in defs do
    match def with
    | ProperList (Sym "define" :: rest) -> evalDefine eval local rest |> ignore
    | _ -> failwith "Not a define in local body"
  eval local body

let standardRules =
  Map.ofList [
    "begin", evalBegin
    "if", evalIf
    "lambda", evalLambda
    "define", evalDefine
    "set!", evalSet
    "and", evalAnd
    "or", evalOr
    "let", evalLet
    "local", evalLocal
    "quote", evalQuote
    "quasiquote", evalQuasiquote
  ]

