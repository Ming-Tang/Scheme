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
  fun eval env (ConsOnly (SymList argFormat, (Body body))) ->
    Lambda(env, argFormat, body)

let rec evalDefine : EvalRule = fun eval env args ->
  match args with
  | [Sym var; Eval eval env value] ->
    Env.var var value env
    Nil
  | ProperList ((Sym func) :: args) :: (Body body) ->
    evalDefine eval env [Sym func
                         (evalLambda eval env [list args; body])]
  | _ -> failwithf "Must be in the form of (define var value) %s"
                   "or (define (func args...) body...)"

let evalSet : EvalRule = fun eval env (Args2 (SymOnly var,
                                              Eval eval env value)) ->
  Env.set var value env
  Nil

let evalQuote : EvalRule = fun eval env (Args1 x) -> x

let evalQuasiquote : EvalRule = fun eval env (Args1 x) ->
  let rec evalQQ x =
    match x with
    | ProperList [Sym "unquote"; y] ->
      eval env y
    | ProperList xs ->
      list (List.map evalQQ xs)
    | _ -> x
  evalQQ x

let standardRules =
  Map.ofList [
    "begin", evalBegin
    "if", evalIf
    "lambda", evalLambda
    "define", evalDefine
    "set!", evalSet
    "quote", evalQuote
    "quasiquote", evalQuasiquote
  ]

