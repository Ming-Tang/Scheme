﻿module Scheme.Rules
open System
open System.Collections.Generic
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

let translation f : EvalRule = fun eval env args ->
  f args |> eval env

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

let evalAnd : EvalRule = translation <| fun xs ->
  List.foldBack (fun a b -> if' a b False) xs True

let evalOr : EvalRule = translation <| fun xs ->
  List.fold (fun a b -> if' a True b) False xs

let evalCond : EvalRule =
  translation <| fun (CondList (cases, els)) ->
  let elsePart =
    match els with
    | None -> list [Sym "error"; Str "cond ran out of cases."]
    | Some x -> x
  List.foldBack (fun (a, b) c -> list [Sym "if"; a; b; c]) cases elsePart

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
    "cond", evalCond
    "lambda", evalLambda
    "define", evalDefine
    "set!", evalSet
    "unset!", evalUnset
    "and", evalAnd
    "or", evalOr
    "let", evalLet
    "local", evalLocal
    "quote", evalQuote
    "quasiquote", evalQuasiquote
    "unquote", evalError "unquote"
    "unquote-splicing", evalError "unquote-splicing"
    "else", evalError "else"
  ]

