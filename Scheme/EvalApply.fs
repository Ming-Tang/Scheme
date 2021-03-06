﻿namespace Scheme
open System
open System.Collections.Generic
open Scheme
open Scheme.Expr

[<AutoOpen>]
module Eval =
  /// Evaluate an expression given evaluation rules an initial environment
  let eval { Primitives = prims
             EvalRules = rules } env0 expr0 =

    let (|SelfEvaluating|_|) expr =
      match expr with
      | False | True | Int _ | Real _ | Str _
      | Lambda _ | Macro _ | Prim _ -> Some(Expr.codeToData expr)
      | Sym _ | Cons _ | Nil  -> None

    let (|RuleMatch|_|) expr =
      match expr with
      | ProperList ((Sym name) :: args) ->
        match Map.tryFind name rules with
        | None -> None
        | Some rule -> Some (name, args, rule)
      | _ -> None

    let (|Apply|_|) expr =
      match expr with
      | ProperList (func :: args) ->
        Some(func, args)
      | _ -> None

    let lookup env sym =
      match Env.lookup sym env with
      | Some v -> v
      | None -> failwithf "Undefined variable: %s" sym

    let matchArgs (argNames, rest) args =
      let arityMismatch n =
        failwithf "Arity mismatch: Expecting %d args but got %d."
                  (List.length argNames) n

      let rec matchArgs' n map argNames args =
        match argNames, args with
        | (argName :: argNames'), (arg :: args') ->
          let map' = Map.add argName arg map
          matchArgs' (n + 1) map' argNames' args'
        | [], args ->
          match rest, args with
          | None, [] -> map
          | None, _ -> arityMismatch (n + List.length args)
          | Some rest, _ ->
            Map.add rest (ProperList args) map
        | [], (_ :: _)
        | (_ :: _), [] -> arityMismatch n

      matchArgs' 0 Map.empty argNames args

    let setupFunc (env, argNames, rest, body) args =
      let argMap = matchArgs (argNames, rest) args
      let env' = Env.extend argMap env
      env', body

    let rec eval env expr =
      match expr with
      | SelfEvaluating expr -> expr
      | Sym x -> lookup env x
      | RuleMatch(name, args, rule) -> rule eval env args
      | Apply(func, args) -> apply env func args
      | _ -> failwithf "Invalid expression: %s" (Expr.format expr)

    and apply env func args =
      match eval env func with
      | Lambda(env', argNames, rest, body) ->
        let args = List.map (eval env) args
        setupFunc (env', argNames, rest, body) args ||> eval
      | Prim prim ->
        prims.[prim] <| List.map (eval env) args
      | Macro(env', argNames, rest, body) ->
        let args = List.map codeToData args
        let res = setupFunc (env', argNames, rest, body) args ||> eval
        eval env <| dataToCode res
      | _ -> failwithf "Not a function: %s" (Expr.format func)

    eval env0 expr0

