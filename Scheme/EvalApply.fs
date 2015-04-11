namespace Scheme
open System;
open System.Collections.Generic;
open Scheme

[<AutoOpen>]
module Eval =
  /// Evaluate an expression given evaluation rules an initial environment
  let eval { Primitives = prims
             EvalRules = rules } env0 expr0 =

    let (|SelfEvaluating|_|) expr =
      match expr with
      | Nil | False | True | Int _ | Real _ | Str _
      | Lambda(_, _, _, _) -> Some()
      | Prim _ | Sym _ | Cons _  -> None

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
      | None when Map.containsKey sym prims -> Prim sym
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

    let rec eval env expr =
      match expr with
      | SelfEvaluating -> expr
      | Sym x -> lookup env x
      | RuleMatch (name, args, rule) -> rule eval env args
      | Apply (func, args) -> apply env func args
      | _ -> failwithf "Invalid expression: %A" expr

    and apply env func args =
      let func = eval env func
      let args = List.map (eval env) args
      match func with
      | Lambda(env, argNames, rest, body) ->
        let argMap = matchArgs (argNames, rest) args
        let env' = Env.extend argMap env
        eval env' body
      | Prim prim -> prims.[prim] args
      | _ -> failwith "Not a function: %A" func

    eval env0 expr0

