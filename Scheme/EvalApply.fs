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
      |  Lambda(_, _, _) -> Some()
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
      | Lambda(env, argNames, body) ->
        if List.length argNames <> List.length args then
          failwithf "Argument count mismatch. Expecting %d but got %d."
            (List.length argNames) (List.length args)
        let args = List.zip argNames args |> Map.ofList
        let env' = Env.extend args env
        eval env' body
      | Prim prim -> prims.[prim] args
      | _ -> failwith "Not a function: %A" func

    eval env0 expr0

