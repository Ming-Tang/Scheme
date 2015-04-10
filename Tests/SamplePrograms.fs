﻿module Scheme.Tests.SamplePrograms

open System
open System.IO
open System.Collections.Generic
open NUnit.Framework
open FsUnit.TopLevelOperators
open Scheme
open Scheme.Eval

[<TestFixture>]
type TestSamplePrograms() =
  [<Test>]
  member x.TestChurchEncoding() =
    let results = new List<string option>()

    let checkExpect : EvalRule = fun eval env (ActivePatterns.Args2(a, b)) ->
      try
        let a = eval env a
        let b = eval env b
        if a <> b then
          sprintf "check-expect failed.\nExpected: %A\nActual: %A" a b
          |> Some
          |> results.Add
        else
          results.Add(None)
      with
      | e -> results.Add(Some (sprintf "%A" e))
      Nil

    let standardConfig = {
      Primitives = Primitives.standardPrimitives
      EvalRules = Map.add "check-expect" checkExpect Rules.standardRules
    }

    // for path in Directory.EnumerateFiles(".") do
    let path = "church.scm"
    let text = File.ReadAllText(path)
    let expr = parse text
    let env = Env.create()

    ProperList (Sym "begin" :: expr)
    |> eval standardConfig env
    |> ignore

    for result in results do
      match result with
      | None -> ()
      | Some msg -> failwith msg
