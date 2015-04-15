module Scheme.Tests.SamplePrograms

open System
open System.IO
open System.Collections.Generic
open NUnit.Framework
open FsUnit.TopLevelOperators
open Scheme
open Scheme.Eval

[<TestFixture>]
type TestSamplePrograms() =
  let run testName =
    let tests = new List<unit -> unit>()
    let results = new List<string option>()

    let checkExpect : EvalRule = fun eval env (ActivePatterns.Args2(a, b)) ->
      tests.Add <| fun() ->
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

    let checkError : EvalRule = fun eval env (ActivePatterns.Args1(a)) ->
      tests.Add <| fun() ->
        try
          eval env a |> ignore
          results.Add(Some "check-error: No error was raised.")
        with
        | e -> results.Add(None)
      Nil

    let rules =
      seq {
        yield! Seq.map (fun (KeyValue(k, v)) -> k, v) Rules.standardRules
        yield "check-expect", checkExpect
        yield "check-error", checkError
      }
      |> Map.ofSeq

    let standardConfig = {
      Primitives = Primitives.standardPrimitives
      EvalRules = rules
    }

    let path = testName + ".scm"
    let text = File.ReadAllText(path)
    let expr = parse text
    let env = Env.extend Primitives.standardSymbols <| Env.create()

    Begin expr
    |> eval standardConfig env
    |> ignore

    for t in tests do t()

    let passes, fails = List.partition Option.isNone <| List.ofSeq results
    if not (Seq.isEmpty fails) then
      let nPasses = List.length passes
      let nFails = List.length fails

      fails
      |> Seq.map (Option.get >> sprintf "- %s")
      |> String.concat "\n"
      |> failwithf "%d passed, %d failed, %d total\n%s"
                   nPasses nFails (nPasses + nFails)

  [<Test>]
  member x.TestChurchEncoding() = run "church"

  [<Test>]
  member x.TestFunctionArguments() = run "functionArguments"

  [<Test>]
  member x.TestMcons() = run "mcons"

