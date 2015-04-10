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
  [<Test>]
  member x.TestChurchEncoding() =
    let tests = new List<unit -> unit>()
    let results = new List<string option>()

    // TODO register tests, run tests last
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

    let standardConfig = {
      Primitives = Primitives.standardPrimitives
      EvalRules = Map.add "check-expect" checkExpect Rules.standardRules
    }

    // for path in Directory.EnumerateFiles(".") do
    let path = "church.scm"
    let text = File.ReadAllText(path)
    let expr = parse text
    let env = Env.extend Primitives.standardSymbols <| Env.create()

    ProperList (Sym "begin" :: expr)
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

