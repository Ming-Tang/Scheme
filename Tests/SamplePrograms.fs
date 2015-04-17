module Scheme.Tests.SamplePrograms

open System
open System.IO
open System.Collections.Generic
open System.IO
open System.Reflection
open Scheme
open Scheme.Eval

type TestFailure =
| CheckErrorFail
| CheckExpectFail of actual: Data Expr * expected: Data Expr
| CheckExpectError of exn

type TestCase = unit -> TestFailure option

let run testName =
  let testCases = new List<TestCase>()

  let checkExpect : EvalRule = fun eval env (ActivePatterns.Args2(a, b)) ->
    testCases.Add <| fun() ->
      try
        let actual = eval env a
        let expected = eval env b
        if actual <> expected then
          Some <| CheckExpectFail(actual, expected)
        else
          None
      with
      | e -> Some <| CheckExpectError e
    Nil

  let checkError : EvalRule = fun eval env (ActivePatterns.Args1(a)) ->
    testCases.Add <| fun() ->
      try
        eval env a |> ignore
        Some CheckErrorFail
      with
      | e -> None
    Nil

  let rules =
    Rules.standardRules
    |> Map.add "check-expect" checkExpect
    |> Map.add "check-error" checkError

  let standardConfig = {
    Primitives = Primitives.standardPrimitives
    EvalRules = rules
  }

  let env = Env.extend Primitives.standardSymbols <| Env.create()

  File.ReadAllText(testName + ".scm")
  |> Types.parse
  |> Begin
  |> eval standardConfig env
  |> ignore

  let results = [ for test in testCases -> test() ]
  let passes, fails = List.partition Option.isNone results

  List.length results,
  passes.Length,
  List.map Option.get fails

let report name (testCount, passCount, fails) =
  let failCount = List.length fails
  if failCount = 0 then
    printfn "%s: %d passed" name passCount
  else
    printfn "%s: %d passed, %d failed" name passCount failCount

  for fail in fails do
    match fail with
    | CheckErrorFail ->
      eprintfn "  check-error failed: Did not encounter error."
    | CheckExpectFail(expected, actual) ->
      eprintfn "  check-expect failed: "
      eprintfn "  Expecting: %A" expected
      eprintfn "  Actual: %A" actual
    | CheckExpectError e ->
      eprintfn "  check-expect failed due to error: "
      eprintfn "%s" e.Message

  fails <> []

[<EntryPoint>]
let main argv =
  if argv.Length = 0 then
    eprintfn "Usage: Tests.exe testName1...testNameN"
    64
  else
    try
      let fails = [
        for program in argv ->
          run program
          |> report program
      ]

      if List.exists id fails then
        1
      else
        0
    with
    | :? FileNotFoundException as e ->
      eprintfn "%s" e.Message
      32

