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
| CheckWithinFail of actual : float * lo : float * hi : float
| CheckWithinError of exn

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

  let checkRange : EvalRule = fun eval env (ActivePatterns.Args3(a, x, y)) ->
    testCases.Add <| fun() ->
      try
        let actual = eval env a
        let lo = eval env x
        let hi = eval env y
        match actual, lo, hi with
        | Real a, Real x, Real y ->
          if x <= a && a <= y then None
          else Some <| CheckWithinFail(a, x, y)
        | Real _, _, _ ->
          failwith "Range must be real numbers."
        | _, _, _ ->
          failwith "Actual value is not a number, but is %s" (Expr.format actual)
      with
      | e -> Some <| CheckExpectError e
    Nil

  let checkWithin : EvalRule = Rules.translation <| fun (ActivePatterns.Args3(a, x, y)) ->
    list [Sym "check-range"; a; list [Sym "-"; x; y]; list [Sym "+"; x; y] ]

  let rules =
    Rules.standardRules
    |> Map.add "check-expect" checkExpect
    |> Map.add "check-error" checkError
    |> Map.add "check-within" checkWithin
    |> Map.add "check-range" checkRange

  let standardConfig = {
    Primitives = Primitives.standardPrimitives
    EvalRules = rules
  }

  let env = Env.extend Primitives.standardSymbols <| Env.create()

  File.ReadAllText(testName + ".scm")
  |> Expr.parse
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
    | CheckExpectFail(actual, expected) ->
      eprintfn " check-expect failed: "
      eprintfn "  Expecting: %s" (Expr.format expected)
      eprintfn "  Actual: %s" (Expr.format actual)
    | CheckExpectError e ->
      eprintfn " check-expect failed due to error: "
      eprintfn "    %s" e.Message
    | CheckWithinFail(actual, lo, hi) ->
      eprintfn " check-within/check-range failed: "
      eprintfn "  Expecting range [%f, %f], delta=%f" lo hi (hi - lo)
      eprintfn "  Actual: %f" actual
    | CheckWithinError(e) ->
      eprintfn "  check-within failed due to error: "
      eprintfn "    %s" e.Message

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

