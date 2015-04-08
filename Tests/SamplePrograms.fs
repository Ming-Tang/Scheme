module Scheme.Tests.SamplePrograms

open System
open NUnit.Framework
open FsUnit.TopLevelOperators

[<TestFixture>]
type Test() =
  [<Test>]
  member x.TestCase() =
    1 + 1
    |> should equal 2

