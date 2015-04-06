module Scheme.Main
open System
open System.Collections.Generic
open Scheme
open Scheme.Eval

let standardConfig = {
  Primitives = Primitives.standardPrimitives
  EvalRules = Rules.standardRules
}

let private L = ProperList
let private S = Str
let private I = Int
let private R = Real
let private s = Sym

let arithmetic =
  makeExpr (s"+", 1, 3,
            (s"-", 10, (s"/", 5, 2.0)))

let logicConditional =
  makeExpr (s"if", (s"and", true, (s"or", false, (s"not", false))),
                   "success", (s"/", 1, 0))

let factorial =
  makeExpr (s"begin", (s"define", (s"f", s"x"),
                         (s"if", (s"=", s"x", 0),
                                 1,
                                 (s"*", s"x", (s"f", (s"-", s"x", 1))))),
                      (s"f", 8))

let fibTailRec =
  makeExpr (s"begin",
            (s"define", s"nil", Nil),
            (s"define", (s"f", s"x"),
              (s"define", (s"f1", s"a", s"b", s"n"),
                (s"if", (s"=", s"n", 0),
                        s"a",
                        (s"f1", s"b", (s"+", s"a", s"b"),
                                      (s"-", s"n", 1)))),
              (s"f1", 1, 1, s"x")),
            (s"define", (s"table", s"f", s"n"),
              (s"define", (s"count", s"i"),
                (s"if", (s"=", s"i", s"n"),
                        s"nil",
                        (s"cons", (s"f", s"i"),
                                  (s"count", (s"+", s"i", 1))))),
              (s"count", 0)),
            (s"table", s"f", 20))

let mutableVariablesAndScoping =
  makeExpr (s"begin",
            (s"define", s"x", 1),
            (s"cons",
             ((s"lambda", (s"x", s"y"),
               (s"set!", s"x", (s"+", s"x", s"y")),
               s"x"),
              10, 5),
              s"x"))

let quotesAndQuasiquotes =
  makeExpr (s"begin",
            (s"define", s"x", (s"quote",
                               ((s"a", s"/", 1, 0),
                                s"c",
                                (s"d", s"e", s"f")))),
            (s"define", s"y", (s"quasiquote",
                               (s"z", (s"a", (s"unquote", s"x"))))),
            (s"define", (s"f", s"x"),
              (s"set!", s"y", (s"quasiquote",
                               (s"y", (s"unquote", (s"cdr",
                                                    (s"car", s"x"))))))),
            (s"f", s"x"),
            s"y")

//[<EntryPoint>]
let main0 argv =
  [arithmetic; logicConditional; factorial; fibTailRec;
   mutableVariablesAndScoping; quotesAndQuasiquotes]
  |> Seq.iter (fun expr ->
    printfn "%A" expr
    expr
    |> eval standardConfig (Env.create())
    |> printfn "%A"
    printfn "---------")

  0

open Microsoft.FSharp.Text.Lexing

[<EntryPoint>]
let main argv =
  printfn "%A" <| parse argv.[0]
  0
