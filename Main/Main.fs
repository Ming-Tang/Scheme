module Scheme.Main
open System
open System.IO
open System.Collections.Generic
open Scheme
open Scheme.Eval
open Scheme.Standard
open Microsoft.FSharp.Text.Lexing

let readAndParse() =
  LexBuffer<char>.FromTextReader Console.In
  |> Parser.start Lexer.read
  |> List.map fromSExprView

[<EntryPoint>]
let main argv =
  let eval = Standard.eval
  let rec loop env =
    try
      let parsed = readAndParse()
      let block = Begin parsed
      let result = eval env block
      if result <> Nil then
        printfn "%s" (Expr.format result)
      loop env
    with
    | e ->
      printfn "%s" e.Message
      loop env

  loop (Standard.createEnv())
  0
