module Scheme.Main
open System
open System.IO
open System.Collections.Generic
open Scheme
open Scheme.Eval
open Scheme.Standard
open Microsoft.FSharp.Text.Lexing

[<EntryPoint>]
let main argv =
  use stream = Console.OpenStandardInput()
  use br = new StreamReader(stream)

  let readAndParse() =
    LexBuffer<char>.FromTextReader br
    |> Parser.start Lexer.read
    |> List.map fromSExprView

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
