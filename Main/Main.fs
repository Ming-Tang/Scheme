module Scheme.Main
open System
open System.Collections.Generic
open Scheme
open Scheme.Eval

let standardConfig = {
  Primitives = Primitives.standardPrimitives
  EvalRules = Rules.standardRules
}

[<EntryPoint>]
let main argv =
  let eval = eval standardConfig
  let rec loop env =
    eprintf "> "
    let line = Console.ReadLine()
    if line = null then
      eprintfn ""
    else
      match line.Trim() with
      | null -> ()
      | "#quit" ->
        eprintfn ""
        Environment.Exit(0)
      | "#reset" -> loop (Env.create())
      | cmd when cmd.StartsWith("#") ->
        eprintfn "Unknown command: %A" cmd
        loop env
      | expr ->
        try
          let parsed = Types.parse expr
          let block = ProperList (Sym "begin" :: parsed)
          let result = eval env block
          if result <> Nil then
            printfn "%A" result
        with
        | e ->
          eprintfn "%s" e.Message
          eprintfn "%s" e.StackTrace
        loop env

  eprintfn "Scheme REPL"
  eprintfn "#quit to end session, #reset to clear definitions"
  eprintfn ""

  loop (Env.create())
  0

