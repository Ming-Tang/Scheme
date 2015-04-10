module Scheme.ActivePatterns
open System;
open System.Collections.Generic;
open Scheme

let private expectingArgs n args =
  failwithf "Expecting %d args. Got %d args." n (List.length args)

let private expectingMoreArgs n args =
  failwithf "Expecting %d or more args. Got %d args." n (List.length args)

let (|Args0|) args =
  match args with
  | [] -> ()
  | _ -> expectingArgs 0 args

let (|Args1|) args =
  match args with
  | [a] -> a
  | _ -> expectingArgs 1 args

let (|Args2|) args =
  match args with
  | [a; b] -> a, b
  | _ -> expectingArgs 2 args

let (|Args3|) args =
  match args with
  | [a; b; c] -> a, b, c
  | _ -> expectingArgs 3 args

let (|Args4|) args =
  match args with
  | [a; b; c; d] -> a, b, c, d
  | _ -> expectingArgs 4 args

let (|Args2OrMore|) args =
  match args with
  | [] | [_] -> expectingMoreArgs 2 args
  | xs -> xs

let (|ConsOnly|) args =
  match args with
  | [] -> failwith "Expecting an non-empty list"
  | x :: xs -> x, xs

let (|SymList|) arg =
  let error() = failwithf "Not a list of symbols: %A" arg
  match arg with
  | ProperList ss ->
    ss
    |> List.map (fun s ->
      match s with
      | Sym s -> s
      | _ -> error())
  | _ -> error()

let (|Eval|) (eval : Eval) env expr =
  eval env expr

let (|SymOnly|) arg =
  match arg with
  | Sym s -> s
  | _ -> failwith "Expecting a symbol."

let (|Body|) args =
  match args with
  | [] -> failwith "Body cannot be empty."
  | [a] -> a
  | _ -> list (Sym "begin" :: args)
