module Scheme.Primitives
open System
open System.Collections.Generic
open System.IO
open System.Reflection
open Scheme
open Scheme.ActivePatterns

let inline numericOp' intOp realOp =
  fun a b ->
    match a, b with
    | Real a, Real b -> realOp a b
    | Int a, Real b -> realOp (float a) b
    | Real a, Int b -> realOp a (float b)
    | Int a, Int b -> intOp a b
    | (Real _ | Int _), x
    | x, _ -> failwithf "Non-numeric argument found: %s" (Expr.format x)

let inline numericOp intOp realOp =
  numericOp' (fun a b -> intOp a b |> Int)
             (fun a b -> realOp a b |> Real)

let inline numericOpB intOp realOp =
  numericOp' (fun a b -> intOp a b |> createBool)
             (fun a b -> realOp a b |> createBool)

let numEqBinary =
  fun a b ->
    match a, b with
    | Real a, Real b -> a = b |> createBool
    | Int a, Real b | Real b, Int a -> (float a) = b |> createBool
    | Int a, Int b -> a = b |> createBool
    | (Real _ | Int _), x
    | x, _ -> failwithf "Non-numeric argument found: %s" (Expr.format x)

let inline numeric init intOp realOp : Prim =
  let op = numericOp intOp realOp
  List.fold op init

/// Require two or more arguments
let req2OrMore (Args2OrMore xs) = xs

let chainOp bin =
  fun (Args2OrMore xs) ->
    Seq.windowed 2 xs
    |> Seq.forall (fun ab -> bin ab.[0] ab.[1])
    |> createBool

let add : Prim = List.fold (numericOp (+) (+)) (Int 0)
let sub : Prim = List.reduce (numericOp (-) (-))
let mul : Prim = List.fold (numericOp ( * ) ( * )) (Int 1)
let div : Prim = List.reduce (numericOp (/) (/))
let numEq : Prim = chainOp (numericOp' (=) (=))
let numLess : Prim = chainOp (numericOp' (<) (<))
let numGreater : Prim = chainOp (numericOp' (>) (>))
let numLessEq : Prim = chainOp (numericOp' (<=) (<=))
let numGreaterEq : Prim = chainOp (numericOp' (>=) (>=))

let stringLength : Prim = fun (Args1(StrOnly s)) -> Int s.Length
let stringRef : Prim = fun (Args2(StrOnly s, IntOnly i)) -> Str (string s.[i])
let stringAppend : Prim = List.fold (fun s (StrOnly t) -> s + t) "" >> Str
let substring : Prim = fun (Args3(StrOnly s, IntOnly m, IntOnly n)) ->
  s.Substring(m, n - m) |> Str

let not' : Prim = fun (Args1 x) ->
  match x with
  | IsTrue -> False
  | IsFalse -> True

let cons : Prim = fun (Args2(hd, tl)) ->
  Cons(hd, tl)

let car : Prim = fun (Args1 x) ->
  match x with
  | Cons(a, _) -> a
  | _ -> failwith "car: Not a cons."

let cdr : Prim = fun (Args1 x) ->
  match x with
  | Cons(_, b) -> b
  | _ -> failwith "car: Not a cons."

let list : Prim = ProperList

let equals : Prim = fun (Args2(a, b)) -> createBool (a = b)

let error : Prim = fun xs ->
  let join = List.map Expr.format >> String.concat " "
  match xs with
  | [Sym s; Str msg] ->
    failwith "%s: %" s msg
  | [Str msg] ->
    failwith msg
  | _ -> failwith "error"

let isNumber : Prim = fun (Args1 x) ->
  match x with
  | Real _ | Int _ -> True
  | _ -> False

let isReal : Prim = fun (Args1 x) ->
  match x with
  | Real _  -> True
  | _ -> False

let isInteger : Prim = fun (Args1 x) ->
  match x with
  | Int _ -> True
  | _ -> False

let isZero : Prim = fun (Args1 x) ->
  match x with
  | Int 0 | Real 0.0 -> True
  | _ -> False

let isBoolean : Prim = fun (Args1 x) ->
  match x with
  | True | False -> True
  | _ -> False

let isList : Prim = fun (Args1 x) ->
  match x with
  | ProperList _ -> True
  | _ -> False

let isPair : Prim = fun (Args1 x) ->
  match x with
  | Cons(_, _) -> True
  | _ -> False

let isNil : Prim = fun (Args1 x) ->
  match x with
  | Nil -> True
  | _ -> False

let isString : Prim = fun (Args1 x) ->
  match x with
  | Str _ -> True
  | _ -> False

let isSymbol : Prim = fun (Args1 x) ->
  match x with
  | Sym _ -> True
  | _ -> False

let isLambda : Prim = fun (Args1 x) ->
  match x with
  | Prim _ | Lambda(_, _, _, _) -> True
  | _ -> False

let standardPrimitives : Primitives =
  Map.ofList [
    "+", add
    "-", sub
    "*", mul
    "/", div
    "=", numEq
    "<", numLess
    ">", numGreater
    "<=", numLessEq
    ">=", numGreaterEq

    "string-length", stringLength
    "string-ref", stringRef
    "string-append", stringAppend
    "substring", substring

    "false?", not'
    "not", not'

    "cons", cons
    "car", car
    "cdr", cdr
    "first", car
    "rest", cdr
    "list", list

    "error", error

    "equal?", equals
    "eqv?", equals
    "eq?", equals
    "boolean?", isBoolean
    "number?", isNumber
    "real?", isReal
    "integer?", isInteger
    "zero?", isZero
    "list?", isList
    "pair?", isPair
    "cons?", isPair
    "empty?", isNil
    "nil?", isNil
    "null?", isNil
    "string?", isString
    "symbol?", isSymbol
    "lambda?", isLambda
    "proc?", isLambda
  ]

let standardSymbols : SymbolTable =
  let env = Env.create()
  let config = {
    Config.Primitives = standardPrimitives
    EvalRules = Rules.standardRules
  }

  use sr = new StreamReader(Assembly.GetExecutingAssembly()
                                    .GetManifestResourceStream("standard.scm"))

  sr.ReadToEnd()
  |> Expr.parse
  |> Begin
  |> Eval.eval config env
  |> ignore

  env.Symbols

