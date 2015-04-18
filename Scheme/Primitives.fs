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

let inline numericUnOp' intOp realOp = fun (Args1(x)) ->
  match x with
  | Int i -> intOp i
  | Real r -> realOp r
  | x -> failwith "Non-numeric argument: %s" (Expr.format x)

let inline numericUnOp intOp realOp =
  numericUnOp' (intOp >> Int) (realOp >> Real)

/// Require two or more arguments
let req2OrMore (Args2OrMore xs) = xs

/// Chain comparison
let chainOp bin =
  fun (Args2OrMore xs) ->
    Seq.windowed 2 xs
    |> Seq.forall (fun ab -> bin ab.[0] ab.[1])
    |> createBool

/// Binary operator of integers only
let intBinOp f = fun (Args2(IntOnly a, IntOnly b)) -> f a b |> Int

/// A rounding operation (round reals into ints)
let roundOp f = numericUnOp' id (f >> int) >> Int

/// An operation that act on reals and return reals
let realOp f = numericUnOp' (float >> f) f >> Real

let add : Prim = List.fold (numericOp (+) (+)) (Int 0)
let sub : Prim = List.reduce (numericOp (-) (-))
let mul : Prim = List.fold (numericOp ( * ) ( * )) (Int 1)
let div : Prim = List.reduce (numericOp (/) (/))
let quotient : Prim = intBinOp (/)
let remainder : Prim = intBinOp (%)
let modulo : Prim = intBinOp (fun n m -> ((n % m) + m) % m)

let numEq : Prim = chainOp (numericOp' (=) (=))
let numLess : Prim = chainOp (numericOp' (<) (<))
let numGreater : Prim = chainOp (numericOp' (>) (>))
let numLessEq : Prim = chainOp (numericOp' (<=) (<=))
let numGreaterEq : Prim = chainOp (numericOp' (>=) (>=))

let abs' : Prim = numericUnOp abs abs
let floor' : Prim = roundOp floor
let ceiling : Prim = roundOp ceil
let truncate' : Prim = roundOp truncate
let round' : Prim = roundOp round

let exp' : Prim = realOp exp
let sin' : Prim = realOp sin
let cos' : Prim = realOp cos
let tan' : Prim = realOp tan
let asin' : Prim = realOp asin
let acos' : Prim = realOp acos
let atan' : Prim = fun args ->
  match args with
  | [_] -> numericUnOp' (float >> atan >> Real) (atan >> Real) args
  | [a; b] -> numericOp' (fun a b -> atan2 (float a) (float b))
                         (fun a b -> atan2 a b) a b |> Real
  | _ -> failwith "atan2: Need one or two numeric arguments"
let sqrt' : Prim = realOp sqrt
let expt : Prim = fun (Args2(a, b)) ->
  match a, b with
  | Real r, Int i -> pown r i |> Real
  | Int b, Int i -> pown b i |> Int
  | Int i, Real r -> (float i) ** r |> Real
  | Real r, Real p -> r ** p |> Real
  | _, _ -> failwith "expt: Expecting two numeric arguments"


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

    "quotient", quotient
    "remainder", remainder
    "modulo", modulo

    "abs", abs'
    "floor", floor'
    "ceiling", ceiling
    "truncate", truncate'
    "round", round'

    "exp", exp'
    "sin", sin'
    "cos", cos'
    "tan", tan'
    "asin", asin'
    "acos", acos'
    "atan", atan'
    "sqrt", sqrt'
    "expt", expt

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

