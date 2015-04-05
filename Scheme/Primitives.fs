module Scheme.Primitives
open System;
open System.Collections.Generic;
open Scheme
open Scheme.ActivePatterns

let inline numericOp intOp realOp =
  fun a b ->
    match a, b with
    | Real a, Real b -> realOp a b |> Real
    | Int a, Real b -> realOp (float a) b |> Real
    | Real a, Int b -> realOp a (float b) |> Real
    | Int a, Int b -> intOp a b |> Int
    | (Real _ | Int _), x
    | x, _ -> failwithf "Non-numeric argument found: %A" x

let inline numeric init intOp realOp : Prim =
  let op = numericOp intOp realOp
  List.fold op init

let add : Prim = List.fold (numericOp (+) (+)) (Int 0)
let sub : Prim = List.reduce (numericOp (-) (-))
let mul : Prim = List.fold (numericOp ( * ) ( * )) (Int 1)
let div : Prim = List.reduce (numericOp (/) (/))

let eq : Prim = fun (Args2(a, b)) -> createBool (a = b)
let ne : Prim = fun (Args2(a, b)) -> createBool (a <> b)

let shortCircuit init op =
  let rec reduce args =
    match args with
    | [] -> init
    | [x; y] -> op x (fun() -> y)
    | x :: xs -> op x (fun() -> reduce xs)
  reduce

let not' : Prim = fun (Args1 x) ->
  match x with
  | IsTrue -> False
  | IsFalse -> True

let and' : Prim = shortCircuit False (fun a b ->
  match a with
  | IsTrue -> b()
  | IsFalse -> False)

let or' : Prim = shortCircuit True (fun a b ->
  match a with
  | IsTrue -> True
  | IsFalse -> b())

let cons : Prim = fun (Args2(hd, tl)) ->
  Cons(hd, tl)

let car : Prim = fun (Args1(x)) ->
  match x with
  | Cons(a, _) -> a
  | _ -> failwith "car: Not a cons."

let cdr : Prim = fun (Args1(x)) ->
  match x with
  | Cons(_, b) -> b
  | _ -> failwith "car: Not a cons."

let standardPrimitives : Primitives =
  Map.ofList [
    "+", add
    "-", sub
    "*", mul
    "/", div
    "=", eq
    "!=", ne

    "and", and'
    "or", or'
    "not", not'

    "cons", cons
    "car", car
    "cdr", cdr
  ]
