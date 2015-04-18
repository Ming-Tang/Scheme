[<AutoOpen>]
module Scheme.Expr
open Scheme
open System
open System.Collections.Generic
open System.Reflection
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Text.Lexing

/// Match a proper list, which is a cons that eventually leads to a nil
let rec (|ProperList|_|) expr =
  match expr with
  | Nil -> Some []
  | Cons (x, ProperList xs) -> Some (x :: xs)
  | _ -> None

/// Construct a proper list from a list of exprs
let ProperList xs =
  List.foldBack (fun a b -> Cons(a, b)) xs Nil

/// Match an improper list, which is a cons that leads to something
/// other than nil.
let rec (|ImproperList|_|) expr =
  match expr with
  | Cons (x, ImproperList(xs, y)) -> Some(x :: xs, y)
  | Cons (x, y) -> Some([x], y)
  | _ -> None

/// Match anything as a proper or improper list. Something that is
/// not a list is consider improper list with one improper element.
let (|ProperImproper|) expr =
    match expr with
    | ProperList xs -> xs, None
    | ImproperList(xs, y) -> xs, Some y
    | x -> [], Some x

/// Construct an improper list
let ImproperList(xs, y) =
  List.foldBack (fun a b -> Cons(a, b)) xs y

let list = ProperList

/// Decompose a list into proper and improper parts
let decomposeList xs =
  let rec decomposeList' xs ys =
    match xs with
    | Nil -> ys, None
    | Cons(a, xs') -> decomposeList' xs' (ys @ [a])
    | x -> ys, Some x
  decomposeList' xs []

let (|ProperImproperList|) xs =
  decomposeList xs

/// Construct a (begin ...) block from a list of exprs
let Begin xs =
  ProperList (Sym "begin" :: xs)

/// Determine if a value should be considered true or false
/// Only the symbol false is false
let (|IsTrue|IsFalse|) expr =
  match expr with
  | False -> IsFalse
  | _ -> IsTrue

let createBool b =
  if b then True else False

/// Convert any type into an Expr. Tuples become lists
let rec makeExpr (value : obj) =
  let typ = value.GetType()
  match value with
  | null -> Nil
  | :? unit -> Nil
  | :? Expr<_> as e -> e
  | :? bool as b -> if b then True else False
  | :? int as i -> Int i
  | :? float as r -> Real r
  | :? string as s -> Str s
  | _ when FSharpType.IsTuple(typ) ->
    FSharpValue.GetTupleFields(value)
    |> Seq.map makeExpr
    |> List.ofSeq
    |> list
  | _ -> failwith "Cannot convert a %O into an Expr."

/// Create an expression from an SExprView
let rec fromSExprView (v : SExprView) =
  match v with
  | SExprView.NilV -> Nil
  | SExprView.TrueV -> True
  | SExprView.FalseV -> False
  | SExprView.IntV i -> Int i
  | SExprView.RealV r -> Real r
  | SExprView.StrV s -> Str s
  | SExprView.SymV s -> Sym s
  | SExprView.QuoteV qb ->
    ProperList [Sym "quote"; fromSExprView qb]
  | SExprView.QuasiquoteV qqb ->
    ProperList [Sym "quasiquote"; fromSExprView qqb]
  | SExprView.UnquoteV uqb ->
    ProperList [Sym "unquote"; fromSExprView uqb]
  | SExprView.UnquoteSplicingV uqsb ->
    ProperList [Sym "unquote-splicing"; fromSExprView uqsb]
  | SExprView.ProperListV xs ->
    ProperList (List.map fromSExprView xs)
  | SExprView.DottedListV(xs, y) ->
    ImproperList (List.map fromSExprView xs, fromSExprView y)

/// Convert an expression into SExprView
let rec toSExprView expr =
  match expr with
  | Nil -> SExprView.NilV
  | True -> SExprView.TrueV
  | False -> SExprView.FalseV
  | Int i -> SExprView.IntV i
  | Real r -> SExprView.RealV r
  | Str s -> SExprView.StrV s
  | Sym s -> SExprView.SymV s
  | Prim s -> SExprView.SymV (sprintf "#<primitive:%s>" s)
  | Lambda(_, _, _, _) -> SExprView.SymV "#<lambda>"
  | ProperList [Sym "quote"; q] ->
    SExprView.QuoteV (toSExprView q)
  | ProperList [Sym "unquote"; uq] ->
    SExprView.UnquoteV (toSExprView uq)
  | ProperList [Sym "unquote-splicing"; uqs] ->
    SExprView.UnquoteSplicingV (toSExprView uqs)
  | ProperList [Sym "quasiquote"; qq] ->
    SExprView.QuasiquoteV (toSExprView qq)
  | ProperList xs ->
    SExprView.ProperListV (List.map toSExprView xs)
  | ImproperList(xs, y) ->
    SExprView.DottedListV (List.map toSExprView xs, toSExprView y)
  | Cons(_, _) -> failwith "Impossible case: A list either proper or improper."

/// Format an expression into a string
let format expr = sprintf "%A" (toSExprView expr)

/// Convert an expression from one form (code or data) to another
let rec convert<'A, 'B when 'A :> CodeOrData
                       and 'B :> CodeOrData> expr : 'B Expr =
  match expr : 'A Expr with
  | Nil -> Nil
  | True -> True
  | False -> False
  | Int i -> Int i
  | Real f -> Real f
  | Str s -> Str s
  | Sym s -> Sym s
  | Prim p -> Prim p
  | Cons(a, b) -> Cons(convert<'A, 'B> a, convert<'A, 'B> b)
  | Lambda(e, a, d, b) -> Lambda(e, a, d, b)

/// Convert a Code Expr into a Data Expr
let inline codeToData expr = convert<Code, Data> expr

/// Convert a Data Expr into a Code Expr
let inline dataToCode expr = convert<Data, Code> expr

/// Parse a sequence of S-expressions
let parse str =
  str
  |> sprintf "(begin\n%s\n)"
  |> LexBuffer<char>.FromString
  |> Parser.start Lexer.read
  |> List.map fromSExprView

