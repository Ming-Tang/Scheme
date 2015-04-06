[<AutoOpen>]
module Scheme.Types
open Scheme
open System;
open System.Collections.Generic;
open System.Reflection
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Text.Lexing

/// An S-expression. Represents a Scheme program or a value
[<StructuredFormatDisplay("{AsSExprView}")>]
type Expr =
| Nil
| True
| False
| Int of int
| Real of float
| Str of string
| Sym of string
| Cons of Expr * Expr
| Lambda of env: Env * argList: string list * body: Expr
/// An environment contains a symbol table, and a pointer to parent scope
and [<ReferenceEquality>] Env = {
  Symbols : SymbolTable
  Parent : Env option
}
/// A mutable lookup table of variables
and SymbolTable = IDictionary<string, Expr>

/// Type of the eval function
type Eval = Env -> Expr -> Expr

/// An evaluation rule takes an eval procedure, an environment and an
/// expression, and evaluates it, returning the evaluated expression
type EvalRule = Eval -> Env -> Expr list -> Expr

/// A lookup table of evaluation rules. The key is the name of the rule (
/// matches based on the first symbol of a list, and the value is a function
/// that takes the expression's arguments, its enviroment, and returns
/// the result.
type EvalRules = Map<string, EvalRule>

/// A primitive function
type Prim = Expr list -> Expr

/// A lookup of primitives
type Primitives = Map<string, Prim>

/// An evaluator configuration, which holds a lookup of primitives,
/// a set of special symbols, and a lookup of evaluation rules
type Config = { Primitives : Primitives
                EvalRules : EvalRules }

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
/// other than nil
let rec (|ImproperList|_|) expr =
  match expr with
  | Cons (x, ImproperList(xs, y)) -> Some(x :: xs, y)
  | Cons (x, y) -> Some([x], y)
  | _ -> None

/// Construct an improper list
let ImproperList xs y =
  List.foldBack (fun a b -> Cons(a, b)) xs y

let list = ProperList

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
  | :? Expr as e -> e
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

let rec fromSExprView (v : Parser.SExprView) =
  match v with
  | Parser.NilV -> Nil
  | Parser.TrueV -> True
  | Parser.FalseV -> False
  | Parser.IntV i -> Int i
  | Parser.RealV r -> Real r
  | Parser.StrV s -> Str s
  | Parser.SymV s -> Sym s
  | Parser.QuoteV qb -> ProperList [Sym "quote"; fromSExprView qb]
  | Parser.QuasiquoteV qqb -> ProperList [Sym "quasiquote"; fromSExprView qqb]
  | Parser.UnquoteV qb -> ProperList [Sym "unquote"; fromSExprView qb]
  | Parser.ProperListV xs -> ProperList (List.map fromSExprView xs)
  | Parser.DottedListV(xs, y) ->
    ImproperList (List.map fromSExprView xs) (fromSExprView y)

let rec toSExprView expr =
  match expr with
  | Nil -> Parser.NilV
  | True -> Parser.TrueV
  | False -> Parser.FalseV
  | Int i -> Parser.IntV i
  | Real r -> Parser.RealV r
  | Str s -> Parser.StrV s
  | Sym s -> Parser.SymV s
  | ProperList [Sym "quote"; q] -> Parser.QuoteV (toSExprView q)
  | ProperList [Sym "unquote"; uq] -> Parser.QuoteV (toSExprView uq)
  | ProperList [Sym "quasiquote"; qq] -> Parser.QuoteV (toSExprView qq)
  | ProperList xs -> Parser.ProperListV (List.map toSExprView xs)
  | ImproperList(xs, y) ->
    Parser.DottedListV (List.map toSExprView xs, toSExprView y)
  | Cons(_, _) -> failwith "Impossible case: A list either proper or improper."
  | Lambda(_, _, _) -> Parser.SymV "#<lambda>"

/// Parse a sequence of S-expressions
let parse str =
  LexBuffer<char>.FromString str
  |> Parser.start Lexer.read
  |> List.map fromSExprView

type Expr with
  member e.AsSExprView = toSExprView e

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Env =
  /// Create a new environment with one scope and no variables
  let create() =
    { Env.Symbols = Dictionary()
      Parent = None }

  /// Find the innermost scope that contains the name, None if not found
  let rec find name ({ Env.Symbols = symbols; Parent = parent } as env) =
    let found, value = symbols.TryGetValue(name)
    if found then
      Some env
    else
      match parent with
      | Some parent -> find name parent
      | None -> None

  /// Lookup the value of a symbol, None if not found
  let rec lookup name env =
    match find name env with
    | Some env -> Some env.Symbols.[name]
    | None -> None

  /// Assign the variable in the most innermost scope of an environment
  let var name value { Env.Symbols = symbols; Parent = parent } =
    symbols.[name] <- value

  /// Set the value of a variable if it's found in a scope. If not found, set
  /// it in the innermos scope
  let set name value env =
    match find name env with
    | Some env -> env.Symbols.[name] <- value
    | None -> env.Symbols.[name] <- value

  /// Delete a variable from an environment in the first innermost scope that
  /// contains it, and return true. If variable not found, return false.
  let delete name env =
    match find name env with
    | Some env -> env.Symbols.Remove(name)
    | None -> false

  /// Extend an environment by adding another innermost scope
  let extend (symbols : #SymbolTable) env =
    { Env.Symbols = Dictionary(dictionary=symbols)
      Parent = Some env }
