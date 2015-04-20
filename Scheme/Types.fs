[<AutoOpen>]
module Scheme.Types
open Scheme
open System
open System.Collections.Generic
open System.Reflection
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Text.Lexing

type CodeOrData = interface end
/// Expression represents code
type Code = Code with interface CodeOrData
/// Expression represents data
type Data = Data with interface CodeOrData

/// An S-expression. Represents a Scheme program or a value
type 'CorD Expr when 'CorD :> CodeOrData =
| Nil
| True
| False
| Int of int
| Real of float
| Str of string
| Sym of string
| Prim of string
| Cons of Expr<'CorD> * Expr<'CorD>
| Lambda of env: Env * args: string list * dot: string option * body: Code Expr
/// An environment contains a symbol table, and a pointer to parent scope
and [<ReferenceEquality>] Env = {
  Symbols : SymbolTable
  Parent : Env option
}
/// A mutable lookup table of variables
and SymbolTable = IDictionary<string, Data Expr>

/// Type of the eval function
type Eval = Env -> Code Expr -> Data Expr

/// An evaluation rule takes an eval procedure, an environment and an
/// expression, and evaluates it, returning the evaluated expression
type EvalRule = Eval -> Env -> Code Expr list -> Data Expr

/// A lookup table of evaluation rules. The key is the name of the rule (
/// matches based on the first symbol of a list, and the value is a function
/// that takes the expression's arguments, its enviroment, and returns
/// the result.
type EvalRules = Map<string, EvalRule>

/// A primitive function
type Prim = Data Expr list -> Data Expr

/// A lookup of primitives
type Primitives = Map<string, Prim>

/// An evaluator configuration, which holds a lookup of primitives,
/// a set of special symbols, and a lookup of evaluation rules
type Config = { Primitives : Primitives
                EvalRules : EvalRules }

