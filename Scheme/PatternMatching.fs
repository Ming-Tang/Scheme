/// Parser for the syntax-rules pattern matching language
module Scheme.PatternMatching
open Scheme
open System

type Pattern =
| Variable of varName: string
| Literal of Data Expr
| Proper of Pattern list
| Improper of proper: Pattern list * improper: Pattern
| Ellipsis of proper: Pattern list * binding: string

let rec parsePattern syms expr =
  let parsePattern' = parsePattern syms
  match expr with
  | ProperList (WithEllipsis(xs, e)) ->
    Ellipsis(List.map parsePattern' xs, e)
  | ProperList xs ->
    Proper(List.map parsePattern' xs)
  | ImproperList(xs, y) ->
    Improper(List.map parsePattern' xs, parsePattern' y)
  | Sym sym when Set.contains sym syms -> Literal expr
  | Sym var -> Variable var
  | _ -> failwith "Invalid pattern expression."

/// Matches an expression against a pattern, which a set of symbols treated
/// literally (rather than as bindings). Returns None if pattern matching
/// fails, or Some of a map of bindings of pattern matching succeeds
let matchPattern syms pat expr =
  None

