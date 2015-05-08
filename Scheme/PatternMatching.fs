/// Parser for the syntax-rules pattern matching language
module Scheme.PatternMatching
open Scheme
open Scheme.Expr
open System

[<AutoOpen>]
/// Module for the <c>option</c> computation expression
module OptionBuilder =
  type OptionBuilder() =
    member inline o.Bind(x, f) = Option.bind f x
    member inline o.Return(x) = Some(x)
    member inline o.ReturnFrom(x) = x

  let option = OptionBuilder()
  let guard p = if p then Some() else None

/// A define-syntax pattern
type 'CorD Pattern when 'CorD :> CodeOrData =
| Variable of varName: string
| Literal of 'CorD Expr
| Proper of 'CorD Pattern list
| Improper of proper: 'CorD Pattern list * improper: 'CorD Pattern
| Ellipsis of proper: 'CorD Pattern list * binding: string

/// Match if a list of expressions end with an ellipsis pattern
let (|WithEllipsis|_|) xs =
  let rec withEllipsis' xs acc =
    match xs with
    | [Sym x; Sym "..."] -> Some(acc, x)
    | [_; _] | [_] | [] -> None
    | x :: xs -> withEllipsis' xs (acc @ [x])
  withEllipsis' xs []

/// Parse a Scheme expression into a pattern
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
  | Int _ | Real _ | Str _ -> Literal expr
  | _ -> failwith "Invalid pattern expression."

/// Match an expression against a pattern. Returns None if pattern matching
/// fails, or Some of a map of bindings of pattern matching succeeds
let matchPattern pat expr =

  /// Add a new binding. Raises an exception if a binding on the specified
  /// variable is already present
  let addBinding k v m =
    if Map.containsKey k m then
      failwithf "Cannot bind variable %s twice." k
    else
      Map.add k v m

  let mergeBindings a b =
    Map.fold (fun m k v -> addBinding k v m) a b

  let isNonEmptyProperList xs =
    match xs with
    | ProperList (_ :: _) -> true
    | _ -> false

  /// Match a pattern against an expression
  let rec matchPattern pat expr =
    match pat, expr with
    | Variable v, expr -> Some (Map.ofList [v, expr])
    | Literal x, expr when x = expr -> Some Map.empty
    | Literal _, _ -> None
    | Proper xs, expr -> matchProper xs expr
    | Improper(xs, y), expr -> matchImproper (xs, y) expr
    | Ellipsis(xs, e), expr -> matchEllipsis (xs, e) expr

  /// Match a list of patterns against an expression one-by-one. If the
  /// expression does not have enough elements compared to the list of
  /// patterns, the matching fails. If the list of patterns ran out,
  /// return rest of the expression
  and matchList (pats : _ Pattern list) expr =
    match pats, expr with
    | p :: ps, Cons(x, xs) ->
      option {
        let! b = matchPattern p x
        let! br, rest = matchList ps xs
        return mergeBindings b br, rest
      }
    | p :: ps, _ -> None
    | [], y -> Some (Map.empty, y)

  /// Match a proper list pattern against an expression. Requires their lengths
  /// to be the same in order to succeed
  and matchProper xs expr =
    option {
      let! b, r = matchList xs expr
      do! guard (r = Nil)
      return b
    }

  /// Match an improper list pattern against an expression. Requires the list
  /// to be long enough for the proper part in order to succeed.
  and matchImproper (xs, y) expr =
    option {
      let! b, r = matchList xs expr
      let! bi = matchPattern y r
      return mergeBindings b bi
    }

  /// Match an ellipsis pattern against an expression. Requires the expression
  /// to be a proper list to be at least one longer than the list part of
  /// the ellipsis pattern in order to succeed
  and matchEllipsis (xs, e) expr =
    option {
      let! b, r = matchList xs expr
      do! guard (isNonEmptyProperList r)
      let! bi = matchPattern (Variable e) r
      return mergeBindings b bi
    }

  matchPattern pat expr

/// Given a list of (pattern, result) pairs, return the first pattern that matches
/// the expression, and the bindings from the pattern
let rec patternCase expr cases =
  match cases with
  | (pat, result) :: cases ->
    match matchPattern pat expr with
    | Some b -> Some((result : 'c), b)
    | None -> patternCase expr cases
  | [] -> None

/// Given a list of (pattern, function) pairs and another function, call the function
/// for the first pattern that match the expression. Or else call the else-function
let patternCont expr patternConts elseCont =
  match patternCase expr patternConts with
  | Some(c, b) -> c b
  | _ -> elseCont()

