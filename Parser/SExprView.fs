[<AutoOpen>]
module Scheme.SExprView

/// Parse tree of an S-expression
[<AutoOpen>]
[<StructuredFormatDisplay("{Format}")>]
type SExprView =
| NilV
| FalseV
| TrueV
| IntV of int
| RealV of float
| StrV of string
| SymV of string
| QuoteV of SExprView
| QuasiquoteV of SExprView
| UnquoteV of SExprView
| ProperListV of SExprView list
| DottedListV of SExprView list * SExprView

let rec format v =
  match v with
  | NilV -> "()"
  | FalseV -> "#f"
  | TrueV -> "#t"
  | IntV i -> string i
  | RealV r -> string r
  | StrV s -> sprintf "%A" s
  | SymV s -> s
  | QuoteV s -> sprintf "'%s" (format s)
  | QuasiquoteV s -> sprintf "`%s" (format s)
  | UnquoteV s -> sprintf ",%s" (format s)
  | ProperListV xs ->
    xs
    |> List.map format
    |> String.concat " "
    |> sprintf "(%s)"
  | DottedListV(xs, y) ->
    let es =
      xs
      |> List.map format
      |> String.concat " "
    sprintf "(%s . %s)" es (format y)

type SExprView with
  member v.Format = format v
