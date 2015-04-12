[<AutoOpen>]
module Scheme.SExprView

/// Parse tree of an S-expression
[<AutoOpen>]
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
