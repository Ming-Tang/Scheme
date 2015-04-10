module Scheme.Standard
open System
open System.Collections.Generic
open Scheme
open Scheme.ActivePatterns

/// Create an environment with standard primitives
let createEnv() =
  let env = Env.create()
  Env.extend Primitives.standardSymbols env

/// Configuration with standard primitives and evaluation rules
let standardConfig = {
  Config.Primitives = Primitives.standardPrimitives
  EvalRules = Rules.standardRules
}

/// Evaluate an Expr using the standard configuration
let eval : Eval = Eval.eval standardConfig

