[<AutoOpen>]
module Scheme.Env
open Scheme
open System
open System.Collections.Generic

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Env =
  /// Create a new environment with one scope and no variables
  let create() =
    { Env.Symbols = Dictionary()
      Parent = None }

  let fromMap (map : #seq<KeyValuePair<_, _>>) =
    let dic = Dictionary()
    for KeyValue(k, v) in map do dic.Add(k, v)

    { Env.Symbols = dic
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
