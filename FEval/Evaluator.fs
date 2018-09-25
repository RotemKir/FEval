namespace FEval

open System.Collections
open Microsoft.FSharp.Quotations
open System.Collections.Generic

type EvaluationState =
    {
        Stack : Stack
        Variables : Dictionary<string, obj>
        EvalFunc : EvaluationState -> Expr -> unit
    }

[<RequireQualifiedAccess>]
module Evaluator =

    let pop state =
        state.Stack.Pop()

    let push state item =
        state.Stack.Push(item)

    let setVar state (variable : Var) value =
        state.Variables.Add(variable.Name, value)

    let getVar state (variable : Var) =
        state.Variables.Item variable.Name

    let evalExpr state expr =
        state.EvalFunc state expr
        state |> pop 

    let evalExprs state exprs =
        exprs |> List.toArray |> Array.map (evalExpr state) 

    let createNew evalFunc =
        {
            Stack = new Stack()
            Variables = new Dictionary<string, obj>()
            EvalFunc = evalFunc
        }

    let eval evalFunc =
        createNew evalFunc |> evalExpr 
