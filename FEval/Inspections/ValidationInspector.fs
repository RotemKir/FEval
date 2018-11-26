namespace FEval.Inspections

[<RequireQualifiedAccess>]
module Valudate =
    open FEval.EvaluationTypes
    open FSharp.Quotations
    open FEval
    open FEval.Inspections.TypeChecks
    open FEval.Inspections.ValidationTypes

    let private getVariableValue inspectionContext name =
        let tempVar = new Var(name, typeof<obj>)
        
        if Evaluator.varExists tempVar inspectionContext.EvaluationState
        then Some <| Evaluator.getVar tempVar inspectionContext.EvaluationState
        else None

    let private isZero value =
        match value.GetType() with
        | IsInt16 value v   -> v = 0s
        | IsInt32 value v   -> v = 0
        | IsInt64 value v   -> v = 0L
        | IsUInt16 value v  -> v = 0us
        | IsUInt32 value v  -> v = 0u
        | IsUInt64 value v  -> v = 0UL
        | IsByte value v    -> v = 0uy
        | IsSByte value v   -> v = 0y
        | IsFloat value v   -> v = 0.0
        | IsFloat32 value v -> v = 0.0f
        | IsDecimal value v -> v = 0.0m
        | _                 -> false

    let private isNotZero = not << isZero

    let createValidationResult isValid errorMessage =
        match isValid with
        | true  -> ValidationResult.Ok
        | false -> ValidationResult.Error errorMessage

    let private validateIfVariableExists inspectionContext name isValid =
        match getVariableValue inspectionContext name with
        | Some value -> isValid value
        | None       -> true

    let private validateIsNotZero inspectionContext name =
        validateIfVariableExists inspectionContext name isNotZero
        |> createValidationResult <| sprintf "Variable %s should not be zero" name

    let private runRule definition inspectionContext =
        match definition with
        | Variable (name, IsNotZero) -> validateIsNotZero inspectionContext name
        | Custom rule                -> rule inspectionContext
        | _                          -> ValidationResult.Ok