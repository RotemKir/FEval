namespace FEval.Inspections

[<RequireQualifiedAccess>]
module Validator =
    open FEval.Inspections.TypeChecks
    open FEval.Inspections.ValidationTypes

    let private getVariableValue validationContext name =
        Map.tryFind name validationContext.Variables

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

    let private createValidationResult isValid errorMessage =
        match isValid with
        | true  -> ValidationResult.Ok
        | false -> ValidationResult.Error errorMessage

    let private validateIfVariableExists validationContext name isValid =
        match getVariableValue validationContext name with
        | Some value -> isValid value
        | None       -> true

    let private validateIsNotZero validationContext name =
        validateIfVariableExists validationContext name isNotZero
        |> createValidationResult <| sprintf "Variable %s should not be zero" name

    let private runRule validationContext definition =
        match definition with
        | Variable (name, IsNotZero) -> validateIsNotZero validationContext name
        | Custom rule                -> rule validationContext
        | _                          -> ValidationResult.Ok

    let runRules validationContext definitions  =
        Seq.map
            <| runRule validationContext
            <| definitions