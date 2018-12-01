namespace FEval.Inspections

[<RequireQualifiedAccess>]
module Validator =
    open FEval.Inspections.ValidationTypes

    let private getVariableValue validationContext name =
        Map.tryFind name validationContext.Variables
            
    let private createValidationResult isValid errorLevel errorMessage =
        match (isValid, errorLevel) with
        | (true, _)              -> ValidationResult.Ok
        | (false, ReturnWarning) -> ValidationResult.Warning errorMessage
        | (false, ReturnError)   -> ValidationResult.Error errorMessage

    let private validateIfVariableExists validationContext validationRule =
        match getVariableValue validationContext validationRule.VariableName with
        | Some value -> validationRule.Validation.IsValid value
        | None       -> true

    let private validateVariable validationContext variableRule =
        createValidationResult 
            <| validateIfVariableExists validationContext variableRule
            <| variableRule.ReturnWhenInvalid 
            <| variableRule.Validation.FormatError variableRule.VariableName

    let private validateCustomRule validationContext customRule =
        customRule validationContext

    let private runRule validationContext definition =
        match definition with
        | Variable variableRule -> validateVariable validationContext variableRule
        | Custom customRule     -> validateCustomRule validationContext customRule

    let runRules validationContext definitions =
        Seq.map
            <| runRule validationContext
            <| definitions