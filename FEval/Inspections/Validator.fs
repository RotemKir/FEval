namespace FEval.Inspections

[<RequireQualifiedAccess>]
module Validator =
    open FEval.Inspections.ValidationRules
    open FEval.Inspections.ValidationTypes

    let private getVariableValue validationContext name =
        Map.tryFind name validationContext.Variables
            
    let private createValidationResult isValid errorLevel errorMessage =
        match (isValid, errorLevel) with
        | (true, _)        -> ValidationResult.Ok
        | (false, Warning) -> ValidationResult.Warning errorMessage
        | (false, Error)   -> ValidationResult.Error errorMessage

    let private validateIfVariableExists validationContext name isValid =
        match getVariableValue validationContext name with
        | Some value -> isValid value
        | None       -> true

    let private getTestValidation test =
        match test with
        | IsNotZero     -> (isNotZero,     fun name -> sprintf "Variable %s should not be zero" name)
        | IsNotNegative -> (isNotNegative, fun name -> sprintf "Variable %s should not be negative" name)
        | _             -> invalidOp "Error" 

    let private validateVariable validationContext variableRule =
        let (validation, errorMessage) = getTestValidation variableRule.Test

        createValidationResult 
            <| validateIfVariableExists validationContext variableRule.VariableName validation
            <| variableRule.ErrorLevel 
            <| errorMessage variableRule.VariableName

    let private runRule validationContext definition =
        match definition with
        | Variable variableRule  
            -> validateVariable validationContext variableRule
        | Custom rule                
            -> rule validationContext
        | _ -> ValidationResult.Ok

    let runRules validationContext definitions  =
        Seq.map
            <| runRule validationContext
            <| definitions