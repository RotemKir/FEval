namespace FEval.Inspections

[<RequireQualifiedAccess>]
module Validator =
    open FEval.Inspections.ValidationRules
    open FEval.Inspections.ValidationTypes

    let private isZeroValidation =
        {
            IsValid = isNotZero
            FormatError = fun name -> sprintf "Variable '%s' should not be zero" name
        }

    let private isNegativeValidation =
        {
            IsValid = isNotNegative
            FormatError = fun name -> sprintf "Variable '%s' should not be negative" name
        }
    
    let private isEmptyValidation =
        {
            IsValid = isNotEmpty
            FormatError = fun name -> sprintf "Variable '%s' should not be empty" name
        }

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

    let private getValidation invalidWhen =
        match invalidWhen with
        | IsZero     -> isZeroValidation
        | IsNegative -> isNegativeValidation
        | IsEmpty    -> isEmptyValidation
        | _          -> invalidOp "Error" 

    let private validateVariable validationContext variableRule =
        let validation = getValidation variableRule.InvalidWhen

        createValidationResult 
            <| validateIfVariableExists validationContext variableRule.VariableName validation.IsValid
            <| variableRule.ErrorLevel 
            <| validation.FormatError variableRule.VariableName

    let private runRule validationContext definition =
        match definition with
        | Variable variableRule  
            -> validateVariable validationContext variableRule
        | Custom rule                
            -> rule validationContext
        | _ -> ValidationResult.Ok

    let runRules validationContext definitions =
        Seq.map
            <| runRule validationContext
            <| definitions