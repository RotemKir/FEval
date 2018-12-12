namespace FEval.Inspections

[<RequireQualifiedAccess>]
module Validator =
    open FEval.Inspections.ValidationsCommon
    open FEval.EvaluationTypes

    let private isCurrentEventRelevantForVariableRule 
            (validationRule : VariableRuleDefinition) 
            (validationContext : ValidationContext) =
        match validationContext.EvaluationEvent with
        | SetVariableEvent eventDetails -> eventDetails.Variable.Name = validationRule.VariableName
        | _                             -> false

    let private formateMessage value validationRule validationContext =
        validationRule.Validation.FormatMessage
            {
                VariableName = validationRule.VariableName 
                Value = value
                ValidationContext = validationContext
            }

    let private convertReturnTypeToValidationResult validationRule message = 
        match validationRule.ReturnWhenInvalid with
        | ReturnWarning -> Warning message
        | ReturnError   -> Error message

    let private createInvalidResult value validationRule validationContext =
        formateMessage value validationRule validationContext  
        |> convertReturnTypeToValidationResult validationRule
            
    let private createValidationResult value validationRule validationContext isValid =
        match isValid with
        | true  -> Ok
        | false -> createInvalidResult value validationRule validationContext
            
    let private createValidationRequest value validationContext =
        { 
            Value = value
            ValidationContext = validationContext 
        }

    let private validateVariable validationRule validationContext =
        let isRelevantRule = isCurrentEventRelevantForVariableRule validationRule validationContext
        let variableValue = getVariableValue validationContext validationRule.VariableName
        
        if isRelevantRule = false || Option.isNone variableValue
        then Ok
        else
            createValidationRequest variableValue.Value validationContext 
            |> validationRule.Validation.IsValid 
            |> createValidationResult variableValue.Value validationRule validationContext

    let private validateCustomRule customRule validationContext =
        customRule validationContext

    let runRule validationContext definition =
        match definition with
        | VariableRule variableRule -> validateVariable variableRule validationContext 
        | CustomRule customRule     -> validateCustomRule customRule validationContext