namespace FEval.Inspections

[<RequireQualifiedAccess>]
module Validator =
    open FEval.Inspections.ValidationsCommon
         
    let private validateIfVariableExists validationRule validationContext =
        Option.bind
            <| fun value -> Some (validationRule.Validation.IsValid value, value) 
            <| getVariableValue validationContext validationRule.VariableName
     
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
            
    let private validateVariable validationRule validationContext =
        match validateIfVariableExists validationRule validationContext with
        | Some (false, value) -> createInvalidResult value validationRule validationContext
        | Some (true, _)      -> Ok 
        | None                -> Ok

    let private validateCustomRule customRule validationContext =
        customRule validationContext

    let private runRule validationContext definition =
        match definition with
        | VariableRule variableRule -> validateVariable variableRule validationContext 
        | CustomRule customRule     -> validateCustomRule customRule validationContext

    let runRules validationContext definitions =
        Seq.map
            <| runRule validationContext
            <| definitions