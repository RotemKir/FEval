namespace FEval.Inspectors

[<RequireQualifiedAccess>]
module internal Validator =
    open FEval.EvaluationTypes
    open FEval.Inspectors.TypeFormatters
    open FEval.Inspectors.ValidationsCommon

    let private isCurrentEventRelevantForVariableRule 
            (validationRule : VariableRuleDefinition) 
            (validationContext : ValidationContext) =
        match validationContext.EvaluationEvent with
        | SetVariableEvent eventDetails -> eventDetails.Variable.Name = validationRule.VariableName
        | _                             -> false

    let private formateMessage (validationRequest : ValidationRequest) validationRule =
        let valueType = validationRequest.Value.GetType()
        sprintf "Variable '%s', %s, %s"
            <| validationRule.VariableName
            <| formatValue validationRequest.Value valueType
            <| validationRule.Validation.FormatMessage validationRequest 

    let private convertReturnTypeToValidationResult validationRule message = 
        match validationRule.ReturnWhenInvalid with
        | ReturnWarning -> Warning message
        | ReturnError   -> Error message

    let private createInvalidResult validationRequest validationRule =
        formateMessage validationRequest validationRule  
        |> convertReturnTypeToValidationResult validationRule
            
    let private createValidationResult validationRequest validationRule isValid =
        match isValid with
        | true  -> Ok
        | false -> createInvalidResult validationRequest validationRule
            
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
            let validationRequest = createValidationRequest variableValue.Value validationContext 
            
            validationRequest 
            |> validationRule.Validation.IsValid 
            |> createValidationResult validationRequest validationRule

    let private validateCustomRule customRule validationContext =
        customRule validationContext

    let runRule validationContext definition =
        match definition with
        | VariableRule variableRule -> validateVariable variableRule validationContext 
        | CustomRule customRule     -> validateCustomRule customRule validationContext