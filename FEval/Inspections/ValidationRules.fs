namespace FEval.Inspections

module ValidationRules =
    open FEval.Inspections.TypeFormatters
    open FEval.Inspections.ValidationTypes
    open FEval.TypeChecks
    open System
       
    let private isZero (value : obj) =
        match value.GetType() with
        | IsInt16   value v -> v = 0s
        | IsInt32   value v -> v = 0
        | IsInt64   value v -> v = 0L
        | IsUInt16  value v -> v = 0us
        | IsUInt32  value v -> v = 0u
        | IsUInt64  value v -> v = 0UL
        | IsByte    value v -> v = 0uy
        | IsSByte   value v -> v = 0y
        | IsFloat   value v -> v = 0.0
        | IsFloat32 value v -> v = 0.0f
        | IsDecimal value v -> v = 0.0m
        | _                 -> false
    
    let private isZeroFormatter formatMessageRequest =
        let valueType = formatMessageRequest.Value.GetType()
        sprintf "Variable '%s', %s, should not be zero"
            <| formatMessageRequest.VariableName
            <| formatValue formatMessageRequest.Value valueType

    let private isZeroValidation =
        {
            IsValid = not << isZero
            FormatMessage = isZeroFormatter
        }

    let private isNegative (value : obj) =
        match value.GetType() with
        | IsInt16   value v -> v < 0s
        | IsInt32   value v -> v < 0
        | IsInt64   value v -> v < 0L
        | IsSByte   value v -> v < 0y
        | IsFloat   value v -> v < 0.0
        | IsFloat32 value v -> v < 0.0f
        | IsDecimal value v -> v < 0.0m
        | _                 -> false

    let private isNegativeFormatter formatMessageRequest =
        let valueType = formatMessageRequest.Value.GetType()
        sprintf "Variable '%s', %s, should not be negative"
            <| formatMessageRequest.VariableName
            <| formatValue formatMessageRequest.Value valueType

    let private isNegativeValidation =
        {
            IsValid = not << isNegative
            FormatMessage = isNegativeFormatter
        }

    let private isEmptyString value =
        String.IsNullOrEmpty value

    let private isEmptyEnumerable value =
        Seq.isEmpty <| Seq.cast value

    let private isEmpty (value : obj) =
        match value.GetType() with
        | IsString      value v -> isEmptyString v
        | IsIEnumerable value v -> isEmptyEnumerable v
        | _                     -> false

    let isEmptyFormatter formatMessageRequest =
        let valueType = formatMessageRequest.Value.GetType()
        sprintf "Variable '%s', %s, should not be empty"
            <| formatMessageRequest.VariableName
            <| formatValue formatMessageRequest.Value valueType

    let private isEmptyValidation =
        {
            IsValid = not << isEmpty
            FormatMessage = isEmptyFormatter
        }
    
    let private formatValueRuleTarget value =
        formatValue value <| value.GetType()

    let private formatVariableRuleTarget name validationContext =
        match getVariableValue validationContext name with
        | Some value -> sprintf "variable '%s', %s" name <| formatValueRuleTarget value
        | None       -> "(null)"

    let private formatRuleTarget target validationContext =
        match target with
        | Value value   -> formatValueRuleTarget value
        | Variable name -> formatVariableRuleTarget name validationContext
    
    let isLessThanFormatter target formatMessageRequest =
        let valueType = formatMessageRequest.Value.GetType()
        sprintf "Variable '%s', %s, should not be less than %s"
            <| formatMessageRequest.VariableName
            <| formatValue formatMessageRequest.Value valueType
            <| formatRuleTarget target formatMessageRequest.ValidationContext

    let private isLessThanValidation target =
        {
            IsValid = fun _ -> true
            FormatMessage = isLessThanFormatter target
        }
        
    let private getVariableValidation invalidWhen =
        match invalidWhen with
        | IsZero            -> isZeroValidation
        | IsNegative        -> isNegativeValidation
        | IsEmpty           -> isEmptyValidation
        | IsLessThan target -> isLessThanValidation target
        | _                 -> invalidOp "Error" 
           
    let ifVariable name invalidWhen thenReturn =
        VariableRule
            {
                VariableName = name
                Validation = getVariableValidation invalidWhen
                ReturnWhenInvalid = thenReturn
            }