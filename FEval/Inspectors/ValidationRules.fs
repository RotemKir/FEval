namespace FEval.Inspectors

module internal ValidationRules =
    open FEval.Inspectors.TypeFormatters
    open FEval.Inspectors.ValidationsCommon
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

    let private isLessThan value (target : obj) =
        match value.GetType() with
        | IsInt16    value v -> v < (target :?> int16)
        | IsInt32    value v -> v < (target :?> int32)
        | IsInt64    value v -> v < (target :?> int64)
        | IsUInt16   value v -> v < (target :?> uint16)
        | IsUInt32   value v -> v < (target :?> uint32)
        | IsUInt64   value v -> v < (target :?> uint64)
        | IsByte     value v -> v < (target :?> byte)
        | IsSByte    value v -> v < (target :?> sbyte)
        | IsFloat    value v -> v < (target :?> float)
        | IsFloat32  value v -> v < (target :?> float32)
        | IsDecimal  value v -> v < (target :?> decimal)
        | IsDateTime value v -> v < (target :?> DateTime)
        | _                 -> false

    let private isMoreThan value (target : obj) =
        match value.GetType() with
        | IsInt16    value v -> v > (target :?> int16)
        | IsInt32    value v -> v > (target :?> int32)
        | IsInt64    value v -> v > (target :?> int64)
        | IsUInt16   value v -> v > (target :?> uint16)
        | IsUInt32   value v -> v > (target :?> uint32)
        | IsUInt64   value v -> v > (target :?> uint64)
        | IsByte     value v -> v > (target :?> byte)
        | IsSByte    value v -> v > (target :?> sbyte)
        | IsFloat    value v -> v > (target :?> float)
        | IsFloat32  value v -> v > (target :?> float32)
        | IsDecimal  value v -> v > (target :?> decimal)
        | IsDateTime value v -> v > (target :?> DateTime)
        | _                 -> false
    
    let private isEqualTo value (target : obj) =
        value = target

    let private isEmptyString value =
        String.IsNullOrEmpty value

    let private isEmptyEnumerable value =
        Seq.isEmpty <| Seq.cast value

    let private isEmpty (value : obj) =
        match value.GetType() with
        | IsString      value v -> isEmptyString v
        | IsIEnumerable value v -> isEmptyEnumerable v
        | _                     -> false
    
    let private isAndInvalid leftValidation rightValidation request =
        leftValidation.IsValid request = false && 
        rightValidation.IsValid request = false

    let private isOrInvalid leftValidation rightValidation request =
        leftValidation.IsValid request = false || 
        rightValidation.IsValid request = false

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

    let private isZeroFormatter _ =
        "should not be zero"

    let private isNegativeFormatter _ =
        "should not be negative"

    let private isEmptyFormatter _ =
        "should not be empty"

    let private isEqualtoFormatter target validationRequest =
        sprintf "should not equal %s"
            <| formatRuleTarget target validationRequest.ValidationContext

    let private isLessThanFormatter target validationRequest =
        sprintf "should not be less than %s"
            <| formatRuleTarget target validationRequest.ValidationContext

    let private isMoreThanFormatter target validationRequest =
        sprintf "should not be more than %s"
            <| formatRuleTarget target validationRequest.ValidationContext
                
    let private andFormatter leftValidation rightValidation validationRequest =
        sprintf "%s AND %s"
            <| leftValidation.FormatMessage validationRequest
            <| rightValidation.FormatMessage validationRequest
            
    let private orFormatter leftValidation rightValidation validationRequest =
        let isLeftValid = leftValidation.IsValid validationRequest
        let isRightValid = rightValidation.IsValid validationRequest
        let leftValidationMessage = leftValidation.FormatMessage validationRequest
        let rightValidationMessage = rightValidation.FormatMessage validationRequest

        match (isLeftValid, isRightValid) with
        | (false, false) -> sprintf "%s AND %s" leftValidationMessage rightValidationMessage
        | (false, true)  -> leftValidationMessage
        | (true, false)  -> rightValidationMessage
        | (true, true)   -> String.Empty        

    let private getRequestValue (request : ValidationRequest) =
        request.Value

    let private getRuleTargetValue target (request : ValidationRequest) =
        match target with
        | Value targetValue -> Some targetValue 
        | Variable name     -> getVariableValue request.ValidationContext name

    let private isInvalidTarget target isInValid request  =
        match getRuleTargetValue target request with
        | Some targetValue -> isInValid request.Value targetValue
        | None             -> false

    let private isZeroValidation =
        {
            IsValid = getRequestValue >> isZero >> not
            FormatMessage = isZeroFormatter
        }
    
    let private isNegativeValidation =
        {
            IsValid = getRequestValue >> isNegative >> not
            FormatMessage = isNegativeFormatter
        }
    
    let private isEmptyValidation =
        {
            IsValid = getRequestValue >> isEmpty >> not
            FormatMessage = isEmptyFormatter
        }
    
    let private isValidation target =
        {
            IsValid = isInvalidTarget target isEqualTo >> not
            FormatMessage = isEqualtoFormatter target
        }
    
    let private isLessThanValidation target =
        {
            IsValid = isInvalidTarget target isLessThan >> not
            FormatMessage = isLessThanFormatter target
        }
    
    let private isMoreThanValidation target =
        {
            IsValid = isInvalidTarget target isMoreThan >> not
            FormatMessage = isMoreThanFormatter target
        }
    
    let private andValidation leftValidation rightValidation =
        {
            IsValid = isAndInvalid leftValidation rightValidation >> not
            FormatMessage = andFormatter leftValidation rightValidation
        }
        
    let private orValidation leftValidation rightValidation =
        {
            IsValid = isOrInvalid leftValidation rightValidation >> not
            FormatMessage = orFormatter leftValidation rightValidation
        }

    let rec internal getVariableValidation invalidWhen =
        match invalidWhen with
        | ``Is Zero``             -> isZeroValidation
        | ``Is Negative``         -> isNegativeValidation
        | ``Is Empty``            -> isEmptyValidation
        | Is target               -> isValidation target
        | ``Is Less Than`` target -> isLessThanValidation target
        | ``Is More Than`` target -> isMoreThanValidation target
        | And (left, right) 
            -> andValidation 
                <| getVariableValidation left 
                <| getVariableValidation right
        | Or (left, right)  
            -> orValidation 
                <| getVariableValidation left 
                <| getVariableValidation right