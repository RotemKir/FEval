﻿namespace FEval.Inspections

module ValidationRules =
    open FEval.Inspections.TypeFormatters
    open FEval.Inspections.ValidationsCommon
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

    let private isLessThanFormatter target validationRequest =
        sprintf "should not be less than %s"
            <| formatRuleTarget target validationRequest.ValidationContext

    let private isMoreThanFormatter target validationRequest =
        sprintf "should not be more than %s"
            <| formatRuleTarget target validationRequest.ValidationContext
                
    let private andFormatter leftValidation rightValidation formatMessageRequest =
        sprintf "(%s AND %s)"
            <| leftValidation.FormatMessage formatMessageRequest
            <| rightValidation.FormatMessage formatMessageRequest

    let private getRequestValue (request : ValidationRequest) =
        request.Value

    let private getRuleTargetValue target (request : ValidationRequest) =
        match target with
        | Value targetValue -> Some targetValue 
        | Variable name     -> getVariableValue request.ValidationContext name

    let private isValidTarget target isValid request  =
        match getRuleTargetValue target request with
        | Some targetValue -> isValid request.Value targetValue
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
    
    let private isLessThanValidation target =
        {
            IsValid = isValidTarget target isLessThan >> not
            FormatMessage = isLessThanFormatter target
        }
    
    let private isMoreThanValidation target =
        {
            IsValid = isValidTarget target isMoreThan >> not
            FormatMessage = isMoreThanFormatter target
        }
    
    let private andValidation leftValidation rightValidation =
        {
            IsValid = isAndInvalid leftValidation rightValidation >> not
            FormatMessage = andFormatter leftValidation rightValidation
        }

    let rec private getVariableValidation invalidWhen =
        match invalidWhen with
        | IsZero            -> isZeroValidation
        | IsNegative        -> isNegativeValidation
        | IsEmpty           -> isEmptyValidation
        | IsLessThan target -> isLessThanValidation target
        | IsMoreThan target -> isMoreThanValidation target
        | And (left, right) -> andValidation 
                                <| getVariableValidation left 
                                <| getVariableValidation right
           
    let ifVariable name invalidWhen thenReturn =
        VariableRule
            {
                VariableName = name
                Validation = getVariableValidation invalidWhen
                ReturnWhenInvalid = thenReturn
            }