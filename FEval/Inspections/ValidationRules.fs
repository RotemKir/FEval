namespace FEval.Inspections

module ValidationRules =
    open FEval.Inspections.ValidationTypes
    open FEval.TypeChecks
    open System

    let private isEmptyString value =
        String.IsNullOrEmpty value

    let private isEmptyEnumerable value =
        Seq.isEmpty <| Seq.cast value

    let isZero (value : obj) =
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
    
    let isNegative (value : obj) =
        match value.GetType() with
        | IsInt16   value v -> v < 0s
        | IsInt32   value v -> v < 0
        | IsInt64   value v -> v < 0L
        | IsSByte   value v -> v < 0y
        | IsFloat   value v -> v < 0.0
        | IsFloat32 value v -> v < 0.0f
        | IsDecimal value v -> v < 0.0m
        | _                 -> false

    let isEmpty (value : obj) =
        match value.GetType() with
        | IsString      value v -> isEmptyString v
        | IsIEnumerable value v -> isEmptyEnumerable v
        | _                     -> false

    let isNotZero : obj -> bool = not << isZero
    
    let isNotNegative : obj -> bool = not << isNegative
    
    let isNotEmpty : obj -> bool = not << isEmpty
    
    let createErrorIfVariable name invalidWhen =
        Variable { VariableName = name ; InvalidWhen = invalidWhen ; ErrorLevel = ErrorLevel.Error }
    
    let createWarningIfVariable name invalidWhen =
        Variable { VariableName = name ; InvalidWhen = invalidWhen ; ErrorLevel = ErrorLevel.Warning }

