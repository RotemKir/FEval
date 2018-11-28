namespace FEval.Inspections

module ValidationRules =
    open FEval.Inspections.ValidationTypes
    open FEval.TypeChecks

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

    let isNotZero : obj -> bool = not << isZero

    let createErrorIfVariable name test =
        Variable (name, test, ErrorLevel.Error)
    
    let createWarningIfVariable name test =
        Variable (name, test, ErrorLevel.Warning)

