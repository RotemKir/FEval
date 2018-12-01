namespace FEval.Inspections

module ValidationRules =
    open FEval.Inspections.ValidationTypes
    open FEval.TypeChecks
    open System

    let private isEmptyString value =
        String.IsNullOrEmpty value

    let private isEmptyEnumerable value =
        Seq.isEmpty <| Seq.cast value

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

    let private isEmpty (value : obj) =
        match value.GetType() with
        | IsString      value v -> isEmptyString v
        | IsIEnumerable value v -> isEmptyEnumerable v
        | _                     -> false

    let private isNotZero : obj -> bool = not << isZero
    
    let private isNotNegative : obj -> bool = not << isNegative
    
    let private isNotEmpty : obj -> bool = not << isEmpty
    
    let private isZeroRule =
        {
            IsValid = isNotZero
            FormatError = fun name -> sprintf "Variable '%s' should not be zero" name
        }

    let private isNegativeRule =
        {
            IsValid = isNotNegative
            FormatError = fun name -> sprintf "Variable '%s' should not be negative" name
        }
    
    let private isEmptyRule =
        {
            IsValid = isNotEmpty
            FormatError = fun name -> sprintf "Variable '%s' should not be empty" name
        }

    let private getVariableValidation invalidWhen =
        match invalidWhen with
        | IsZero     -> isZeroRule
        | IsNegative -> isNegativeRule
        | IsEmpty    -> isEmptyRule
        | _          -> invalidOp "Error" 

    let ifVariable name invalidWhen thenReturn =
        Variable 
            { 
                VariableName = name 
                Validation = getVariableValidation invalidWhen 
                ReturnWhenInvalid = thenReturn
            }