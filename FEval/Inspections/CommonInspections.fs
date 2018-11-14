namespace FEval.Inspections

module CommonInspections =
    open Microsoft.FSharp.Quotations.Patterns
    open Microsoft.FSharp.Quotations
    open Microsoft.FSharp.Reflection
    open System
    open FEval

    type InpectionStage = Pre | Post

    type Inspector<'a> = DateTime -> InspectionEvent -> EvaluationState -> 'a

    let getExprName expr =
        match expr with
        | Application         _ -> "Application"
        | Call                _ -> "Call"
        | Coerce              _ -> "Coerce"
        | DefaultValue        _ -> "DefaultValue"
        | FieldGet            _ -> "FieldGet"
        | FieldSet            _ -> "FieldSet"
        | ForIntegerRangeLoop _ -> "ForIntegerRangeLoop"
        | IfThenElse          _ -> "IfThenElse"
        | Lambda              _ -> "Lambda"
        | Let                 _ -> "Let"
        | LetRecursive        _ -> "LetRecursive"
        | NewArray            _ -> "NewArray"
        | NewObject           _ -> "NewObject"
        | NewRecord           _ -> "NewRecord"
        | NewTuple            _ -> "NewTuple"
        | NewUnionCase        _ -> "NewUnionCase"
        | PropertyGet         _ -> "PropertyGet"
        | PropertySet         _ -> "PropertySet"
        | QuoteRaw            _ -> "QuoteRaw"
        | QuoteTyped          _ -> "QuoteTyped"
        | Sequential          _ -> "Sequential"
        | TryFinally          _ -> "TryFinally"
        | TryWith             _ -> "TryWith"
        | TupleGet            _ -> "TupleGet"
        | TypeTest            _ -> "TypeTest"
        | UnionCaseTest       _ -> "UnionCaseTest"
        | Value               _ -> "Value"
        | VarSet              _ -> "VarSet"
        | Var                 _ -> "Var"
        | WhileLoop           _ -> "WhileLoop"
        |                     _ -> failwithf "Expression %O is not supported" expr
    
    let getDeclaringType (instanceExpr : Expr option) declaringType =
        match instanceExpr with
        | Some expr -> expr.Type
        | None      -> declaringType

    let getTupleItemType tupleType index =
        FSharpType.GetTupleElements tupleType
        |> Array.item index

    let getFunctionReturnType funcType =
        snd <| FSharpType.GetFunctionElements funcType
    
    let inspectExprEvent inspectionEvent exprInspection =
        match inspectionEvent with
        | ExprEvent expr -> Some <| exprInspection expr
        | _              -> None

    let inspectMethodEvent inspectionEvent methodInspection =
        match inspectionEvent with
        | MethodEvent methodEventDetails -> Some <| methodInspection methodEventDetails 
        | _                              -> None
