namespace FEval.Inspectors

module InspectionsCommon =
    open Microsoft.FSharp.Quotations.Patterns
    open Microsoft.FSharp.Quotations
    open Microsoft.FSharp.Reflection

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
    
    let private getTypeWithDefault instance getInstanceType defaultType =
        match instance with
        | Some value -> getInstanceType value
        | None       -> defaultType

    let getExprType (instanceExpr : Expr option) =
        getTypeWithDefault instanceExpr (fun expr -> expr.Type)

    let getInstanceType instance =
        getTypeWithDefault <| Option.ofObj instance <| (fun i -> i.GetType())

    let getTupleItemType tupleType index =
        FSharpType.GetTupleElements tupleType
        |> Array.item index

    let getFunctionReturnType funcType =
        snd <| FSharpType.GetFunctionElements funcType