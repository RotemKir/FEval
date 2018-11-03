namespace FEval

module TypeFormatters =
    open Microsoft.FSharp.Quotations
    open Microsoft.FSharp.Reflection
    open System
    open System.Reflection

    let (|IsOption|_|) (valueType : Type) =
        if valueType.Name = "FSharpOption`1"
        then Some valueType
        else None

    let (|IsTuple|_|) (valueType : Type) =
        if FSharpType.IsTuple valueType
        then Some valueType
        else None
        
    let (|IsFunction|_|) (valueType : Type) =
        if FSharpType.IsFunction valueType
        then Some valueType
        else None

    let formatGenericTypeArguments typeFormatter (declaringType : Type) separator =
        Array.map typeFormatter declaringType.GenericTypeArguments
        |> String.concat separator

    let formatTupleType typeFormatter tupleType =
        sprintf "(%s)" <| formatGenericTypeArguments typeFormatter tupleType ", "

    let formatFunctionType typeFormatter functionType =
        sprintf "(%s)" <| formatGenericTypeArguments typeFormatter functionType " -> "

    let rec formatType (valueType : Type) =
        match valueType with
        | IsFunction t -> formatFunctionType formatType t
        | IsTuple t    -> formatTupleType formatType t
        | IsOption _   -> "Option"
        | t            -> t.Name
        
    let formatStateLastValue state valueType =
        let value = Evaluator.getLastValue state

        match valueType with
        | IsFunction t                 -> formatType t
        | IsOption t when value = null -> sprintf "None : %s" <| formatType t
        | t                            -> sprintf "%O : %s" value <| formatType t

    let formatVariable (variable : Var) =
        sprintf "%s : %s" variable.Name <| formatType variable.Type
        
    let formatMethodDisplayName (instanceExpr : Expr option) (methodInfo : MethodInfo) =
        match instanceExpr with
        | Some instance -> sprintf "%s.%s" (formatType instance.Type) methodInfo.Name
        | None          -> methodInfo.Name
