namespace FEval

module TypeFormatters =
    open Microsoft.FSharp.Quotations
    open Microsoft.FSharp.Reflection
    open System
    open System.Reflection

    // Private functions

    let private getParameterTypes =
        Array.map (fun (p : ParameterInfo) -> p.ParameterType)

    // Public functions
    
    let (|IsOption|_|) (valueType : Type) =
        if valueType.Name = "FSharpOption`1"
        then Some valueType
        else None

    let (|IsTuple|_|) valueType =
        if FSharpType.IsTuple valueType
        then Some valueType
        else None
        
    let (|IsFunction|_|) valueType =
        if FSharpType.IsFunction valueType
        then Some valueType
        else None

    let formatTypes types separator typeFormatter =
        Array.map typeFormatter types
        |> String.concat separator

    let formatGenericTypeArguments (declaringType : Type) =
        formatTypes declaringType.GenericTypeArguments

    let formatTupleType tupleType typeFormatter =
        sprintf "(%s)" <| formatGenericTypeArguments tupleType ", " typeFormatter

    let formatFunctionType functionType typeFormatter  =
        sprintf "(%s)" <| formatGenericTypeArguments functionType " -> " typeFormatter

    let rec formatType (valueType : Type) =
        match valueType with
        | IsFunction t -> formatFunctionType t formatType 
        | IsTuple t    -> formatTupleType t formatType 
        | IsOption _   -> "Option"
        | t            -> t.Name
        
    let formatValue value valueType =
        match valueType with
        | IsFunction t                 -> formatType t
        | IsOption t when value = null -> sprintf "None : %s" <| formatType t
        | t                            -> sprintf "%A : %s" value <| formatType t

    let formatStateLastValue state =
        formatValue <| Evaluator.getLastValue state

    let formatVariable (variable : Var) =
        sprintf "%s : %s" variable.Name <| formatType variable.Type
        
    let formatMethodDisplayName (instanceExpr : Expr option) (methodInfo : MethodInfo) =
        match instanceExpr with
        | Some instance -> sprintf "%s.%s" (formatType instance.Type) methodInfo.Name
        | None          -> methodInfo.Name

    let formatParameters parameters =
        formatTypes <| getParameterTypes parameters <| ", " <| formatType

    let formatCtor (constructorInfo : ConstructorInfo) =
        sprintf "%s (%s)" 
        <| formatType constructorInfo.DeclaringType 
        <| formatParameters (constructorInfo.GetParameters())