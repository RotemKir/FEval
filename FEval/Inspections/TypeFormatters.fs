namespace FEval.Inspections

module TypeFormatters =
    open Microsoft.FSharp.Quotations
    open Microsoft.FSharp.Reflection
    open System
    open System.Reflection
    open FEval.Inspections.CommonInspections
    open FEval.Inspections.TypeChecks

    // Private functions

    let private getParameterTypes =
        Array.map (fun (p : ParameterInfo) -> p.ParameterType)

    let private typeNameOverrides = 
        new Map<string, string>
            [|
                ("FSharpOption`1", "Option")
                ("FSharpList`1"  , "List")
                ("FSharpMap`2"   , "Map")
            |]      

    let private normalizeGenericTypeName (typeName :String) =
        if typeName.IndexOf('`') > -1 
        then typeName.Split('`').[0]
        else typeName

    let private getTypeNameOverride typeName =
        match Map.tryFind typeName typeNameOverrides with
        | Some s -> s
        | None   -> normalizeGenericTypeName typeName

    // Public functions
    
    let formatTypes (types : Type array) separator typeFormatter =
        Array.map typeFormatter types
        |> String.concat separator

    let formatGenericTypeArguments (declaringType : Type) =
        formatTypes declaringType.GenericTypeArguments

    let formatTupleType tupleType typeFormatter =
        sprintf "(%s)" <| formatGenericTypeArguments tupleType ", " typeFormatter

    let formatFunctionType functionType typeFormatter  =
        sprintf "(%s)" <| formatGenericTypeArguments functionType " -> " typeFormatter

    let formatGenericType (genericType : Type) typeFormatter =
        sprintf "%s<%s>" 
        <| getTypeNameOverride genericType.Name
        <| formatGenericTypeArguments genericType ", " typeFormatter

    let rec formatType (valueType : Type) =
        match valueType with
        // Format functions as arrow notation (a -> b -> c) where a,b,c are types
        | IsFunction t    -> formatFunctionType t formatType 
        // Format tuples as (a, b, c)  where a,b,c are types
        | IsTuple t       -> formatTupleType t formatType 
        // Format generic types as a<b,c> where a is the type name and b,c are the type arguments
        | IsGenericType t -> formatGenericType t formatType
        // Return the type name since no special formatting exists
        | t               -> t.Name
        
    let rec formatValue value valueType =
        match valueType with
        // If null object format as null since we can't unbox an actual value
        | IsObject _ when value = null        -> "null : Object"
        // If value is actually object, format as Object to prevent stack overflow
        | IsObject t when value.GetType() = t -> "Object"
        // If object, call this method again with the actual value type
        | IsObject _                          -> sprintf "%s (Object)" <| (formatValue value <| value.GetType())
        // Format functions as arrow notation (a -> b -> c)
        | IsFunction t                        -> formatType t
        // Format null option as None, otherwise we'll show null
        | IsOption t when value = null        -> sprintf "None : %s" <| formatType t
        // If the type overrides ToString use it to format the value
        | HasToString _                       -> sprintf "%A : %s" value <| formatType valueType
        // Format only the type since no special formatting exists
        | t                                   -> formatType t

    let formatVariable (variable : Var) =
        sprintf "%s : %s" variable.Name <| formatType variable.Type
    
    let formatVariables (variables : Var seq) =
        Seq.map formatVariable variables
        |> String.concat ", "

    let formatParameters parameters =
        formatTypes <| getParameterTypes parameters <| ", " <| formatType

    let formatIndexerParameters parameters =
        match parameters with
        | [||] -> String.Empty
        | _    -> sprintf "[%s]" <| formatParameters parameters

    let formatMethodName (methodInfo : MethodInfo) declaringType =
        sprintf "%s.%s" <| formatType declaringType <| methodInfo.Name

    let formatMethod (methodInfo : MethodInfo) instanceExpr =
        let declaringType = getDeclaringType instanceExpr methodInfo.DeclaringType
        let parameters = methodInfo.GetParameters()
        sprintf "%s(%s)" 
            <| formatMethodName methodInfo declaringType
            <| formatParameters parameters
        
    let formatCtor (constructorInfo : ConstructorInfo) =
        sprintf "%s(%s)" 
        <| formatType constructorInfo.DeclaringType 
        <| formatParameters (constructorInfo.GetParameters())

    let formatProperty (propertyInfo : PropertyInfo) instanceExpr =
        let typeName = formatType <| getDeclaringType instanceExpr propertyInfo.DeclaringType
        let parameters = formatIndexerParameters <| propertyInfo.GetIndexParameters()
        sprintf "%s.%s%s" typeName propertyInfo.Name parameters

    let formatField (fieldInfo : FieldInfo) instanceExpr =
        let typeName = formatType <| getDeclaringType instanceExpr fieldInfo.DeclaringType
        sprintf "%s.%s" typeName fieldInfo.Name

    let formatUnionCaseInfo (unionCaseInfo : UnionCaseInfo) =
        let typeName = formatType <| unionCaseInfo.DeclaringType
        sprintf "%s : %s" unionCaseInfo.Name typeName 