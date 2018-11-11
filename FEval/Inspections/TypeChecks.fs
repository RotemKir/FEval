namespace FEval.Inspections

module TypeChecks =
    open System
    open Microsoft.FSharp.Reflection

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

    let (|IsObject|_|) valueType =
        if valueType = typeof<obj>
        then Some valueType
        else None
        
    let (|IsGenericType|_|) (valueType : Type) =
        if valueType.IsGenericType
        then Some valueType
        else None

    let (|HasToString|_|) (valueType : Type) =
        let toStringMethod = valueType.GetMethod("ToString", [||])
        
        if toStringMethod.DeclaringType = typeof<obj> || 
           toStringMethod.DeclaringType = typeof<ValueType>
        then None
        else Some valueType