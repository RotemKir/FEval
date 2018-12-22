namespace FEval

module internal TypeChecks =
    open System
    open Microsoft.FSharp.Reflection
    open System.Collections

    let private convertIfType<'t> (value : obj) valueType =
        if valueType = typeof<'t> 
        then Some <| (value :?> 't)
        else None

    let (|IsInt16|_|) = convertIfType<int16>
    let (|IsInt32|_|) = convertIfType<int32>
    let (|IsInt64|_|) = convertIfType<int64>
    let (|IsUInt16|_|) = convertIfType<uint16>
    let (|IsUInt32|_|) = convertIfType<uint32>
    let (|IsUInt64|_|) = convertIfType<uint64>
    let (|IsByte|_|) = convertIfType<byte>
    let (|IsSByte|_|) = convertIfType<sbyte>
    let (|IsFloat|_|) = convertIfType<float>
    let (|IsFloat32|_|) = convertIfType<float32>
    let (|IsDecimal|_|) = convertIfType<decimal>
    let (|IsString|_|) = convertIfType<string>
    let (|IsDateTime|_|) = convertIfType<DateTime>

    let (|IsIEnumerable|_|) (value : obj) (valueType : Type) =
        if typeof<IEnumerable>.IsAssignableFrom(valueType)
        then Some (value :?> IEnumerable)
        else None

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