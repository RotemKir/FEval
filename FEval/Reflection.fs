namespace FEval

module Reflection =
    open System.Reflection
    open Microsoft.FSharp.Reflection

    // Public functions

    let invokeMethod (methodInfo : MethodInfo) parameters =
        methodInfo.Invoke (null, parameters)

    let makeUnion unionCaseInfo args =
        FSharpValue.MakeUnion (unionCaseInfo, args)

    let makeRecord recordType values =
        FSharpValue.MakeRecord (recordType, values)

    let makeTuple tupleType elements =
        FSharpValue.MakeTuple (elements, tupleType)

    let makeFunction domain range body =
        let funcType = FSharpType.MakeFunctionType (domain, range)
        FSharpValue.MakeFunction (funcType, body)   

