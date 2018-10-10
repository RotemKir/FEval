namespace FEval

module Reflection =
    open System
    open System.Reflection
    open Microsoft.FSharp.Reflection

    // Public functions
    
    let invokeMethod instance (methodInfo : MethodBase) parameters =
        methodInfo.Invoke(instance, parameters)
    
    let invokeCtor (constructorInfo : ConstructorInfo) parameters =
        constructorInfo.Invoke(parameters)

    let invokeGetProperty instance (propertyinfo : PropertyInfo) parameters =
        let getPropertyMethod = propertyinfo.GetGetMethod()
        invokeMethod instance getPropertyMethod parameters

    let invokeSetProperty instance (propertyinfo : PropertyInfo) value indexerParameters = 
        let setPropertyMethod = propertyinfo.GetSetMethod()
        invokeMethod instance setPropertyMethod <| Array.append indexerParameters [|value|]
        
    let getMethodInfo instance methodName =
        let instanceType = instance.GetType()
        instanceType.GetMethod(methodName)

    let makeUnion unionCaseInfo args =
        FSharpValue.MakeUnion (unionCaseInfo, args)

    let makeRecord recordType values =
        FSharpValue.MakeRecord (recordType, values)

    let makeTuple tupleType elements =
        FSharpValue.MakeTuple (elements, tupleType)

    let makeArray arrayType (elements : obj[]) =
        let typedArray = Array.CreateInstance(arrayType, elements.Length)
        Array.iteri (fun index item -> typedArray.SetValue(item, index)) elements
        typedArray

    let makeFunction domain range body =
        let funcType = FSharpType.MakeFunctionType (domain, range)
        FSharpValue.MakeFunction (funcType, body)   

    let createNewInstance (typeToCreate : Type) =
        Activator.CreateInstance typeToCreate 

    let (|MethodFullName|_|) (methodInfo : MethodInfo) =
        Some <| sprintf "%s.%s" methodInfo.DeclaringType.FullName methodInfo.Name