namespace FEval

[<RequireQualifiedAccess>]
module internal Reflection =
    open System
    open System.Reflection
    open Microsoft.FSharp.Reflection
    open Microsoft.FSharp.Quotations

    let private typeofExpr = typeof<Expr>
    let private genericExprTypeDefinition = typeof<Expr<obj>>.GetGenericTypeDefinition()
    
    let private getPrivateProperty (targetType : Type) target name =
        targetType.InvokeMember(
            name, 
            BindingFlags.GetProperty ||| BindingFlags.Instance ||| BindingFlags.NonPublic,
            null, 
            target, 
            null)

    let private getPrivateCtors (targetType : Type) =
        targetType.GetConstructors(BindingFlags.NonPublic ||| BindingFlags.Instance)

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
    
    let invokeGetField instance (fieldinfo : FieldInfo) =
        fieldinfo.GetValue(instance)
        
    let invokeSetField instance (fieldinfo : FieldInfo) value =
        fieldinfo.SetValue(instance, value)

    let getInvokeMethodInfo instance =
        let instanceType = instance.GetType()
        instanceType.GetMethods()
        |> Seq.filter (fun m -> m.Name = "Invoke")
        |> Seq.filter (fun m -> m.GetParameters().Length = 1)
        |> Seq.item 0

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

    let getMethodFullName (methodInfo : MethodInfo) =
        sprintf "%s.%s" methodInfo.DeclaringType.FullName methodInfo.Name

    let getTupleField index tuple =
        FSharpValue.GetTupleField (tuple, index)

    let getUnionCaseInfo union =
        FSharpValue.GetUnionFields (union, union.GetType()) |> fst

    let getType obj =
        obj.GetType()

    let convertExprToTyped (expr : Expr) targetType =
        let genericExprType = genericExprTypeDefinition.MakeGenericType([| targetType |])
        let tree = getPrivateProperty typeofExpr expr "Tree"
        let ctors = getPrivateCtors genericExprType
        
        ctors.[0].Invoke [| tree ; expr.CustomAttributes |]

    let getReflectedMethodDefinition methodInfo =
        Expr.TryGetReflectedDefinition methodInfo