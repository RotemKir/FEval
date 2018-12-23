﻿namespace FEval.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open FEval.Inspectors.InspectionsCommon
open FEval.Tests.TestHelpers

[<TestClass>]
type CommonInspectionsTests() =
    
    [<TestMethod>]
    member __.``getExprType - has no instance - returns default type``() = 
        let result = getExprType None typeof<int>
        Assert.AreEqual(typeof<int>, result)
        
    [<TestMethod>]
    member __.``getExprType - has instance - returns instance type``() = 
        let result = getExprType <| createValueExpr<string>() <| typeof<int>
        Assert.AreEqual(typeof<string>, result)
    
    [<TestMethod>]
    member __.``getInstanceType - has no instance - returns default type``() = 
        let result = getInstanceType null typeof<int>
        Assert.AreEqual(typeof<int>, result)
        
    [<TestMethod>]
    member __.``getInstanceType - has instance - returns instance type``() = 
        let result = getInstanceType "Hello" <| typeof<int>
        Assert.AreEqual(typeof<string>, result)
    

    [<TestMethod>]
    member __.``getTupleItemType - returns item type by index``() = 
        let result = getTupleItemType <| typeof<int * string> <| 1
        Assert.AreEqual(typeof<string>, result)
        
    [<TestMethod>]
    member __.``getFunctionReturnType - function with two parameters - returns return type``() = 
        let result = getFunctionReturnType <| typeof<int -> string>
        Assert.AreEqual(typeof<string>, result)

    [<TestMethod>]
    member __.``getFunctionReturnType - function with three parameters - returns return type``() = 
        let result = getFunctionReturnType <| typeof<int -> string -> bool>
        Assert.AreEqual(typeof<string -> bool>, result)

        
        