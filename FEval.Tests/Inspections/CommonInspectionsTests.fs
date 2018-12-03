namespace FEval.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open FEval.Inspections.InspectionsCommon
open FEval.Tests.TestHelpers

[<TestClass>]
type CommonInspectionsTests() =
    
    [<TestMethod>]
    member this.``getExprType - has no instance - returns default type``() = 
        let result = getExprType None typeof<int>
        Assert.AreEqual(typeof<int>, result)
        
    [<TestMethod>]
    member this.``getExprType - has instance - returns instance type``() = 
        let result = getExprType <| createValueExpr<string>() <| typeof<int>
        Assert.AreEqual(typeof<string>, result)
    
    [<TestMethod>]
    member this.``getInstanceType - has no instance - returns default type``() = 
        let result = getInstanceType null typeof<int>
        Assert.AreEqual(typeof<int>, result)
        
    [<TestMethod>]
    member this.``getInstanceType - has instance - returns instance type``() = 
        let result = getInstanceType "Hello" <| typeof<int>
        Assert.AreEqual(typeof<string>, result)
    

    [<TestMethod>]
    member this.``getTupleItemType - returns item type by index``() = 
        let result = getTupleItemType <| typeof<int * string> <| 1
        Assert.AreEqual(typeof<string>, result)
        
    [<TestMethod>]
    member this.``getFunctionReturnType - function with two parameters - returns return type``() = 
        let result = getFunctionReturnType <| typeof<int -> string>
        Assert.AreEqual(typeof<string>, result)

    [<TestMethod>]
    member this.``getFunctionReturnType - function with three parameters - returns return type``() = 
        let result = getFunctionReturnType <| typeof<int -> string -> bool>
        Assert.AreEqual(typeof<string -> bool>, result)

        
        