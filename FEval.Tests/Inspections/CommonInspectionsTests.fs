namespace FEval.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open FEval.Inspections.CommonInspections
open FEval.Tests.TestHelpers

[<TestClass>]
type CommonInspectionsTests() =
    [<TestMethod>]
    member this.``getDeclaringType - has no instance - returns declaring type``() = 
        let result = getDeclaringType None typeof<int>
        Assert.AreEqual(typeof<int>, result)
        
    [<TestMethod>]
    member this.``getDeclaringType - has instance - returns instance type``() = 
        let result = getDeclaringType <| createValueExpr<string>() <| typeof<int>
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

        
        