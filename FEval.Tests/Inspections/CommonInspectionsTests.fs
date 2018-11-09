namespace FEval.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open FEval.CommonInspections
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
