namespace FEval.Tests

open System
open FEval.Evaluations
open FEval.Tests.TestHelpers
open Microsoft.VisualStudio.TestTools.UnitTesting
open Microsoft.FSharp.Quotations

[<TestClass>]
type EvaluationsTest() =
    let assertEval expr expectedResult =
        Assert.AreEqual(expectedResult, eval expr)

    let collectionAssertEval expr expectedResult =
        CollectionAssert.AreEqual(expectedResult, eval expr)

    let listAssertEval expr expectedResult =
        CollectionAssert.AreEqual(expectedResult |> List.toArray, eval expr |> List.toArray)

    [<TestMethod>]
    member this.``Evaluate Const Int32``() = 
        assertEval <@ 4 @> 4

    [<TestMethod>]
    member this.``Evaluate Const string``() = 
        assertEval <@ "Hello World" @> "Hello World"

    [<TestMethod>]
    member this.``Evaluate Const bool``() = 
        assertEval <@ true @> true

    [<TestMethod>]
    member this.``Evaluate Const float``() = 
        assertEval <@ 0.87 @> 0.87
 
    [<TestMethod>]
    member this.``Evaluate Const decimal``() = 
        assertEval <@ 0.87m @> 0.87m

    [<TestMethod>]
    member this.``Evaluate None``() = 
        assertEval <@ None @> None

    [<TestMethod>]
    member this.``Evaluate Some number``() = 
        assertEval <@ Some 4 @> <| Some 4
    
    [<TestMethod>]
    member this.``Evaluate Some simple addition``() = 
        assertEval <@ Some (6 + 9) @> <| Some 15

    [<TestMethod>]
    member this.``Evaluate static method call with one parameter``() = 
        assertEval <@ Math.Abs(-3) @> 3

    [<TestMethod>]
    member this.``Evaluate 2 static method calls with one parameter``() = 
        assertEval <@ Math.Ceiling(Math.Abs(-5.67)) @> <| double 6
        
    [<TestMethod>]
    member this.``Evaluate static method call with 2 parameters of same type``() = 
        assertEval <@ Math.Max(10, 75) @> 75

    [<TestMethod>]
    member this.``Evaluate static method call with 2 parameters of different type``() = 
        assertEval <@ Math.Round(0.1234, 2) @> 0.12
    
    [<TestMethod>]
    member this.``Evaluate unary operator``() = 
        assertEval <@ not true @> false

    [<TestMethod>]
    member this.``Evaluate binary operator``() = 
        assertEval <@ 7 + 14 @> 21
    
    [<TestMethod>]
    member this.``Evaluate several operators chain``() = 
        assertEval <@ 7 + 14 * 9 @> 133
    
    [<TestMethod>]
    member this.``Evaluate a record type``() = 
        assertEval <@ { FirstName = "First" ; LastName = "Last" } @> { FirstName = "First" ; LastName = "Last" } 
        
    [<TestMethod>]
    member this.``Evaluate a tuple``() = 
        assertEval <@ (16, "String", true) @> (16, "String", true) 

    [<TestMethod>]
    member this.``Evaluate a simple let statement``() = 
        assertEval <@ let x = 3 in x @> 3 

    [<TestMethod>]
    member this.``Evaluate a let statement with addition``() = 
        assertEval <@ let x = 3 in x + 5 @> 8

    [<TestMethod>]
    member this.``Evaluate a statement with right pipeline``() = 
        collectionAssertEval <@ 3 |> Array.create 5 @> [| 3 ; 3 ; 3 ; 3 ; 3 |]
         
    [<TestMethod>]
    member this.``Evaluate a statement with left pipeline``() = 
        collectionAssertEval <@ Array.create 5 <| true @> [| true ; true; true ; true ; true |]
    
    [<TestMethod>]
    member this.``Evaluate new array``() = 
        collectionAssertEval <@ [|1;2;3;4;5|] @> [|1;2;3;4;5|]
    
    [<TestMethod>]
    member this.``Evaluate new list``() = 
        listAssertEval <@ [1;2;3;4;5] @> [1;2;3;4;5]
