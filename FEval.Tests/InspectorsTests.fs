namespace FEval.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open FEval.Evaluations
open FEval.Inspectors
open FEval.Tests.TestHelpers
open System.Collections.Generic

[<TestClass>]
type InspectorsTest() =
    let addMessageToList (list : List<string>) message =
        list.Add(message)

    let mockPerformanceInspectorConfig messageList =
        {
            HandleMessage = addMessageToList messageList
            PreMessageFormatter = defaultPerformancePreMessageFormatter
            PostMessageFormatter = defaultPostformancePreMessageFormatter
        }

    let assertMessages (expected : string array) (actual : List<string>) =
        Assert.AreEqual(expected.Length, actual.Count)
        Array.iteri (fun i s -> StringAssert.Contains(actual.[i], s)) expected

    let assertInspectors expr createInspectors expectedMessages =
        let messageList = new List<string>()
        evalWith expr <| createInspectors messageList |> ignore
        assertMessages expectedMessages messageList

    [<TestMethod>]
    member this.``Evaluate performance inspector - value``() = 
        assertInspectors
            <@ 4 @>
            (fun list -> [| performanceInspector <| mockPerformanceInspectorConfig list|])
            [| 
                "Start - Get value: 4 (Int32)" 
                "End - Get value: 4 (Int32)" 
            |]

    [<TestMethod>]
    member this.``Evaluate performance inspector - call static method``() = 
        assertInspectors
            <@ abs -3 @>
            (fun list -> [| performanceInspector <| mockPerformanceInspectorConfig list|])
            [| 
                "Start - Calling Abs"
                "Start - Get value: -3 (Int32)"
                "End - Get value: -3 (Int32)"
                "End - Called Abs, Returned: 3 (Int32)" 
            |]

    [<TestMethod>]
    member this.``Evaluate performance inspector - none union case``() = 
        assertInspectors
            <@ None @>
            (fun list -> [| performanceInspector <| mockPerformanceInspectorConfig list|])
            [| 
                "Start - Creating None (FSharpOption`1)"
                "End - Created None (FSharpOption`1)" 
            |]
            
    [<TestMethod>]
    member this.``Evaluate performance inspector - some union case``() = 
        assertInspectors
            <@ Some 16 @>
            (fun list -> [| performanceInspector <| mockPerformanceInspectorConfig list|])
            [| 
                "Start - Creating Some (FSharpOption`1)"
                "Start - Get value: 16 (Int32)" 
                "End - Get value: 16 (Int32)" 
                "End - Created Some(16) (FSharpOption`1)" 
            |]
            
    [<TestMethod>]
    member this.``Evaluate performance inspector - new record``() = 
        assertInspectors
            <@ { FirstName = "First" ; LastName = "Last" } @>
            (fun list -> [| performanceInspector <| mockPerformanceInspectorConfig list|])
            [| 
                "Start - Creating new Person"
                "Start - Get value: First (String)" 
                "End - Get value: First (String)" 
                "Start - Get value: Last (String)" 
                "End - Get value: Last (String)" 
                "End - Created {FirstName = \"First\";\n LastName = \"Last\";} (Person)"
            |]
    
    [<TestMethod>]
    member this.``Evaluate performance inspector - new tuple``() = 
        assertInspectors
            <@ (16, "Text", true) @>
            (fun list -> [| performanceInspector <| mockPerformanceInspectorConfig list|])
            [| 
                "Start - Creating new Tuple (Int32, String, Boolean)"
                "Start - Get value: 16 (Int32)" 
                "End - Get value: 16 (Int32)" 
                "Start - Get value: Text (String)" 
                "End - Get value: Text (String)" 
                "Start - Get value: True (Boolean)" 
                "End - Get value: True (Boolean)" 
                "End - Created Tuple (16, Text, True) (Int32, String, Boolean)"
            |]
    
    [<TestMethod>]
    member this.``Evaluate performance inspector - let statement``() = 
        assertInspectors
            <@ let x = 18 in x @>
            (fun list -> [| performanceInspector <| mockPerformanceInspectorConfig list|])
            [| 
                "Start - Let x (Int32)"
                "Start - Get value: 18 (Int32)" 
                "End - Get value: 18 (Int32)" 
                "Start - Get variable x (Int32)" 
                "End - Get variable x, Returned 18 (Int32)" 
                "End - Let x returned 18"
            |]