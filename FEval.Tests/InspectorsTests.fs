﻿namespace FEval.Tests

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
    member this.``Evaluate performance inspector - Value``() = 
        assertInspectors
            <@ 4 @>
            (fun list -> [| performanceInspector <| mockPerformanceInspectorConfig list|])
            [| 
                "Start - Get value: 4 (Int32)" 
                "End - Get value: 4 (Int32)" 
            |]

    [<TestMethod>]
    member this.``Evaluate performance inspector - Call static method``() = 
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
    member this.``Evaluate performance inspector - None union case``() = 
        assertInspectors
            <@ None @>
            (fun list -> [| performanceInspector <| mockPerformanceInspectorConfig list|])
            [| 
                "Start - Creating None (FSharpOption`1)"
                "End - Created None (FSharpOption`1)" 
            |]
            
    [<TestMethod>]
    member this.``Evaluate performance inspector - Some union case``() = 
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