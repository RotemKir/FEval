namespace FEval.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open FEval.Evaluations
open FEval.Inspectors
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

    [<TestMethod>]
    member this.``Evaluate performance inspector - Value``() = 
        let messageList = new List<string>()
        evalWith 
            <@ 4 @>
            [| performanceInspector <| mockPerformanceInspectorConfig messageList|]
        |> ignore
        assertMessages
            [| "Start - Get value: 4 (Int32)" ; "End - Get value: 4 (Int32)" |]
            messageList

    [<TestMethod>]
    member this.``Evaluate performance inspector - Call static method``() = 
        let messageList = new List<string>()
        evalWith 
            <@ abs -3 @>
            [| performanceInspector <| mockPerformanceInspectorConfig messageList|]
        |> ignore
        assertMessages
            [| 
                "Start - Call Abs"
                "Start - Get value: -3 (Int32)"
                "End - Get value: -3 (Int32)"
                "End - Called Abs, Returned: 3 (Int32)" 
            |]
            messageList