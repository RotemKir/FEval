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
                "Start - Get value 4 : Int32" 
                "End - Get value 4 : Int32" 
            |]

    [<TestMethod>]
    member this.``Evaluate performance inspector - call static method``() = 
        assertInspectors
            <@ abs -3 @>
            (fun list -> [| performanceInspector <| mockPerformanceInspectorConfig list|])
            [| 
                "Start - Calling Abs"
                "Start - Get value -3 : Int32"
                "End - Get value -3 : Int32"
                "End - Called Abs, Returned 3 : Int32" 
            |]

    [<TestMethod>]
    member this.``Evaluate performance inspector - none union case``() = 
        assertInspectors
            <@ None @>
            (fun list -> [| performanceInspector <| mockPerformanceInspectorConfig list|])
            [| 
                "Start - Creating None : Option"
                "End - Created None : Option" 
            |]
            
    [<TestMethod>]
    member this.``Evaluate performance inspector - some union case``() = 
        assertInspectors
            <@ Some 16 @>
            (fun list -> [| performanceInspector <| mockPerformanceInspectorConfig list|])
            [| 
                "Start - Creating Some : Option"
                "Start - Get value 16 : Int32" 
                "End - Get value 16 : Int32" 
                "End - Created Some 16 : Option" 
            |]
            
    [<TestMethod>]
    member this.``Evaluate performance inspector - new record``() = 
        assertInspectors
            <@ { FirstName = "First" ; LastName = "Last" } @>
            (fun list -> [| performanceInspector <| mockPerformanceInspectorConfig list|])
            [| 
                "Start - Creating new Person"
                "Start - Get value \"First\" : String" 
                "End - Get value \"First\" : String" 
                "Start - Get value \"Last\" : String" 
                "End - Get value \"Last\" : String" 
                "End - Created {FirstName = \"First\";\n LastName = \"Last\";} : Person"
            |]
    
    [<TestMethod>]
    member this.``Evaluate performance inspector - new tuple``() = 
        assertInspectors
            <@ (16, "Text", true) @>
            (fun list -> [| performanceInspector <| mockPerformanceInspectorConfig list|])
            [| 
                "Start - Creating new Tuple (Int32, String, Boolean)"
                "Start - Get value 16 : Int32" 
                "End - Get value 16 : Int32" 
                "Start - Get value \"Text\" : String" 
                "End - Get value \"Text\" : String" 
                "Start - Get value true : Boolean" 
                "End - Get value true : Boolean" 
                "End - Created Tuple (16, \"Text\", true) : (Int32, String, Boolean)"
            |]
    
    [<TestMethod>]
    member this.``Evaluate performance inspector - let statement``() = 
        assertInspectors
            <@ let x = 18 in x @>
            (fun list -> [| performanceInspector <| mockPerformanceInspectorConfig list|])
            [| 
                "Start - Let x : Int32"
                "Start - Get value 18 : Int32" 
                "End - Get value 18 : Int32" 
                "Start - Get variable x : Int32" 
                "End - Get variable x, Returned 18 : Int32" 
                "End - Let x returned 18"
            |]
            
    [<TestMethod>]
    member this.``Evaluate performance inspector - function application``() = 
        assertInspectors
            <@ let f x = x + 1 in f 3 @>
            (fun list -> [| performanceInspector <| mockPerformanceInspectorConfig list|])
            [| 
                "Start - Let f : (Int32 -> Int32)"
                "Start - Creating lambda (Int32 -> Int32)"
                "End - Created lambda (Int32 -> Int32)"
                "Start - Applying function (Int32 -> Int32)"
                "Start - Get variable f : (Int32 -> Int32)"
                "End - Get variable f, Returned (Int32 -> Int32)"
                "Start - Get value 3 : Int32"
                "End - Get value 3 : Int32"
                "Start - Calling op_Addition"
                "Start - Get variable x : Int32"
                "End - Get variable x, Returned 3 : Int32"
                "Start - Get value 1 : Int32"
                "End - Get value 1 : Int32"
                "End - Called op_Addition, Returned 4 : Int32"
                "End - Applyied function (Int32 -> Int32), Returned 4 : Int32"
                "End - Let f returned 4 : Int32"
            |]
            
    [<TestMethod>]
    member this.``Evaluate performance inspector - call instance method``() = 
        assertInspectors
            <@ let x = 3 in x.ToString() @>
            (fun list -> [| performanceInspector <| mockPerformanceInspectorConfig list|])
            [| 
                "Start - Let x : Int32"
                "Start - Get value 3 : Int32"
                "End - Get value 3 : Int32"
                "Start - Calling Int32.ToString"
                "Start - Get variable x : Int32"
                "End - Get variable x, Returned 3 : Int32"                
                "End - Called Int32.ToString, Returned \"3\" : String" 
                "End - Let x returned \"3\" : String"
            |]

    [<TestMethod>]
    member this.``Evaluate performance inspector - coerce to object``() = 
        assertInspectors
            <@ let x = "Hello" in x :> obj @>
            (fun list -> [| performanceInspector <| mockPerformanceInspectorConfig list|])
            [| 
                "Start - Let x : String"
                "Start - Get value \"Hello\" : String"
                "End - Get value \"Hello\" : String"
                "Start - Coercing String to Object"
                "Start - Get variable x : String"
                "End - Get variable x, Returned \"Hello\" : String"                
                "End - Coerced String to Object"
                "End - Let x returned \"Hello\" : Object"
            |]
        
    [<TestMethod>]
    member this.``Evaluate performance inspector - create new object``() = 
        assertInspectors
            <@ new ChildClass("Hello") @>
            (fun list -> [| performanceInspector <| mockPerformanceInspectorConfig list|])
            [| 
                "Start - Creating new object ChildClass (String)"
                "Start - Get value \"Hello\" : String"
                "End - Get value \"Hello\" : String"
                "End - Created new object ChildClass"
            |]