namespace FEval.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open FEval
open FEval.Evaluations
open FEval.Tests.TestHelpers
open System.Collections.Generic

[<TestClass>]
type PerformanceInspectorTests() =
    let addMessageToList (list : List<string>) message =
        list.Add(message)

    let mockPerformanceInspectorConfig messageList : PerformanceInspector.Config =
        {
            HandleMessage = addMessageToList messageList
            PreMessageFormatter = PerformanceInspector.defaultPreMessageFormatter
            PostMessageFormatter = PerformanceInspector.defaultPostMessageFormatter
        }

    let assertMessages (expected : string array) (actual : List<string>) =
        Assert.AreEqual(expected.Length, actual.Count)
        Array.iteri (fun i s -> StringAssert.Contains(actual.[i], s, sprintf "Item %i" i)) expected

    let assertInspectors expr createInspectors expectedMessages =
        let messageList = new List<string>()
        evalWith expr <| createInspectors messageList |> ignore
        assertMessages expectedMessages messageList

    [<TestMethod>]
    member this.``Evaluate performance inspector - value``() = 
        assertInspectors
            <@ 4 @>
            (fun list -> [| PerformanceInspector.createNew <| mockPerformanceInspectorConfig list|])
            [| 
                "Start - Getting value 4 : Int32" 
                "End   - Got value 4 : Int32" 
            |]

    [<TestMethod>]
    member this.``Evaluate performance inspector - call static method``() = 
        assertInspectors
            <@ abs -3 @>
            (fun list -> [| PerformanceInspector.createNew <| mockPerformanceInspectorConfig list|])
            [| 
                "Start - Calling Operators.Abs(Int32)"
                "Start - Getting value -3 : Int32"
                "End   - Got value -3 : Int32"
                "End   - Called Operators.Abs(Int32), Returned 3 : Int32" 
            |]

    [<TestMethod>]
    member this.``Evaluate performance inspector - none union case``() = 
        assertInspectors
            <@ None @>
            (fun list -> [| PerformanceInspector.createNew <| mockPerformanceInspectorConfig list|])
            [| 
                "Start - Creating None : Option<Object>"
                "End   - Created None : Option<Object>" 
            |]
            
    [<TestMethod>]
    member this.``Evaluate performance inspector - some union case``() = 
        assertInspectors
            <@ Some 16 @>
            (fun list -> [| PerformanceInspector.createNew <| mockPerformanceInspectorConfig list|])
            [| 
                "Start - Creating Some : Option<Int32>"
                "Start - Getting value 16 : Int32" 
                "End   - Got value 16 : Int32" 
                "End   - Created Some 16 : Option<Int32>" 
            |]
            
    [<TestMethod>]
    member this.``Evaluate performance inspector - new record``() = 
        assertInspectors
            <@ { FirstName = "First" ; LastName = "Last" } @>
            (fun list -> [| PerformanceInspector.createNew <| mockPerformanceInspectorConfig list|])
            [| 
                "Start - Creating new Person"
                "Start - Getting value \"First\" : String" 
                "End   - Got value \"First\" : String" 
                "Start - Getting value \"Last\" : String" 
                "End   - Got value \"Last\" : String" 
                "End   - Created {FirstName = \"First\";\n LastName = \"Last\";} : Person"
            |]
    
    [<TestMethod>]
    member this.``Evaluate performance inspector - new tuple``() = 
        assertInspectors
            <@ (16, "Text", true) @>
            (fun list -> [| PerformanceInspector.createNew <| mockPerformanceInspectorConfig list|])
            [| 
                "Start - Creating new Tuple (Int32, String, Boolean)"
                "Start - Getting value 16 : Int32" 
                "End   - Got value 16 : Int32" 
                "Start - Getting value \"Text\" : String" 
                "End   - Got value \"Text\" : String" 
                "Start - Getting value true : Boolean" 
                "End   - Got value true : Boolean" 
                "End   - Created Tuple (16, \"Text\", true) : (Int32, String, Boolean)"
            |]
    
    [<TestMethod>]
    member this.``Evaluate performance inspector - let statement``() = 
        assertInspectors
            <@ let x = 18 in x @>
            (fun list -> [| PerformanceInspector.createNew <| mockPerformanceInspectorConfig list|])
            [| 
                "Start - Let x : Int32"
                "Start - Getting value 18 : Int32" 
                "End   - Got value 18 : Int32" 
                "Start - Getting variable x : Int32" 
                "End   - Got variable x, Returned 18 : Int32" 
                "End   - Let x returned 18"
            |]
            
    [<TestMethod>]
    member this.``Evaluate performance inspector - function application``() = 
        assertInspectors
            <@ let f x = x + 1 in f 3 @>
            (fun list -> [| PerformanceInspector.createNew <| mockPerformanceInspectorConfig list|])
            [| 
                "Start - Let f : (Int32 -> Int32)"
                "Start - Creating lambda (Int32 -> Int32)"
                "End   - Created lambda (Int32 -> Int32)"
                "Start - Applying function (Int32 -> Int32)"
                "Start - Getting variable f : (Int32 -> Int32)"
                "End   - Got variable f, Returned (Int32 -> Int32)"
                "Start - Getting value 3 : Int32"
                "End   - Got value 3 : Int32"
                "Start - Calling Operators.op_Addition(Int32, Int32)"
                "Start - Getting variable x : Int32"
                "End   - Got variable x, Returned 3 : Int32"
                "Start - Getting value 1 : Int32"
                "End   - Got value 1 : Int32"
                "End   - Called Operators.op_Addition(Int32, Int32), Returned 4 : Int32"
                "End   - Applyied function (Int32 -> Int32), Returned 4 : Int32"
                "End   - Let f returned 4 : Int32"
            |]
            
    [<TestMethod>]
    member this.``Evaluate performance inspector - call instance method``() = 
        assertInspectors
            <@ let x = 3 in x.ToString() @>
            (fun list -> [| PerformanceInspector.createNew <| mockPerformanceInspectorConfig list|])
            [| 
                "Start - Let x : Int32"
                "Start - Getting value 3 : Int32"
                "End   - Got value 3 : Int32"
                "Start - Calling Int32.ToString()"
                "Start - Getting variable x : Int32"
                "End   - Got variable x, Returned 3 : Int32"
                "End   - Called Int32.ToString(), Returned \"3\" : String" 
                "End   - Let x returned \"3\" : String"
            |]

    [<TestMethod>]
    member this.``Evaluate performance inspector - coerce to object``() = 
        assertInspectors
            <@ let x = "Hello" in x :> obj @>
            (fun list -> [| PerformanceInspector.createNew <| mockPerformanceInspectorConfig list|])
            [| 
                "Start - Let x : String"
                "Start - Getting value \"Hello\" : String"
                "End   - Got value \"Hello\" : String"
                "Start - Coercing String to Object"
                "Start - Getting variable x : String"
                "End   - Got variable x, Returned \"Hello\" : String"
                "End   - Coerced String to Object"
                "End   - Let x returned \"Hello\" : String (Object)"
            |]
        
    [<TestMethod>]
    member this.``Evaluate performance inspector - create new object``() = 
        assertInspectors
            <@ new ChildClass("Hello") @>
            (fun list -> [| PerformanceInspector.createNew <| mockPerformanceInspectorConfig list|])
            [| 
                "Start - Creating new object ChildClass(String)"
                "Start - Getting value \"Hello\" : String"
                "End   - Got value \"Hello\" : String"
                "End   - Created new object ChildClass"
            |]
            
    [<TestMethod>]
    member this.``Evaluate performance inspector - get property``() = 
        assertInspectors
            <@ let x = new ChildClass("Hello") in x.NameProperty @>
            (fun list -> [| PerformanceInspector.createNew <| mockPerformanceInspectorConfig list|])
            [| 
                "Start - Let x : ChildClass"
                "Start - Creating new object ChildClass(String)"
                "Start - Getting value \"Hello\" : String"
                "End   - Got value \"Hello\" : String"
                "End   - Created new object ChildClass"
                "Start - Getting property ChildClass.NameProperty"
                "Start - Getting variable x : ChildClass"
                "End   - Got variable x, Returned ChildClass"
                "End   - Got property ChildClass.NameProperty, Returned \"Hello\" : String"
                "End   - Let x returned \"Hello\" : String"
            |]
            
    [<TestMethod>]
    member this.``Evaluate performance inspector - set property``() = 
        assertInspectors
            <@ 
            let child = new ChildClass("Hello")
            child.NameProperty <- "World"
            child.NameProperty 
            @>
            (fun list -> [| PerformanceInspector.createNew <| mockPerformanceInspectorConfig list|])
            [| 
                "Start - Let child : ChildClass"
                "Start - Creating new object ChildClass(String)"
                "Start - Getting value \"Hello\" : String"
                "End   - Got value \"Hello\" : String"
                "End   - Created new object ChildClass"
                "Start - Performing PropertySet and then PropertyGet"
                "Start - Setting property ChildClass.NameProperty"
                "Start - Getting variable child : ChildClass"
                "End   - Got variable child, Returned ChildClass"
                "Start - Getting value \"World\" : String"
                "End   - Got value \"World\" : String"
                "End   - Set property ChildClass.NameProperty"
                "Start - Getting property ChildClass.NameProperty"
                "Start - Getting variable child : ChildClass"
                "End   - Got variable child, Returned ChildClass"
                "End   - Got property ChildClass.NameProperty, Returned \"World\" : String"
                "End   - Performed PropertySet and then PropertyGet"
                "End   - Let child returned \"World\" : String"
            |]
            
    [<TestMethod>]
    member this.``Evaluate performance inspector - get indexer property``() = 
        assertInspectors
            <@ 
            let indexerClass = new IndexerClass()
            indexerClass.[2]
            @>
            (fun list -> [| PerformanceInspector.createNew <| mockPerformanceInspectorConfig list|])
            [| 
                "Start - Let indexerClass : IndexerClass"
                "Start - Creating new object IndexerClass()"
                "End   - Created new object IndexerClass"
                "Start - Getting property IndexerClass.Item"
                "Start - Getting variable indexerClass : IndexerClass"
                "End   - Got variable indexerClass, Returned IndexerClass"
                "Start - Getting value 2 : Int32"
                "End   - Got value 2 : Int32"
                "End   - Got property IndexerClass.Item[Int32]"
                "End   - Let indexerClass returned \"three\" : String"
            |]
            
    [<TestMethod>]
    member this.``Evaluate performance inspector - set indexer property``() = 
        assertInspectors
            <@ 
            let indexerClass = new IndexerClass()
            indexerClass.[2] <- "Lovely Two"
            @>
            (fun list -> [| PerformanceInspector.createNew <| mockPerformanceInspectorConfig list|])
            [| 
                "Start - Let indexerClass : IndexerClass"
                "Start - Creating new object IndexerClass()"
                "End   - Created new object IndexerClass"
                "Start - Setting property IndexerClass.Item[Int32]"
                "Start - Getting variable indexerClass : IndexerClass"
                "End   - Got variable indexerClass, Returned IndexerClass"
                "Start - Getting value \"Lovely Two\" : String"
                "End   - Got value \"Lovely Two\" : String"
                "Start - Getting value 2 : Int32"
                "End   - Got value 2 : Int32"
                "End   - Set property IndexerClass.Item[Int32]"
                "End   - Let indexerClass returned Unit"
            |]
            
    [<TestMethod>]
    member this.``Evaluate performance inspector - struct default value``() = 
        assertInspectors
            <@  let struct1 = new Struct() in struct1             @>
            (fun list -> [| PerformanceInspector.createNew <| mockPerformanceInspectorConfig list|])
            [| 
                "Start - Let struct1 : Struct"
                "Start - Creating default value for Struct"
                "End   - Created default value for Struct"
                "Start - Getting variable struct1 : Struct"
                "End   - Got variable struct1, Returned Struct"
                "End   - Let struct1 returned Struct"
            |]            