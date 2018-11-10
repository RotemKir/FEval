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
                "Start - Creating new tuple (Int32, String, Boolean)"
                "Start - Getting value 16 : Int32" 
                "End   - Got value 16 : Int32" 
                "Start - Getting value \"Text\" : String" 
                "End   - Got value \"Text\" : String" 
                "Start - Getting value true : Boolean" 
                "End   - Got value true : Boolean" 
                "End   - Created tuple (16, \"Text\", true) : (Int32, String, Boolean)"
            |]
    
    [<TestMethod>]
    member this.``Evaluate performance inspector - new array``() = 
        assertInspectors
            <@ [|1 ; 2 ; 3|] @>
            (fun list -> [| PerformanceInspector.createNew <| mockPerformanceInspectorConfig list|])
            [| 
                "Start - Creating new array Int32"
                "Start - Getting value 1 : Int32" 
                "End   - Got value 1 : Int32" 
                "Start - Getting value 2 : Int32" 
                "End   - Got value 2 : Int32" 
                "Start - Getting value 3 : Int32" 
                "End   - Got value 3 : Int32" 
                "End   - Created array [|1; 2; 3|] : Int32"
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
                "End   - Applied function (Int32 -> Int32), Returned 4 : Int32"
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
            <@  let struct1 = new Struct() in struct1 @>
            (fun list -> [| PerformanceInspector.createNew <| mockPerformanceInspectorConfig list|])
            [| 
                "Start - Let struct1 : Struct"
                "Start - Creating default value for Struct"
                "End   - Created default value for Struct"
                "Start - Getting variable struct1 : Struct"
                "End   - Got variable struct1, Returned Struct"
                "End   - Let struct1 returned Struct"
            |]       
            
    [<TestMethod>]
    member this.``Evaluate performance inspector - get field``() = 
        assertInspectors
            <@  let field = new FieldClass(54) in field.number @>
            (fun list -> [| PerformanceInspector.createNew <| mockPerformanceInspectorConfig list|])
            [| 
                "Start - Let field : FieldClass"
                "Start - Creating new object FieldClass(Int32)"
                "Start - Getting value 54 : Int32"
                "End   - Got value 54 : Int32"
                "End   - Created new object FieldClass"
                "Start - Getting field FieldClass.number"
                "Start - Getting variable field : FieldClass"
                "End   - Got variable field, Returned FieldClass"
                "End   - Got field FieldClass.number, Returned 54"
                "End   - Let field returned 54 : Int32"
            |]
            
    [<TestMethod>]
    member this.``Evaluate performance inspector - set field``() = 
        assertInspectors
            <@  let field = new FieldClass(54) in field.number <- 73 @>
            (fun list -> [| PerformanceInspector.createNew <| mockPerformanceInspectorConfig list|])
            [| 
                "Start - Let field : FieldClass"
                "Start - Creating new object FieldClass(Int32)"
                "Start - Getting value 54 : Int32"
                "End   - Got value 54 : Int32"
                "End   - Created new object FieldClass"
                "Start - Setting field FieldClass.number"
                "Start - Getting variable field : FieldClass"
                "End   - Got variable field, Returned FieldClass"
                "Start - Getting value 73 : Int32"
                "End   - Got value 73 : Int32"
                "End   - Set field FieldClass.number"
                "End   - Let field returned Unit"
            |]
            
    [<TestMethod>]
    member this.``Evaluate performance inspector - var set``() = 
        assertInspectors
            <@  let mutable x = 3 in x <- 18 @>
            (fun list -> [| PerformanceInspector.createNew <| mockPerformanceInspectorConfig list|])
            [| 
                "Start - Let x : Int32"
                "Start - Getting value 3 : Int32"
                "End   - Got value 3 : Int32"
                "Start - Setting variable x : Int32"
                "Start - Getting value 18 : Int32"
                "End   - Got value 18 : Int32"
                "End   - Set variable x : Int32"
                "End   - Let x returned Unit"
            |]
            
    [<TestMethod>]
    member this.``Evaluate performance inspector - for loop``() = 
        assertInspectors
            <@  let mutable x = 0 in for i = 1 to 3 do x <- x + i @>
            (fun list -> [| PerformanceInspector.createNew <| mockPerformanceInspectorConfig list|])
            [| 
                "Start - Let x : Int32"
                "Start - Getting value 0 : Int32"
                "End   - Got value 0 : Int32"
                "Start - Running for loop on i : Int32"
                "Start - Getting value 1 : Int32"
                "End   - Got value 1 : Int32"
                "Start - Getting value 3 : Int32"
                "End   - Got value 3 : Int32"
                // Loop #1
                "Start - Setting variable x : Int32"
                "Start - Calling Operators.op_Addition(Int32, Int32)"
                "Start - Getting variable x : Int32"
                "End   - Got variable x, Returned 0 : Int32"
                "Start - Getting variable i : Int32"
                "End   - Got variable i, Returned 1 : Int32"
                "End   - Called Operators.op_Addition(Int32, Int32), Returned 1 : Int32"
                "End   - Set variable x : Int32"                
                // Loop #2               
                "Start - Setting variable x : Int32"
                "Start - Calling Operators.op_Addition(Int32, Int32)"
                "Start - Getting variable x : Int32"
                "End   - Got variable x, Returned 1 : Int32"
                "Start - Getting variable i : Int32"
                "End   - Got variable i, Returned 2 : Int32"
                "End   - Called Operators.op_Addition(Int32, Int32), Returned 3 : Int32"
                "End   - Set variable x : Int32"
                // Loop #3
                "Start - Setting variable x : Int32"
                "Start - Calling Operators.op_Addition(Int32, Int32)"
                "Start - Getting variable x : Int32"
                "End   - Got variable x, Returned 3 : Int32"
                "Start - Getting variable i : Int32"
                "End   - Got variable i, Returned 3 : Int32"
                "End   - Called Operators.op_Addition(Int32, Int32), Returned 6 : Int32"
                "End   - Set variable x : Int32"
                "End   - Ran for loop on i : Int32"
                "End   - Let x returned Unit"
            |]

    [<TestMethod>]
    member this.``Evaluate performance inspector - if then else``() = 
        assertInspectors
            <@ if true then 4 else 5 @>
            (fun list -> [| PerformanceInspector.createNew <| mockPerformanceInspectorConfig list|])
            [| 
                "Start - Evaluating if"
                "Start - Getting value true : Boolean"
                "End   - Got value true : Boolean"
                "Start - Getting value 4 : Int32"
                "End   - Got value 4 : Int32"
                "End   - Evaluated if, Returned 4 : Int32"
            |]
            
    [<TestMethod>]
    member this.``Evaluate performance inspector - tuple get``() = 
        assertInspectors
            <@ let (a, b) = (1, 2) in a @>
            (fun list -> [| PerformanceInspector.createNew <| mockPerformanceInspectorConfig list|])
            [| 
                "Start - Let patternInput : (Int32, Int32)"
                "Start - Creating new tuple (Int32, Int32)"
                "Start - Getting value 1 : Int32"
                "End   - Got value 1 : Int32"
                "Start - Getting value 2 : Int32"
                "End   - Got value 2 : Int32"
                "End   - Created tuple (1, 2) : (Int32, Int32)"
                "Start - Let b : Int32"
                "Start - Getting value from tuple (Int32, Int32) at index 1"
                "Start - Getting variable patternInput : (Int32, Int32)"
                "End   - Got variable patternInput, Returned (1, 2) : (Int32, Int32)"
                "End   - Got value from tuple (Int32, Int32) at index 1, Retuned 2 : Int32"
                "Start - Let a : Int32"
                "Start - Getting value from tuple (Int32, Int32) at index 0"
                "Start - Getting variable patternInput : (Int32, Int32)"
                "End   - Got variable patternInput, Returned (1, 2) : (Int32, Int32)"
                "End   - Got value from tuple (Int32, Int32) at index 0, Retuned 1 : Int32"
                "Start - Getting variable a : Int32"
                "End   - Got variable a, Returned 1"
                "End   - Let a returned 1 : Int32"
                "End   - Let b returned 1 : Int32"
                "End   - Let patternInput returned 1 : Int32"
            |]
            
    [<TestMethod>]
    member this.``Evaluate performance inspector - union case test``() = 
        assertInspectors
            <@ match UnionB with | UnionA -> true | _ -> false @>
            (fun list -> [| PerformanceInspector.createNew <| mockPerformanceInspectorConfig list|])
            [| 
                "Start - Let matchValue : Union"
                "Start - Creating UnionB : Union"
                "End   - Created UnionB : Union"
                "Start - Evaluating if"
                "Start - Checking if union matches UnionA : Union"
                "Start - Getting variable matchValue : Union"
                "End   - Got variable matchValue, Returned UnionB : Union"
                "End   - Checked if union matches UnionA : Union, Returned false : Boolean"
                "Start - Getting value false : Boolean"
                "End   - Got value false : Boolean"
                "End   - Evaluated if, Returned false : Boolean"
                "End   - Let matchValue returned false : Boolean"
            |]
            
    [<TestMethod>]
    member this.``Evaluate performance inspector - type test``() = 
        assertInspectors
            <@ 
            let x : obj = 6 :> obj           
            match x with | :? int -> true | _ -> false
            @>
            (fun list -> [| PerformanceInspector.createNew <| mockPerformanceInspectorConfig list|])
            [| 
                "Start - Let x : Object"
                "Start - Coercing Int32 to Object"
                "Start - Getting value 6 : Int32"
                "End   - Got value 6 : Int32"
                "End   - Coerced Int32 to Object"
                "Start - Evaluating if"
                "Start - Testing if Object is Int32"
                "Start - Getting variable x : Object"
                "End   - Got variable x, Returned 6 : Int32 (Object)"
                "End   - Tested if Object is Int32, Returned true : Boolean"
                "Start - Getting value true : Boolean"
                "End   - Got value true : Boolean"
                "End   - Evaluated if, Returned true : Boolean"
                "End   - Let x returned true : Boolean"
            |]
            
    [<TestMethod>]
    member this.``Evaluate performance inspector - try with``() = 
        assertInspectors
            <@ try 4 with | _ -> 5 @>
            (fun list -> [| PerformanceInspector.createNew <| mockPerformanceInspectorConfig list|])
            [| 
                "Start - Handling with try with"
                "Start - Getting value 4 : Int32"
                "End   - Got value 4 : Int32"
                "End   - Handled with try with, Returned 4 : Int32"
            |]
            
    [<TestMethod>]
    member this.``Evaluate performance inspector - try finally``() = 
        assertInspectors
            <@ try 4 finally ignore() @>
            (fun list -> [| PerformanceInspector.createNew <| mockPerformanceInspectorConfig list|])
            [| 
                "Start - Handling with try finally"
                "Start - Getting value 4 : Int32"
                "End   - Got value 4 : Int32"
                "Start - Calling Operators.Ignore(Unit)"
                "Start - Getting value Unit"
                "End   - Got value Unit"
                "End   - Called Operators.Ignore(Unit), Returned Void"
                "End   - Handled with try finally, Returned 4 : Int32"
            |]
            
    [<TestMethod>]
    member this.``Evaluate performance inspector - while loop``() = 
        assertInspectors
            <@ while false do ignore() @>
            (fun list -> [| PerformanceInspector.createNew <| mockPerformanceInspectorConfig list|])
            [| 
                "Start - Running while loop"
                "Start - Getting value false : Boolean"
                "End   - Got value false : Boolean"
                "End   - Ran while loop"
            |]
            
    [<TestMethod>]
    member this.``Evaluate performance inspector - recursive let``() = 
        assertInspectors
            <@ let rec x = 4 in x @>
            (fun list -> [| PerformanceInspector.createNew <| mockPerformanceInspectorConfig list|])
            [| 
                "Start - Recursive let x : Int32"
                "Start - Getting value 4 : Int32"
                "End   - Got value 4 : Int32"
                "Start - Getting variable x : Int32"
                "End   - Got variable x, Returned 4 : Int32"
                "End   - Recursive let x : Int32 returned 4 : Int32"
            |]