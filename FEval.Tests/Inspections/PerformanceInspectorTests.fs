namespace FEval.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open FEval.EvaluationTypes
open FEval.Inspectors
open FEval.Tests.TestHelpers
open System.Collections.Generic

[<TestClass>]
type PerformanceInspectorTests() =
    
    let addMessageToList (list : List<string>) (logEvent : LogEvent<PerformanceInspector.InspectionResult>) =
        match logEvent.InspectionResult with
        | PerformanceInspector.PreResult message     -> list.Add(message)
        | PerformanceInspector.PostResult (message, _) -> list.Add(message)

    [<TestMethod>]
    member __.``Evaluate performance inspector - value``() = 
        assertInspectors
            <@ 4 @>
            (fun list -> [| PerformanceInspector.createNew <| addMessageToList list |])
            [| 
                "Getting value 4 : Int32" 
                "Got value 4 : Int32" 
            |]

    [<TestMethod>]
    member __.``Evaluate performance inspector - call static method``() = 
        assertInspectors
            <@ abs -3 @>
            (fun list -> [| PerformanceInspector.createNew <| addMessageToList list |])
            [| 
                "Calling Operators.Abs(Int32)"
                "Getting value -3 : Int32"
                "Got value -3 : Int32"
                "Called Operators.Abs(Int32), Returned 3 : Int32" 
            |]

    [<TestMethod>]
    member __.``Evaluate performance inspector - none union case``() = 
        assertInspectors
            <@ None @>
            (fun list -> [| PerformanceInspector.createNew <| addMessageToList list |])
            [| 
                "Creating None : Option<Object>"
                "Created None : Option<Object>" 
            |]
            
    [<TestMethod>]
    member __.``Evaluate performance inspector - some union case``() = 
        assertInspectors
            <@ Some 16 @>
            (fun list -> [| PerformanceInspector.createNew <| addMessageToList list |])
            [| 
                "Creating Some : Option<Int32>"
                "Getting value 16 : Int32" 
                "Got value 16 : Int32" 
                "Created Some 16 : Option<Int32>" 
            |]
            
    [<TestMethod>]
    member __.``Evaluate performance inspector - new record``() = 
        assertInspectors
            <@ { FirstName = "First" ; LastName = "Last" } @>
            (fun list -> [| PerformanceInspector.createNew <| addMessageToList list |])
            [| 
                "Creating new Person"
                "Getting value \"First\" : String" 
                "Got value \"First\" : String" 
                "Getting value \"Last\" : String" 
                "Got value \"Last\" : String" 
                "Created {FirstName = \"First\";\n LastName = \"Last\";} : Person"
            |]
    
    [<TestMethod>]
    member __.``Evaluate performance inspector - new tuple``() = 
        assertInspectors
            <@ (16, "Text", true) @>
            (fun list -> [| PerformanceInspector.createNew <| addMessageToList list |])
            [| 
                "Creating new tuple (Int32, String, Boolean)"
                "Getting value 16 : Int32" 
                "Got value 16 : Int32" 
                "Getting value \"Text\" : String" 
                "Got value \"Text\" : String" 
                "Getting value true : Boolean" 
                "Got value true : Boolean" 
                "Created tuple (16, \"Text\", true) : (Int32, String, Boolean)"
            |]
    
    [<TestMethod>]
    member __.``Evaluate performance inspector - new array``() = 
        assertInspectors
            <@ [|1 ; 2 ; 3|] @>
            (fun list -> [| PerformanceInspector.createNew <| addMessageToList list |])
            [| 
                "Creating new array Int32"
                "Getting value 1 : Int32" 
                "Got value 1 : Int32" 
                "Getting value 2 : Int32" 
                "Got value 2 : Int32" 
                "Getting value 3 : Int32" 
                "Got value 3 : Int32" 
                "Created array [|1; 2; 3|] : Int32"
            |]

    [<TestMethod>]
    member __.``Evaluate performance inspector - let statement``() = 
        assertInspectors
            <@ let x = 18 in x @>
            (fun list -> [| PerformanceInspector.createNew <| addMessageToList list |])
            [| 
                "Let x : Int32"
                "Getting value 18 : Int32" 
                "Got value 18 : Int32" 
                "Getting variable x : Int32" 
                "Got variable x, Returned 18 : Int32" 
                "Let x returned 18"
            |]
            
    [<TestMethod>]
    member __.``Evaluate performance inspector - function application``() = 
        assertInspectors
            <@ let f x = x + 1 in f 3 @>
            (fun list -> [| PerformanceInspector.createNew <| addMessageToList list |])
            [| 
                "Let f : (Int32 -> Int32)"
                "Creating lambda (Int32 -> Int32)"
                "Created lambda (Int32 -> Int32)"
                "Applying function (Int32 -> Int32)"
                "Getting variable f : (Int32 -> Int32)"
                "Got variable f, Returned (Int32 -> Int32)"
                "Getting value 3 : Int32"
                "Got value 3 : Int32"
                "Calling Operators.op_Addition(Int32, Int32)"
                "Getting variable x : Int32"
                "Got variable x, Returned 3 : Int32"
                "Getting value 1 : Int32"
                "Got value 1 : Int32"
                "Called Operators.op_Addition(Int32, Int32), Returned 4 : Int32"
                "Applied function (Int32 -> Int32), Returned 4 : Int32"
                "Let f returned 4 : Int32"
            |]
            
    [<TestMethod>]
    member __.``Evaluate performance inspector - call instance method``() = 
        assertInspectors
            <@ let x = 3 in x.ToString() @>
            (fun list -> [| PerformanceInspector.createNew <| addMessageToList list |])
            [| 
                "Let x : Int32"
                "Getting value 3 : Int32"
                "Got value 3 : Int32"
                "Calling Int32.ToString()"
                "Getting variable x : Int32"
                "Got variable x, Returned 3 : Int32"
                "Called Int32.ToString(), Returned \"3\" : String" 
                "Let x returned \"3\" : String"
            |]

    [<TestMethod>]
    member __.``Evaluate performance inspector - coerce to object``() = 
        assertInspectors
            <@ let x = "Hello" in x :> obj @>
            (fun list -> [| PerformanceInspector.createNew <| addMessageToList list |])
            [| 
                "Let x : String"
                "Getting value \"Hello\" : String"
                "Got value \"Hello\" : String"
                "Coercing String to Object"
                "Getting variable x : String"
                "Got variable x, Returned \"Hello\" : String"
                "Coerced String to Object"
                "Let x returned \"Hello\" : String (Object)"
            |]
        
    [<TestMethod>]
    member __.``Evaluate performance inspector - create new object``() = 
        assertInspectors
            <@ new ChildClass("Hello") @>
            (fun list -> [| PerformanceInspector.createNew <| addMessageToList list |])
            [| 
                "Creating new object ChildClass(String)"
                "Getting value \"Hello\" : String"
                "Got value \"Hello\" : String"
                "Created new object ChildClass"
            |]
            
    [<TestMethod>]
    member __.``Evaluate performance inspector - get property``() = 
        assertInspectors
            <@ let x = new ChildClass("Hello") in x.NameProperty @>
            (fun list -> [| PerformanceInspector.createNew <| addMessageToList list |])
            [| 
                "Let x : ChildClass"
                "Creating new object ChildClass(String)"
                "Getting value \"Hello\" : String"
                "Got value \"Hello\" : String"
                "Created new object ChildClass"
                "Getting property ChildClass.NameProperty"
                "Getting variable x : ChildClass"
                "Got variable x, Returned ChildClass"
                "Got property ChildClass.NameProperty, Returned \"Hello\" : String"
                "Let x returned \"Hello\" : String"
            |]
            
    [<TestMethod>]
    member __.``Evaluate performance inspector - set property``() = 
        assertInspectors
            <@ 
            let child = new ChildClass("Hello")
            child.NameProperty <- "World"
            child.NameProperty 
            @>
            (fun list -> [| PerformanceInspector.createNew <| addMessageToList list |])
            [| 
                "Let child : ChildClass"
                "Creating new object ChildClass(String)"
                "Getting value \"Hello\" : String"
                "Got value \"Hello\" : String"
                "Created new object ChildClass"
                "Performing PropertySet and then PropertyGet"
                "Setting property ChildClass.NameProperty"
                "Getting variable child : ChildClass"
                "Got variable child, Returned ChildClass"
                "Getting value \"World\" : String"
                "Got value \"World\" : String"
                "Set property ChildClass.NameProperty"
                "Getting property ChildClass.NameProperty"
                "Getting variable child : ChildClass"
                "Got variable child, Returned ChildClass"
                "Got property ChildClass.NameProperty, Returned \"World\" : String"
                "Performed PropertySet and then PropertyGet"
                "Let child returned \"World\" : String"
            |]
            
    [<TestMethod>]
    member __.``Evaluate performance inspector - get indexer property``() = 
        assertInspectors
            <@ 
            let indexerClass = new IndexerClass()
            indexerClass.[2]
            @>
            (fun list -> [| PerformanceInspector.createNew <| addMessageToList list |])
            [| 
                "Let indexerClass : IndexerClass"
                "Creating new object IndexerClass()"
                "Created new object IndexerClass"
                "Getting property IndexerClass.Item"
                "Getting variable indexerClass : IndexerClass"
                "Got variable indexerClass, Returned IndexerClass"
                "Getting value 2 : Int32"
                "Got value 2 : Int32"
                "Got property IndexerClass.Item[Int32]"
                "Let indexerClass returned \"three\" : String"
            |]
            
    [<TestMethod>]
    member __.``Evaluate performance inspector - set indexer property``() = 
        assertInspectors
            <@ 
            let indexerClass = new IndexerClass()
            indexerClass.[2] <- "Lovely Two"
            @>
            (fun list -> [| PerformanceInspector.createNew <| addMessageToList list |])
            [| 
                "Let indexerClass : IndexerClass"
                "Creating new object IndexerClass()"
                "Created new object IndexerClass"
                "Setting property IndexerClass.Item[Int32]"
                "Getting variable indexerClass : IndexerClass"
                "Got variable indexerClass, Returned IndexerClass"
                "Getting value \"Lovely Two\" : String"
                "Got value \"Lovely Two\" : String"
                "Getting value 2 : Int32"
                "Got value 2 : Int32"
                "Set property IndexerClass.Item[Int32]"
                "Let indexerClass returned Unit"
            |]
            
    [<TestMethod>]
    member __.``Evaluate performance inspector - struct default value``() = 
        assertInspectors
            <@  let struct1 = new Struct() in struct1 @>
            (fun list -> [| PerformanceInspector.createNew <| addMessageToList list |])
            [| 
                "Let struct1 : Struct"
                "Creating default value for Struct"
                "Created default value for Struct"
                "Getting variable struct1 : Struct"
                "Got variable struct1, Returned Struct"
                "Let struct1 returned Struct"
            |]       
            
    [<TestMethod>]
    member __.``Evaluate performance inspector - get field``() = 
        assertInspectors
            <@  let field = new FieldClass(54) in field.number @>
            (fun list -> [| PerformanceInspector.createNew <| addMessageToList list |])
            [| 
                "Let field : FieldClass"
                "Creating new object FieldClass(Int32)"
                "Getting value 54 : Int32"
                "Got value 54 : Int32"
                "Created new object FieldClass"
                "Getting field FieldClass.number"
                "Getting variable field : FieldClass"
                "Got variable field, Returned FieldClass"
                "Got field FieldClass.number, Returned 54"
                "Let field returned 54 : Int32"
            |]
            
    [<TestMethod>]
    member __.``Evaluate performance inspector - set field``() = 
        assertInspectors
            <@  let field = new FieldClass(54) in field.number <- 73 @>
            (fun list -> [| PerformanceInspector.createNew <| addMessageToList list |])
            [| 
                "Let field : FieldClass"
                "Creating new object FieldClass(Int32)"
                "Getting value 54 : Int32"
                "Got value 54 : Int32"
                "Created new object FieldClass"
                "Setting field FieldClass.number"
                "Getting variable field : FieldClass"
                "Got variable field, Returned FieldClass"
                "Getting value 73 : Int32"
                "Got value 73 : Int32"
                "Set field FieldClass.number"
                "Let field returned Unit"
            |]
            
    [<TestMethod>]
    member __.``Evaluate performance inspector - var set``() = 
        assertInspectors
            <@  let mutable x = 3 in x <- 18 @>
            (fun list -> [| PerformanceInspector.createNew <| addMessageToList list |])
            [| 
                "Let x : Int32"
                "Getting value 3 : Int32"
                "Got value 3 : Int32"
                "Setting variable x : Int32"
                "Getting value 18 : Int32"
                "Got value 18 : Int32"
                "Set variable x : Int32"
                "Let x returned Unit"
            |]
            
    [<TestMethod>]
    member __.``Evaluate performance inspector - for loop``() = 
        assertInspectors
            <@  let mutable x = 0 in for i = 1 to 3 do x <- x + i @>
            (fun list -> [| PerformanceInspector.createNew <| addMessageToList list |])
            [| 
                "Let x : Int32"
                "Getting value 0 : Int32"
                "Got value 0 : Int32"
                "Running for loop on i : Int32"
                "Getting value 1 : Int32"
                "Got value 1 : Int32"
                "Getting value 3 : Int32"
                "Got value 3 : Int32"
                // Loop #1
                "Setting variable x : Int32"
                "Calling Operators.op_Addition(Int32, Int32)"
                "Getting variable x : Int32"
                "Got variable x, Returned 0 : Int32"
                "Getting variable i : Int32"
                "Got variable i, Returned 1 : Int32"
                "Called Operators.op_Addition(Int32, Int32), Returned 1 : Int32"
                "Set variable x : Int32"                
                // Loop #2               
                "Setting variable x : Int32"
                "Calling Operators.op_Addition(Int32, Int32)"
                "Getting variable x : Int32"
                "Got variable x, Returned 1 : Int32"
                "Getting variable i : Int32"
                "Got variable i, Returned 2 : Int32"
                "Called Operators.op_Addition(Int32, Int32), Returned 3 : Int32"
                "Set variable x : Int32"
                // Loop #3
                "Setting variable x : Int32"
                "Calling Operators.op_Addition(Int32, Int32)"
                "Getting variable x : Int32"
                "Got variable x, Returned 3 : Int32"
                "Getting variable i : Int32"
                "Got variable i, Returned 3 : Int32"
                "Called Operators.op_Addition(Int32, Int32), Returned 6 : Int32"
                "Set variable x : Int32"
                "Ran for loop on i : Int32"
                "Let x returned Unit"
            |]

    [<TestMethod>]
    member __.``Evaluate performance inspector - if then else``() = 
        assertInspectors
            <@ if true then 4 else 5 @>
            (fun list -> [| PerformanceInspector.createNew <| addMessageToList list |])
            [| 
                "Evaluating if"
                "Getting value true : Boolean"
                "Got value true : Boolean"
                "Getting value 4 : Int32"
                "Got value 4 : Int32"
                "Evaluated if, Returned 4 : Int32"
            |]
            
    [<TestMethod>]
    member __.``Evaluate performance inspector - tuple get``() = 
        assertInspectors
            <@ let (a, _) = (1, 2) in a @>
            (fun list -> [| PerformanceInspector.createNew <| addMessageToList list |])
            [| 
                "Let patternInput : (Int32, Int32)"
                "Creating new tuple (Int32, Int32)"
                "Getting value 1 : Int32"
                "Got value 1 : Int32"
                "Getting value 2 : Int32"
                "Got value 2 : Int32"
                "Created tuple (1, 2) : (Int32, Int32)"
                "Let a : Int32"
                "Getting value from tuple (Int32, Int32) at index 0"
                "Getting variable patternInput : (Int32, Int32)"
                "Got variable patternInput, Returned (1, 2) : (Int32, Int32)"
                "Got value from tuple (Int32, Int32) at index 0, Retuned 1 : Int32"
                "Getting variable a : Int32"
                "Got variable a, Returned 1"
                "Let a returned 1 : Int32"
                "Let patternInput returned 1 : Int32"
            |]
            
    [<TestMethod>]
    member __.``Evaluate performance inspector - union case test``() = 
        assertInspectors
            <@ match UnionB with | UnionA -> true | _ -> false @>
            (fun list -> [| PerformanceInspector.createNew <| addMessageToList list |])
            [| 
                "Let matchValue : Union"
                "Creating UnionB : Union"
                "Created UnionB : Union"
                "Evaluating if"
                "Checking if union matches UnionA : Union"
                "Getting variable matchValue : Union"
                "Got variable matchValue, Returned UnionB : Union"
                "Checked if union matches UnionA : Union, Returned false : Boolean"
                "Getting value false : Boolean"
                "Got value false : Boolean"
                "Evaluated if, Returned false : Boolean"
                "Let matchValue returned false : Boolean"
            |]
            
    [<TestMethod>]
    member __.``Evaluate performance inspector - type test``() = 
        assertInspectors
            <@ 
            let x : obj = 6 :> obj           
            match x with | :? int -> true | _ -> false
            @>
            (fun list -> [| PerformanceInspector.createNew <| addMessageToList list |])
            [| 
                "Let x : Object"
                "Coercing Int32 to Object"
                "Getting value 6 : Int32"
                "Got value 6 : Int32"
                "Coerced Int32 to Object"
                "Evaluating if"
                "Testing if Object is Int32"
                "Getting variable x : Object"
                "Got variable x, Returned 6 : Int32 (Object)"
                "Tested if Object is Int32, Returned true : Boolean"
                "Getting value true : Boolean"
                "Got value true : Boolean"
                "Evaluated if, Returned true : Boolean"
                "Let x returned true : Boolean"
            |]
            
    [<TestMethod>]
    member __.``Evaluate performance inspector - try with``() = 
        assertInspectors
            <@ try 4 with | _ -> 5 @>
            (fun list -> [| PerformanceInspector.createNew <| addMessageToList list |])
            [| 
                "Handling with try with"
                "Getting value 4 : Int32"
                "Got value 4 : Int32"
                "Handled with try with, Returned 4 : Int32"
            |]
            
    [<TestMethod>]
    member __.``Evaluate performance inspector - try finally``() = 
        assertInspectors
            <@ try 4 finally ignore() @>
            (fun list -> [| PerformanceInspector.createNew <| addMessageToList list |])
            [| 
                "Handling with try finally"
                "Getting value 4 : Int32"
                "Got value 4 : Int32"
                "Calling Operators.Ignore(Unit)"
                "Getting value Unit"
                "Got value Unit"
                "Called Operators.Ignore(Unit), Returned Void"
                "Handled with try finally, Returned 4 : Int32"
            |]
            
    [<TestMethod>]
    member __.``Evaluate performance inspector - while loop``() = 
        assertInspectors
            <@ while false do ignore() @>
            (fun list -> [| PerformanceInspector.createNew <| addMessageToList list |])
            [| 
                "Running while loop"
                "Getting value false : Boolean"
                "Got value false : Boolean"
                "Ran while loop"
            |]
            
    [<TestMethod>]
    member __.``Evaluate performance inspector - recursive let``() = 
        assertInspectors
            <@ let rec x = 4 in x @>
            (fun list -> [| PerformanceInspector.createNew <| addMessageToList list |])
            [| 
                "Recursive let x : Int32"
                "Getting value 4 : Int32"
                "Got value 4 : Int32"
                "Getting variable x : Int32"
                "Got variable x, Returned 4 : Int32"
                "Recursive let x : Int32 returned 4 : Int32"
            |]
            
    [<TestMethod>]
    member __.``Evaluate performance inspector - typed code quotation``() = 
        assertInspectors
            <@ <@ 4 @> @>
            (fun list -> [| PerformanceInspector.createNew <| addMessageToList list |])
            [| 
                "Getting quote (Value) : Int32"
                "Got quote (Value) : Int32"
            |]
              
    [<TestMethod>]
    member __.``Evaluate performance inspector - raw code quotation``() = 
        assertInspectors
            <@ <@@ 7 @@> @>
            (fun list -> [| PerformanceInspector.createNew <| addMessageToList list |])
            [| 
                "Getting quote (Value) : Int32"
                "Got quote (Value) : Int32"
            |]