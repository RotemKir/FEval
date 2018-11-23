namespace FEval.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open FEval.Inspections
open FEval.Tests.TestHelpers
open System.Collections.Generic
open FEval.EvaluationTypes

[<TestClass>]
type DataSetInspectorTests() =
    
    let addMessageToList (list : List<string>) (logEvent : LogEvent<DataSetInspector.InspectionResult>) =
        list.Add(sprintf "%s - %s" logEvent.InspectionResult.Name logEvent.InspectionResult.Value)

    [<TestMethod>]
    member this.``Evaluate data set inspector - let expression``() = 
        assertInspectors
            <@ let number = 2 in number @>
            (fun list -> [| DataSetInspector.createNew <| addMessageToList list |])
            [| 
                "number - 2 : Int32" 
            |]

    [<TestMethod>]
    member this.``Evaluate data set inspector - let recursive expression``() = 
        assertInspectors
            <@ let rec number = 2 in number @>
            (fun list -> [| DataSetInspector.createNew <| addMessageToList list |])
            [| 
                "number - 2 : Int32" 
            |]
            
    [<TestMethod>]
    member this.``Evaluate data set inspector - var set expression``() = 
        assertInspectors
            <@ let mutable number = 2 in number <- 6 @>
            (fun list -> [| DataSetInspector.createNew <| addMessageToList list |])
            [| 
                "number - 2 : Int32" 
                "number - 6 : Int32" 
            |]

    [<TestMethod>]
    member this.``Evaluate data set inspector - lambda expression``() = 
        assertInspectors
            <@ let f x = x + 1 in f 3 @>
            (fun list -> [| DataSetInspector.createNew <| addMessageToList list |])
            [| 
                "f - (Int32 -> Int32)" 
                "x - 3 : Int32" 
            |]
            
    [<TestMethod>]
    member this.``Evaluate data set inspector - for loop expression``() = 
        assertInspectors
            <@ for i = 1 to 10 do ignore() @>
            (fun list -> [| DataSetInspector.createNew <| addMessageToList list |])
            [| 
                "i - 1 : Int32" 
                "i - 2 : Int32" 
                "i - 3 : Int32" 
                "i - 4 : Int32" 
                "i - 5 : Int32" 
                "i - 6 : Int32" 
                "i - 7 : Int32" 
                "i - 8 : Int32" 
                "i - 9 : Int32" 
                "i - 10 : Int32" 
                "i - 11 : Int32" 
            |]
            
    [<TestMethod>]
    member this.``Evaluate data set inspector - set property expression``() = 
        assertInspectors
            <@ let child = new ChildClass("Hello") in child.NameProperty <- "World" @>
            (fun list -> [| DataSetInspector.createNew <| addMessageToList list |])
            [| 
                "child - ChildClass"
                "ChildClass.NameProperty - \"World\" : String" 
            |]
            
    [<TestMethod>]
    member this.``Evaluate data set inspector - set indexer property expression``() = 
        assertInspectors
            <@  let indexerClass = new IndexerClass() in indexerClass.[2] <- "Lovely Two" @>
            (fun list -> [| DataSetInspector.createNew <| addMessageToList list |])
            [| 
                "indexerClass - IndexerClass"
                "IndexerClass.Item[2 : Int32] - \"Lovely Two\" : String" 
            |]

    [<TestMethod>]
    member this.``Evaluate data set inspector - set field expression``() = 
        assertInspectors
            <@ let fieldClass = new FieldClass(4) in fieldClass.number <- 8 @>
            (fun list -> [| DataSetInspector.createNew <| addMessageToList list |])
            [| 
                "fieldClass - FieldClass"
                "FieldClass.number - 8 : Int32" 
            |]