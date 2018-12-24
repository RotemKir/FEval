namespace FEval.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open FEval.Tests.TestHelpers
open FEval.Inspectors
open System.Collections.Generic
open FEval.EvaluationTypes

[<TestClass>]
type MethodCallInspectorTests() =
    
    let addMessageToList (list : List<string>) (logEvent : LogEvent<MethodCallInspector.InspectionResult>) =
        list.Add(sprintf "%s - %s" logEvent.InspectionResult.Method logEvent.InspectionResult.Message)

    [<TestMethod>]
    member __.``Evaluate method call inspector - instance method, no parameters``() = 
        assertInspectors
            <@ let number = 2 in number.ToString()  @>
            (fun list -> [| MethodCallInspector.createNew <| addMessageToList list |])
            [| 
                "Int32.ToString - () -> \"2\" : String" 
            |]

    [<TestMethod>]
    member __.``Evaluate method call inspector - instance method, one parameter``() = 
        assertInspectors
            <@ let number = 2 in number.ToString("F")  @>
            (fun list -> [| MethodCallInspector.createNew <| addMessageToList list |])
            [| 
                "Int32.ToString - (\"F\" : String) -> \"2.00\" : String" 
            |]
            
    [<TestMethod>]
    member __.``Evaluate method call inspector - instance method, several parameters``() = 
        assertInspectors
            <@ let text = "text" in text.PadLeft(7, 'A')  @>
            (fun list -> [| MethodCallInspector.createNew <| addMessageToList list |])
            [| 
                "String.PadLeft - (7 : Int32, 'A' : Char) -> \"AAAtext\" : String" 
            |]
            
    [<TestMethod>]
    member __.``Evaluate method call inspector - static method, one parameter``() = 
        assertInspectors
            <@ abs -3  @>
            (fun list -> [| MethodCallInspector.createNew <| addMessageToList list |])
            [| 
                "Operators.Abs - (-3 : Int32) -> 3 : Int32" 
            |]
              
    [<TestMethod>]
    member __.``Evaluate method call inspector - static method, several parameters``() = 
        assertInspectors
            <@ max 5 18 @>
            (fun list -> [| MethodCallInspector.createNew <| addMessageToList list |])
            [| 
                "Operators.Max - (5 : Int32, 18 : Int32) -> 18 : Int32" 
            |]