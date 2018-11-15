namespace FEval.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open FEval.Tests.TestHelpers
open FEval.Inspections
open System.Collections.Generic

[<TestClass>]
type MethodCallInspectorTests() =
    
    let addMessageToList (list : List<string>) (inspectionResult : MethodCallInspector.InspectionResult) =
        list.Add(sprintf "%s - %s" inspectionResult.Method inspectionResult.Message)

    let mockConfig messageList : MethodCallInspector.Config =
        {
            HandleInspectionResult = addMessageToList messageList
        }
          
    [<TestMethod>]
    member this.``Evaluate method call inspector - instance method, no parameters``() = 
        assertInspectors
            <@ let number = 2 in number.ToString()  @>
            (fun list -> [| MethodCallInspector.createNew <| mockConfig list |])
            [| 
                "Int32.ToString - () -> \"2\" : String" 
            |]

    [<TestMethod>]
    member this.``Evaluate method call inspector - instance method, one parameter``() = 
        assertInspectors
            <@ let number = 2 in number.ToString("F")  @>
            (fun list -> [| MethodCallInspector.createNew <| mockConfig list |])
            [| 
                "Int32.ToString - (\"F\" : String) -> \"2.00\" : String" 
            |]
            
    [<TestMethod>]
    member this.``Evaluate method call inspector - instance method, several parameters``() = 
        assertInspectors
            <@ let text = "text" in text.PadLeft(7, 'A')  @>
            (fun list -> [| MethodCallInspector.createNew <| mockConfig list |])
            [| 
                "String.PadLeft - (7 : Int32, 'A' : Char) -> \"AAAtext\" : String" 
            |]
            
    [<TestMethod>]
    member this.``Evaluate method call inspector - static method, one parameter``() = 
        assertInspectors
            <@ abs -3  @>
            (fun list -> [| MethodCallInspector.createNew <| mockConfig list |])
            [| 
                "Operators.Abs - (-3 : Int32) -> 3 : Int32" 
            |]
              
    [<TestMethod>]
    member this.``Evaluate method call inspector - static method, several parameters``() = 
        assertInspectors
            <@ max 5 18 @>
            (fun list -> [| MethodCallInspector.createNew <| mockConfig list |])
            [| 
                "Operators.Max - (5 : Int32, 18 : Int32) -> 18 : Int32" 
            |]