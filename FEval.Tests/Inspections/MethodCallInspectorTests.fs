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
            (fun list -> [| MethodCallInspector.createNew <| mockConfig list|])
            [| 
                "Int32.ToString - () -> \"2\" : String" 
            |]