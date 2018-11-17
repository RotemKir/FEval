namespace FEval.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open FEval.Inspections
open FEval.Tests.TestHelpers
open System.Collections.Generic

[<TestClass>]
type DataSetInspectorTests() =
    
    let addMessageToList (list : List<string>) (inspectionResult : DataSetInspector.InspectionResult) =
        list.Add(sprintf "%s - %s" inspectionResult.Name inspectionResult.Value)

    [<TestMethod>]
    member this.``Evaluate data set inspector - let expression``() = 
        assertInspectors
            <@ let number = 2 in number @>
            (fun list -> [| DataSetInspector.createNew <| addMessageToList list |])
            [| 
                "number - 2 : Int32" 
            |]
