namespace FEval.Tests

open FEval.EvaluationTypes
open FEVal.Inspections
open FEval.Inspections.ValidationsCommon
open FEval.Tests.TestHelpers
open Microsoft.VisualStudio.TestTools.UnitTesting
open System.Collections.Generic

[<TestClass>]
type ValidationInspectorTests() =

    let addMessagesToList (list : List<string>) messages format =
        Seq.iter (fun s -> list.Add(sprintf format s)) messages

    let addMessageToList (list : List<string>) (logEvent : LogEvent<ValidationInspector.InspectionResult>) =
        addMessagesToList list logEvent.InspectionResult.Warnings "Warning - %s"
        addMessagesToList list logEvent.InspectionResult.Errors "Error - %s"

    let createRule result =
        CustomRule (fun _ -> result)

    let createInspector rules list =
        ValidationInspector.createNew 
            <| rules 
            <| addMessageToList list

    [<TestMethod>]
    member __.``Evaluate validation inspector - no rules - returns no message``() = 
        assertInspectors
            <@ 4 @>
            (fun list -> [| createInspector [||] list |])
            [||]

    [<TestMethod>]
    member __.``Evaluate validation inspector - rule result is ok - returns no message``() = 
        assertInspectors
            <@ 4 @>
            (fun list -> 
                [| createInspector [| createRule ValidationResult.Ok |] list |])
            [||]

    [<TestMethod>]
    member __.``Evaluate validation inspector - rule result is warning - returns warning message``() = 
        assertInspectors
            <@ 4 @>
            (fun list -> 
                [| createInspector [| createRule <| ValidationResult.Warning "message" |] list |])
            [|
                "Warning - message"
            |]
    
    [<TestMethod>]
    member __.``Evaluate validation inspector - all rule results are warnings - returns warning messages``() = 
        assertInspectors
            <@ 4 @>
            (fun list -> 
                [| createInspector 
                    [| 
                        createRule <| ValidationResult.Warning "message 1" 
                        createRule <| ValidationResult.Warning "message 2" 
                        createRule <| ValidationResult.Warning "message 3" 
                    |] 
                    list 
                |])
            [|
                "Warning - message 1"
                "Warning - message 2"
                "Warning - message 3"
            |]
    
    [<TestMethod>]
    member __.``Evaluate validation inspector - rule result is error - returns error message``() = 
        assertInspectors
            <@ 4 @>
            (fun list -> 
                [| createInspector [| createRule <| ValidationResult.Error "message" |] list |])
            [|
                "Error - message"
            |]
    
    [<TestMethod>]
    member __.``Evaluate validation inspector - all rule results are errors - returns error messages``() = 
        assertInspectors
            <@ 4 @>
            (fun list -> 
                [| createInspector 
                    [| 
                        createRule <| ValidationResult.Error "message 1" 
                        createRule <| ValidationResult.Error "message 2" 
                        createRule <| ValidationResult.Error "message 3" 
                    |] 
                    list 
                |])
            [|
                "Error - message 1"
                "Error - message 2"
                "Error - message 3"
            |]
    
    [<TestMethod>]
    member __.``Evaluate validation inspector - result are ok, warning and error - returns warning and error messages``() = 
        assertInspectors
            <@ 4 @>
            (fun list -> 
                [| createInspector 
                    [| 
                        createRule <| ValidationResult.Error "message 1" 
                        createRule <| ValidationResult.Ok 
                        createRule <| ValidationResult.Warning "message 2" 
                    |] 
                    list 
                |])
            [|
                "Warning - message 2"
                "Error - message 1"
            |]