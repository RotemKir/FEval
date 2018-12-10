namespace FEVal.Inspections

open FEval.EvaluationEvents
open FEval.Inspections
open FEval.Inspections.ValidationsCommon
open FEval.Logging

[<RequireQualifiedAccess>]
module ValidationInspector =

    type InspectionResult =
        {
            Warnings : string seq
            Errors : string seq
        }

    let private emptyInspectionResult =
        {
            Warnings = [||]
            Errors = [||]
        }

    let private handleRule validationContext inspectionResult rule  =
        match Validator.runRule validationContext rule with
        | Ok 
            -> inspectionResult
        | Warning message 
            -> { inspectionResult with Warnings = Seq.append inspectionResult.Warnings [| message |] }
        | Error message 
            -> { inspectionResult with Errors = Seq.append inspectionResult.Errors [| message |] }

    let private runValidationRules rules validationContext = 
        Seq.fold
            <| handleRule validationContext 
            <| emptyInspectionResult
            <| rules

    let private convertResultToOption inspectionResult =
        if inspectionResult = emptyInspectionResult
        then None
        else Some inspectionResult

    let private handlePostInspection rules inspectionContext =
        createValidationContext inspectionContext 
        |> runValidationRules rules
        |> convertResultToOption

    let private handleInspectionMessage rules message =
        match message with
        | IsPostInspection (_, inspectionContext) -> handlePostInspection rules inspectionContext
        | _                                       -> None
    
    let private formatLogLine prefix =
        sprintf "***%s*** - %s" prefix

    let private createValidationStringFormatter logEvent =
        let logPrefix = createStringLogPrefix logEvent
        Seq.append
            <| Seq.map (formatLogLine "Warning") logEvent.InspectionResult.Warnings
            <| Seq.map (formatLogLine "Error") logEvent.InspectionResult.Errors
        |> Seq.map logPrefix

    let private txtLogConfig =
        {
            Formatter = MultiLine <| createValidationStringFormatter
            Header = None
        }

    // Public functions

    let createNew rules = createInspector <| handleInspectionMessage rules 
    let createTxtLogger = createLogger txtLogConfig