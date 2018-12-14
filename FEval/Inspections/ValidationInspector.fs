namespace FEval.Inspections

open FEval.EvaluationEvents
open FEval.Inspections.ValidationsCommon
open FEval.Logging
open FEval.EvaluationTypes

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

    let private checkedIfResultHasError inspectionResult inspectionContext =
        if not <| Seq.isEmpty inspectionResult.Errors
        then 
            String.concat ", " inspectionResult.Errors
            |> setInspectionError inspectionContext.ErrorAgent

    let private handlePostInspection rules inspectionContext =
        let inspectionResult = 
            createValidationContext inspectionContext 
            |> runValidationRules rules
        checkedIfResultHasError inspectionResult inspectionContext

        inspectionResult 
        |> convertResultToOption

    let private handleInspectionMessage rules message =
        match message with
        | IsPostInspection (_, inspectionContext) -> handlePostInspection rules inspectionContext
        | _                                       -> None
    
    let private formatLogLine prefix =
        sprintf "***%s*** - %s" prefix

    let private formatCsvLine prefix =
        sprintf "***%s***,\"%s\"" prefix

    let private createValidationStringFormatter logPrefixFormatter lineFormatter logEvent =
        let logPrefix = logPrefixFormatter logEvent
        Seq.append
            <| Seq.map (lineFormatter "Warning") logEvent.InspectionResult.Warnings
            <| Seq.map (lineFormatter "Error") logEvent.InspectionResult.Errors
        |> Seq.map logPrefix

    let private txtLogConfig =
        {
            Formatter = MultiLine <| createValidationStringFormatter createStringLogPrefix formatLogLine
            Header = None
        }
        
    let private csvLogConfig =
        {
            Formatter = MultiLine <| createValidationStringFormatter createCsvLogPrefix formatCsvLine
            Header = None
        }

    // Public functions

    let createNew rules = createInspector <| handleInspectionMessage rules 
    let createTxtLogger = createLogger txtLogConfig
    let createCsvLogger = createLogger csvLogConfig