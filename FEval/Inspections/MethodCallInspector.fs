﻿namespace FEval.Inspections

[<RequireQualifiedAccess>]
module MethodCallInspector =
    open FEval.EvaluationTypes
    open FEval.InspectionEvents
    open FEval.Inspections.CommonInspections
    open FEval.Inspections.TypeFormatters
    open System
    open System.Reflection
    open FEval.Inspections.Logging

    type InspectionResult = 
        {
            Time : DateTime 
            Method : string 
            Message : string
        }

    let private formatResult (methodInfo : MethodInfo) result =
        formatValue <| Option.get result <| methodInfo.ReturnType

    let private formatMethodCall (methodEventDetails : MethodEventDetails) =
        sprintf "(%s) -> %s" 
            <| formatParameterValues (methodEventDetails.Method.GetParameters()) methodEventDetails.Parameters
            <| formatResult methodEventDetails.Method methodEventDetails.Result
            
    let private createInspectionResult startTime (methodEventDetails : MethodEventDetails) =
        let method = methodEventDetails.Method 
        {
            Time = startTime
            Method = formatMethodName method 
                     <| getInstanceType methodEventDetails.Instance method.DeclaringType
            Message = formatMethodCall methodEventDetails
        }

    let private postInspection (inspectionContext : InspectionContext) eventDetails =
        createInspectionResult inspectionContext.Time eventDetails

    let private handleInspectionMessage logAction message =
        match message with
        | IsPostInspection (preContext, _) & IsMethodEvent eventDetails
            -> logAction <| postInspection preContext eventDetails
        | _ -> ignore()
    
    // Public Functions

    let createNew logAction = createInspector <| handleInspectionMessage logAction

    let stringInspectionResultFormatter inspectionResult =
        sprintf "%s - %s - %s" 
            <| formatDateTimeForLog inspectionResult.Time 
            <| inspectionResult.Method 
            <| inspectionResult.Message
            
    let csvInspectionResultFormatter inspectionResult =
        sprintf "%s,%s,\"%s\""
            <| formatDateTimeForLog inspectionResult.Time 
            <| inspectionResult.Method 
            <| formatCsvLine inspectionResult.Message
        
    let defaultLogConfig =
        {
            Formatter = stringInspectionResultFormatter
            Header = None
        }

    let csvLogConfig =
        {
            Formatter = csvInspectionResultFormatter
            Header = Some "Time,Method,Message"
        }