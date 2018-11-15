namespace FEval.Inspections

[<RequireQualifiedAccess>]
module MethodCallInspector =
    open FEval.EvaluationTypes
    open FEval.Inspections.CommonInspections
    open FEval.Inspections.TypeFormatters
    open System
    open System.Reflection

    type InspectionResult = 
        {
            Time : DateTime 
            Method : string 
            Message : string
        }

    type Config =
        {
            HandleInspectionResult : InspectionResult -> unit
        }

    let private formatParameters parameters =
        "()"

    let private formatResult (methodInfo : MethodInfo) result =
        formatValue <| Option.get result <| methodInfo.ReturnType

    let private formatMethodCall methodEventDetails  =
        sprintf "%s -> %s" 
            <| formatParameters methodEventDetails.Parameters
            <| formatResult methodEventDetails.Method methodEventDetails.Result

    let private createInspectionResult startTime (methodEventDetails : MethodEventDetails) =
        {
            Time = startTime
            Method = formatMethodName methodEventDetails.Method <| methodEventDetails.Instance.GetType() 
            Message = formatMethodCall methodEventDetails
        }

    let private postInspector config startTime inspectionEvent _ =
        inspectMethodEvent 
            <| inspectionEvent 
            <| createInspectionResult startTime
        |> Option.get
        |> config.HandleInspectionResult

    let private preInspector config inspectionEvent =
        match inspectionEvent with
        | MethodEvent _ -> Some <| postInspector config DateTime.Now
        | _             -> None

    // Public Functions

    let createNew config inspectionEvent _ =
        preInspector config inspectionEvent
