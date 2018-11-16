namespace FEval.Inspections

[<RequireQualifiedAccess>]
module MethodCallInspector =
    open FEval.EvaluationTypes
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

    let private formatParamter parameterValue (parameterInfo : ParameterInfo) =
        formatValue parameterValue parameterInfo.ParameterType

    let private formatParameters (methodInfo : MethodInfo) parameters =
        Seq.map2 formatParamter <| parameters <| methodInfo.GetParameters()
        |> String.concat ", "
        |> sprintf "(%s)"

    let private formatResult (methodInfo : MethodInfo) result =
        formatValue <| Option.get result <| methodInfo.ReturnType

    let private formatMethodCall (methodEventDetails : MethodEventDetails) =
        sprintf "%s -> %s" 
            <| formatParameters methodEventDetails.Method methodEventDetails.Parameters
            <| formatResult methodEventDetails.Method methodEventDetails.Result
            
    let private createInspectionResult startTime (methodEventDetails : MethodEventDetails) =
        let method = methodEventDetails.Method 
        {
            Time = startTime
            Method = formatMethodName method 
                     <| getInstanceType methodEventDetails.Instance method.DeclaringType
            Message = formatMethodCall methodEventDetails
        }

    let private postInspector logAction startTime inspectionContext =
        inspectMethodEvent 
            <| inspectionContext.InspectionEvent 
            <| createInspectionResult startTime
        |> Option.get
        |> logAction

    // Public Functions

    let createNew logAction inspectionContext =
        inspectMethodEvent
            <| inspectionContext.InspectionEvent
            <| (fun _ -> postInspector logAction inspectionContext.Time)

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