namespace FEval.Inspections

[<RequireQualifiedAccess>]
module MethodCallInspector =
    open FEval.EvaluationTypes
    open FEval.Inspections.CommonInspections
    open FEval.Inspections.TypeFormatters
    open System
    open System.Reflection
    open FEval.Inspections.Persistance

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

    let stringInspectionResultFormatter inspectionResult =
        sprintf "%s - %s - %s" 
            <| formatTimeForLog inspectionResult.Time 
            <| inspectionResult.Method 
            <| inspectionResult.Message
            
    let csvInspectionResultFormatter inspectionResult =
        sprintf "%s,%s,\"%s\""
            <| formatTimeForLog inspectionResult.Time 
            <| inspectionResult.Method 
            <| formatCsvLine inspectionResult.Message
        
    let createFileLogConfig formatter fileName =
        {
            HandleInspectionResult = appendLineToFile fileName formatter
        }

    let createDefaultFileLogConfig = 
        createFileLogConfig stringInspectionResultFormatter
        
    let createCsvLogConfig fileName =
        setFileHeader fileName "Time,Method,Message"
        createFileLogConfig csvInspectionResultFormatter fileName