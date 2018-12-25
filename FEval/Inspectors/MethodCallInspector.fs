namespace FEval.Inspectors

[<RequireQualifiedAccess>]
module internal MethodCallInspector =
    open FEval.EvaluationTypes
    open FEval.EvaluationEvents
    open FEval.Inspectors.InspectionsCommon
    open FEval.Inspectors.TypeFormatters
    open FEval.Logging
    open System.Reflection

    type InspectionResult = 
        {
            Method : string 
            Message : string
        }

    let private formatResult (methodInfo : MethodInfo) result =
        formatValue <| Option.get result <| methodInfo.ReturnType

    let private formatMethodCall (methodEventDetails : MethodEventDetails) =
        sprintf "(%s) -> %s" 
            <| formatParameterValues (methodEventDetails.Method.GetParameters()) methodEventDetails.Parameters
            <| formatResult methodEventDetails.Method methodEventDetails.Result
            
    let private createInspectionResult (methodEventDetails : MethodEventDetails) =
        let method = methodEventDetails.Method 
        {
            Method = formatMethodName method 
                     <| getInstanceType methodEventDetails.Instance method.DeclaringType
            Message = formatMethodCall methodEventDetails
        }

    let private handleInspectionMessage message =
        match message with
        | IsPostInspection _ & IsMethodEvent eventDetails
            -> Some <| createInspectionResult eventDetails
        | _ -> None
    
    let private csvFileHeader = "Method,Message"

    let private txtInspectionResultFormatter inspectionResult =
        sprintf "%s - %s" inspectionResult.Method inspectionResult.Message
            
    let private csvInspectionResultFormatter inspectionResult =
        sprintf "%s,\"%s\"" inspectionResult.Method <| formatCsvLine inspectionResult.Message
        
    let private txtLogConfig =
        {
            Formatter = SingleLine <| createStringFormatter txtInspectionResultFormatter
            Header = None
        }

    let private csvLogConfig =
        {
            Formatter = SingleLine <| createCsvFormatter csvInspectionResultFormatter
            Header = Some <| createCsvFileHeader csvFileHeader
        }

    // Public Functions

    let createNew = createInspector handleInspectionMessage
    let createTxtLogger = createLogger txtLogConfig
    let createCsvLogger = createLogger csvLogConfig
