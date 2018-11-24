namespace FEval.Inspections

[<RequireQualifiedAccess>]
module MethodCallInspector =
    open FEval.EvaluationTypes
    open FEval.EvaluationEvents
    open FEval.Inspections.CommonInspections
    open FEval.Inspections.TypeFormatters
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
    
    // Public Functions

    let createNew = createInspector handleInspectionMessage

    let stringInspectionResultFormatter inspectionResult =
        sprintf "%s - %s" inspectionResult.Method inspectionResult.Message
            
    let csvInspectionResultFormatter inspectionResult =
        sprintf "%s,\"%s\"" inspectionResult.Method <| formatCsvLine inspectionResult.Message
        
    let defaultLogConfig =
        {
            Formatter = createStringFormatter stringInspectionResultFormatter
            Header = None
        }

    let csvLogConfig =
        {
            Formatter = createCsvFormatter csvInspectionResultFormatter
            Header = Some <| createCsvFileHeader "Method,Message"
        }