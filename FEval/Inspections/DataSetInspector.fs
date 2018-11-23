namespace FEval.Inspections

[<RequireQualifiedAccess>]
module DataSetInspector =
    open FEval.EvaluationTypes
    open FEval.EvaluationEvents
    open FEval.Inspections.CommonInspections
    open FEval.Inspections.Logging
    open FEval.Inspections.TypeFormatters
    open System

    type InspectionResult =
        {
            Time : DateTime 
            Name : string
            Value : string
        }

    // Private functions

    let private inspectSetVariable eventDetails time =
        { 
            Time = time
            Name = eventDetails.Variable.Name
            Value = formatValue eventDetails.Value eventDetails.Variable.Type
        }
  
    let private formatSetPropertyName (eventDetails : SetPropertyEventDetails) =
        let declaringType = getInstanceType eventDetails.Instance eventDetails.Property.DeclaringType
        let propertyName = formatPropertyName eventDetails.Property declaringType
        let indexerParameters = formatIndexerParameterValues 
                                <| eventDetails.Property.GetIndexParameters()
                                <| eventDetails.IndexerParameters
        sprintf "%s%s" propertyName indexerParameters
               
    let private inspectSetProperty eventDetails time =
        { 
            Time = time
            Name = formatSetPropertyName eventDetails
            Value = formatValue eventDetails.Value eventDetails.Property.PropertyType
        }
    
    let private formatSetFieldName (eventDetails : SetFieldEventDetails) =
        getInstanceType eventDetails.Instance eventDetails.Field.DeclaringType
        |> formatFieldName eventDetails.Field

    let private inspectSetField eventDetails time =
        { 
            Time = time
            Name = formatSetFieldName eventDetails
            Value = formatValue eventDetails.Value eventDetails.Field.FieldType
        }

    let private handleInspectionMessage logAction message =
        match message with
        | IsPreInspection inspectionContext & IsSetVariableEvent eventDetails
            -> logAction <| inspectSetVariable eventDetails inspectionContext.Time
        | IsPreInspection inspectionContext & IsSetPropertyEvent eventDetails
            -> logAction <| inspectSetProperty eventDetails inspectionContext.Time
        | IsPreInspection inspectionContext & IsSetFieldEvent eventDetails
            -> logAction <| inspectSetField eventDetails inspectionContext.Time
        | _ -> ignore()
    
    // Public functions
        
    let createNew logAction = createInspector <| handleInspectionMessage logAction

    let stringInspectionResultFormatter inspectionResult =
        sprintf "%s - %s <- %s" 
            <| formatDateTimeForLog inspectionResult.Time 
            <| inspectionResult.Name 
            <| inspectionResult.Value
            
    let csvInspectionResultFormatter inspectionResult =
        sprintf "%s,%s,\"%s\""
            <| formatDateTimeForLog inspectionResult.Time 
            <| inspectionResult.Name 
            <| inspectionResult.Value

    let defaultLogConfig =
        {
            Formatter = stringInspectionResultFormatter
            Header = None
        }

    let csvLogConfig =
        {
            Formatter = csvInspectionResultFormatter
            Header = Some "Time,Name,Value"
        }