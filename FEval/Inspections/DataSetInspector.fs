namespace FEval.Inspections

[<RequireQualifiedAccess>]
module DataSetInspector =
    open FEval.EvaluationTypes
    open FEval.EvaluationEvents
    open FEval.Inspections.CommonInspections
    open FEval.Logging
    open FEval.Inspections.TypeFormatters

    type InspectionResult =
        {
            Name : string
            Value : string
        }

    // Private functions

    let private inspectSetVariable eventDetails =
        { 
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
               
    let private inspectSetProperty eventDetails =
        { 
            Name = formatSetPropertyName eventDetails
            Value = formatValue eventDetails.Value eventDetails.Property.PropertyType
        }
    
    let private formatSetFieldName (eventDetails : SetFieldEventDetails) =
        getInstanceType eventDetails.Instance eventDetails.Field.DeclaringType
        |> formatFieldName eventDetails.Field

    let private inspectSetField eventDetails =
        { 
            Name = formatSetFieldName eventDetails
            Value = formatValue eventDetails.Value eventDetails.Field.FieldType
        }

    let private handleInspectionMessage message =
        match message with
        | IsPreInspection _ & IsSetVariableEvent eventDetails
            -> Some <| inspectSetVariable eventDetails
        | IsPreInspection _ & IsSetPropertyEvent eventDetails
            -> Some <| inspectSetProperty eventDetails
        | IsPreInspection _ & IsSetFieldEvent eventDetails
            -> Some <| inspectSetField eventDetails
        | _ -> None
    
    // Public functions
        
    let createNew = createInspector handleInspectionMessage 

    let stringInspectionResultFormatter inspectionResult =
        sprintf "%s <- %s" inspectionResult.Name inspectionResult.Value
            
    let csvInspectionResultFormatter inspectionResult =
        sprintf "%s,\"%s\"" inspectionResult.Name  <| formatCsvLine inspectionResult.Value

    let defaultLogConfig =
        {
            Formatter = createStringFormatter stringInspectionResultFormatter
            Header = None
        }

    let csvLogConfig =
        {
            Formatter = createCsvFormatter csvInspectionResultFormatter
            Header = Some <| createCsvFileHeader "Name,Value"
        }