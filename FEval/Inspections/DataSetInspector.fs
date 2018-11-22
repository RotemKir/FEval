namespace FEval.Inspections

[<RequireQualifiedAccess>]
module DataSetInspector =
    open FEval.EvaluationTypes
    open FEval.EvaluationEvents
    open FEval.Inspections.CommonInspections
    open FEval.Inspections.TypeFormatters

    type InspectionResult =
        {
            Name : string
            Value : string
        }

        (*
        Handle the following events:
        SetVariableEvent for:
            Let
            LetRecursive
            VarSet
            Lambda
        SetPropertyEvent
        SetFieldEvent
        *)

    // Private functions

    let private inspectSetVariable eventDetails =
        { 
            Name = eventDetails.Variable.Name
            Value = formatValue eventDetails.Value eventDetails.Variable.Type
        }
  
    let private formatSetPropertyName eventDetails =
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

    let private handleInspectionMessage logAction message =
        match message with
        | IsPreInspection _ & IsSetVariableEvent eventDetails
            -> logAction <| inspectSetVariable eventDetails
        | IsPreInspection _ & IsSetPropertyEvent eventDetails
            -> logAction <| inspectSetProperty eventDetails
        | _ -> ignore()
    

    // Public functions
        
    let createNew logAction = createInspector <| handleInspectionMessage logAction