namespace FEval.Inspections

[<RequireQualifiedAccess>]
module DataSetInspector =
    open FEval.EvaluationTypes
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

    let private inspectSetVariable logAction eventDetails =
        logAction 
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

    let private inspectSetProperty logAction eventDetails =
        logAction 
            { 
                Name = formatSetPropertyName eventDetails
                Value = formatValue eventDetails.Value eventDetails.Property.PropertyType
            }

    // Public functions
        
    let createNew logAction inspectionContext =
        match inspectionContext.InspectionEvent with
        | SetVariableEvent eventDetails -> inspectSetVariable logAction eventDetails
        | SetPropertyEvent eventDetails -> inspectSetProperty logAction eventDetails
        | _                             -> ignore()
        None