namespace FEval.Inspections

[<RequireQualifiedAccess>]
module DataSetInspector =
    open FEval.EvaluationTypes
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
        None

    // Public functions
        
    let createNew logAction inspectionContext =
        match inspectionContext.InspectionEvent with
        | SetVariableEvent eventDetails -> inspectSetVariable logAction eventDetails
        | _                             -> None