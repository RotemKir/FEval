namespace FEval.Inspections

[<RequireQualifiedAccess>]
module MethodCallInspector =
    open FEval.Inspections.CommonInspections
    open System

    type InspectionResult = 
        {
            time : DateTime 
            method : string 
            message : string
        }

    type Config =
        {
            HandleInspectionResult : InspectionResult -> unit
            PreInspector : Inspector<InspectionResult>
            PostInspector : Inspector<InspectionResult>
        }