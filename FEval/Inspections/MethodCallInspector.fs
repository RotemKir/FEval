namespace FEval.Inspections

[<RequireQualifiedAccess>]
module MethodCallInspector =
    open System
    open FEval.Inspections.CommonInspections

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

    let private postInspector config startTime inspectionEvent _ =
        inspectMethodEvent 
            inspectionEvent 
            (fun m -> {Time = startTime ; Method = m.Method.Name ; Message = ""})
        |> Option.get
        |> config.HandleInspectionResult

    // Public Functions

    let createNew config _ _ =
        Some <| postInspector config DateTime.Now
