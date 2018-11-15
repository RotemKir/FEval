namespace FEval 

module EvaluationTypes =

    open System.Reflection
    open Microsoft.FSharp.Quotations
    open System.Collections.Generic
    open System
    
    type EvaluationState =
        {
            LastValue      : obj
            Variables      : Map<string, obj>
            RecVariables   : Dictionary<string, obj>
            EvalFunc       : EvaluationFunc
            Inspectors     : PreInspcetor seq
        }

    and EvaluationFunc = Expr -> EvaluationState -> EvaluationState

    and InspectionEvent =
        | ExprEvent of Expr
        | MethodEvent of MethodEventDetails

    and MethodEventDetails =
        {
            Method : MethodInfo
            Instance : obj
            Parameters : obj array 
            Result : obj option
        }

    and PreInspcetor = InspectionEvent -> EvaluationState -> PostInspector option

    and PostInspector = InspectionEvent -> EvaluationState -> unit

    and InpectionStage = Pre | Post

    and Inspector<'a> = DateTime -> InspectionEvent -> EvaluationState -> 'a