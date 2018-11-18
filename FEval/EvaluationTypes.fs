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
        | SetVariableEvent of SetVariableEventDetails
        | SetPropertyEvent of SetPropertyEventDetails

    and MethodEventDetails =
        {
            Method : MethodInfo
            Instance : obj
            Parameters : obj array 
            Result : obj option
        }

    and SetVariableEventDetails =
        {
            Variable : Var
            Value : obj
        }

    and SetPropertyEventDetails =
        {
            Property : PropertyInfo
            Instance : obj
            Value : obj
            IndexerParameters : obj array
        }

    and Inspector<'a> = InspectionContext -> 'a

    and PreInspcetor = Inspector<PostInspector option>

    and PostInspector = Inspector<unit>

    and InspectionStage = Pre | Post

    and InspectionContext =
        {
            InspectionStage : InspectionStage
            InspectionEvent : InspectionEvent
            Time : DateTime
            EvaluationState : EvaluationState
        }