namespace FEval 

module EvaluationTypes =

    open System.Reflection
    open Microsoft.FSharp.Quotations
    open System.Collections.Generic
    open System
    
    type EvaluationState =
        {
            RunDetails     : EvaluationRunDetails
            LastValue      : obj
            Variables      : Map<string, obj>
            RecVariables   : Dictionary<string, obj>
            EvalFunc       : EvaluationFunc
            Inspectors     : Inspector seq
        }

    and EvaluationRunDetails =
        {
            RunId : Guid
            ProcessId : int
            ProcessName : string
            ThreadId : int
        }

    and EvaluationFunc = Expr -> EvaluationState -> EvaluationState

    and EvaluationEvent =
        | ExprEvent        of Expr
        | MethodEvent      of MethodEventDetails
        | SetVariableEvent of SetVariableEventDetails
        | SetPropertyEvent of SetPropertyEventDetails
        | SetFieldEvent    of SetFieldEventDetails

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

    and SetFieldEventDetails =
        {
            Field : FieldInfo
            Instance : obj
            Value : obj
        }

    and InspectionStage = Pre | Post

    and InspectionContext =
        {
            InspectionStage : InspectionStage
            EvaluationEvent : EvaluationEvent
            Time : DateTime
            EvaluationState : EvaluationState
        }
        
    and InspectionMessage = 
        | PreInspectionMessage of PreMessage : InspectionContext
        | PostInspectionMessage of PreMessage : InspectionContext * PostMessage : InspectionContext
        | Dispose of replyChannel : AsyncReplyChannel<unit>
        | Sync of replyChannel : AsyncReplyChannel<unit>
         
    and Inspector = MailboxProcessor<InspectionMessage>

    and LogEvent<'a> =
        {
            Time : DateTime
            RunDetails : EvaluationRunDetails
            InspectionResult : 'a
        }