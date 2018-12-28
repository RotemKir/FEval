namespace FEval.Inspectors

open FEval.EvaluationTypes
open System.Collections.Generic

module ValidationsCommon =

    type ValidationContext =
        {
            Variables : Map<string, obj>
            EvaluationEvent : EvaluationEvent
        }

    type ValidationResult =
        | Ok
        | Warning of string
        | Error of string
    
    type RuleTarget =
        | Value of obj
        | Variable of name : string

    type InvalidWhen =
        | ``Is Zero``
        | ``Is Negative``
        | ``Is Empty``
        | Is of RuleTarget
        | ``Is Less Than`` of RuleTarget
        | ``Is More Than`` of RuleTarget
        | And of InvalidWhen * InvalidWhen
        | Or of InvalidWhen * InvalidWhen
        static member (&&&) (leftOperand, rightOperand) = And (leftOperand, rightOperand)
        static member (|||) (leftOperand, rightOperand) = Or (leftOperand, rightOperand)
        
    type ReturnWhenInvalid =
        | ``Return Warning``
        | ``Return Error``

    type RuleDefinition =
        | VariableRule of VariableRuleDefinition
        | CustomRule of CustomRule

    and VariableRuleDefinition =
        {
            VariableName : string
            Validation : VariableValidation
            ReturnWhenInvalid : ReturnWhenInvalid
        }

    and ValidationRequest =
        {
            Value : obj
            ValidationContext : ValidationContext
        }

    and VariableValidation =
        {
            IsValid : ValidationRequest -> bool
            FormatMessage : ValidationRequest -> string
        }

    and CustomRule = ValidationContext -> ValidationResult

    let getVariableValue validationContext name =
        Map.tryFind name validationContext.Variables

    let createValidationContext inspectionContext =
        {
            Variables = 
                Seq.fold 
                    <| (fun vars (pair : KeyValuePair<string, obj>) -> Map.add pair.Key pair.Value vars)
                    <| inspectionContext.EvaluationState.Variables
                    <| inspectionContext.EvaluationState.RecVariables
            EvaluationEvent = inspectionContext.EvaluationEvent
        }