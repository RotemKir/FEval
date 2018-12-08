﻿namespace FEval.Inspections

open FEval.EvaluationTypes
open System.Collections.Generic

module ValidationsCommon =

    type ValidationContext =
        {
            Variables : Map<string, obj>
        }

    type ValidationResult =
        | Ok
        | Warning of string
        | Error of string
    
    type RuleTarget =
        | Value of obj
        | Variable of name : string

    type InvalidWhen =
        | IsZero
        | IsNegative
        | IsEmpty
        | IsLessThan of RuleTarget
        | IsMoreThan of RuleTarget

    type ReturnWhenInvalid =
        | ReturnWarning
        | ReturnError

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
            FormatMessage : FormatMessageRequest -> string
        }

    and FormatMessageRequest =
        {
            VariableName : string
            Value : obj
            ValidationContext : ValidationContext
        }

    and CustomRule = ValidationContext -> ValidationResult

    let getVariableValue validationContext name =
        Map.tryFind name validationContext.Variables

    let createValidationContext (inspectionContext : InspectionContext) =
        {
            Variables = 
                Seq.fold 
                    <| (fun vars (pair : KeyValuePair<string, obj>) -> Map.add pair.Key pair.Value vars)
                    <| inspectionContext.EvaluationState.Variables
                    <| inspectionContext.EvaluationState.RecVariables
        }