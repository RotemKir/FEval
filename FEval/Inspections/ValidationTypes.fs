namespace FEval.Inspections

module ValidationTypes =
    open FEval.EvaluationTypes

    type ValidationResult =
        | Ok
        | Warning of string
        | Error of string

    type Rule = InspectionContext -> ValidationResult

    type RuleTarget =
        | Const of obj
        | Variable of name : string

    type Test =
        | IsNotZero
        | IsNotNegative
        | IsNotEmpty
        | IsLessThan of RuleTarget
        | IsMoreThan of RuleTarget

    type RuleDefinition =
        | Variable of name : string * test : Test
        | Custom of Rule



