namespace FEval.Inspections

module ValidationTypes =

    type ValidationContext =
        {
            Variables : Map<string, obj>
        }

    type ValidationResult =
        | Ok
        | Warning of string
        | Error of string

    type Rule = ValidationContext -> ValidationResult

    type RuleTarget =
        | Const of obj
        | Variable of name : string

    type Test =
        | IsNotZero
        | IsNotNegative
        | IsNotEmpty
        | IsLessThan of RuleTarget
        | IsMoreThan of RuleTarget

    type ErrorLevel =
        | Warning
        | Error

    type RuleDefinition =
        | Variable of name : string * test : Test * errorLevel : ErrorLevel
        | Custom of Rule



