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

    type InvalidWhen =
        | IsZero
        | IsNegative
        | IsEmpty
        | IsLessThan of RuleTarget
        | IsMoreThan of RuleTarget

    type ErrorLevel =
        | Warning
        | Error

    type RuleDefinition =
        | Variable of VariableRuleDefinition
        | Custom of Rule

    and VariableRuleDefinition =
        {
            VariableName : string
            InvalidWhen : InvalidWhen
            ErrorLevel : ErrorLevel
        }

    type VariableValidation =
        {
            IsValid : obj -> bool
            FormatError : string -> string
        }
