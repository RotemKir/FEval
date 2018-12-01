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
    
    type RuleTarget =
        | Const of obj
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
        | Variable of VariableRuleDefinition
        | Custom of CustomRule

    and VariableRuleDefinition =
        {
            VariableName : string
            Validation : VariableValidation
            ReturnWhenInvalid : ReturnWhenInvalid
        }

    and VariableValidation =
        {
            IsValid : obj -> bool
            FormatError : string -> string
        }

    and CustomRule = ValidationContext -> ValidationResult
