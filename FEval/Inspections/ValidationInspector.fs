namespace FEval.Inspections

[<RequireQualifiedAccess>]
module Valudate =
    open FEval.EvaluationTypes
    
    type Rule = InspectionContext -> Result<unit, string>

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

    let private runRule definition inspectionContext =
        match definition with
        | Variable (name, test) -> Result.Ok()
        | Custom rule           -> rule inspectionContext