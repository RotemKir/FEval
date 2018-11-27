namespace FEval.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open FEval.Inspections
open FEval.Inspections.ValidationTypes

[<TestClass>]
type ValidatorTests() =
    
    let createValidationContext variables =
        {
            Variables = variables
        }

    let assertLists expectedValues actualValues =
        Seq.iter2 (fun expected actual -> Assert.AreEqual(expected, actual)) expectedValues actualValues

    let testRunRules variables rules expctedResults =
        Validator.runRules
            <| createValidationContext variables
            <| rules
        |> assertLists expctedResults

    [<TestMethod>]
    member this.``runRules - no rules - returns empty result``() = 
        testRunRules 
            <| new Map<string, obj> [||]
            <| [||]
            <| [||]
    
    [<TestMethod>]
    member this.``runRules - custom rule - result is ok - returns ok``() = 
        testRunRules 
            <| new Map<string, obj> [||]
            <| [| RuleDefinition.Custom (fun _ -> ValidationResult.Ok) |]
            <| [| ValidationResult.Ok |]
    
    [<TestMethod>]
    member this.``runRules - custom rule - result is warning - returns warning``() = 
        testRunRules 
            <| new Map<string, obj> [||]
            <| [| RuleDefinition.Custom (fun _ -> ValidationResult.Warning "Warning") |]
            <| [| ValidationResult.Warning "Warning" |]

    [<TestMethod>]
    member this.``runRules - custom rule - result is error - returns error``() = 
        testRunRules 
            <| new Map<string, obj> [||]
            <| [| RuleDefinition.Custom (fun _ -> ValidationResult.Error "Error") |]
            <| [| ValidationResult.Error "Error" |]
    
    [<TestMethod>]
    member this.``runRules - is not zero rule - variable doesn't exist - returns ok``() = 
        testRunRules 
            <| new Map<string, obj> [||]
            <| [| RuleDefinition.Variable ("Var", IsNotZero) |]
            <| [| ValidationResult.Ok |]

    [<TestMethod>]
    member this.``runRules - is not zero rule - variable is zero int16 - returns error``() = 
        testRunRules 
            <| new Map<string, obj> [| ("Var", 0s :> obj) |]
            <| [| RuleDefinition.Variable ("Var", IsNotZero) |]
            <| [| ValidationResult.Error "Variable Var should not be zero" |]
    
    [<TestMethod>]
    member this.``runRules - is not zero rule - variable is not zero int16 - returns ok``() = 
        testRunRules 
            <| new Map<string, obj> [| ("Var", 3s :> obj) |]
            <| [| RuleDefinition.Variable ("Var", IsNotZero) |]
            <| [| ValidationResult.Ok |]

