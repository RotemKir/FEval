namespace FEval.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open FEval.Inspections
open FEval.Inspections.ValidationTypes

[<TestClass>]
type ValidatorTests() =
    
    let createVariableRule name isValid errorLevel =
        {
            VariableName = name 
            Validation =  
                { 
                    IsValid = fun _ -> isValid
                    FormatMessage = fun request -> request.VariableName
                } 
            ReturnWhenInvalid = errorLevel
        }

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
            <| [| CustomRule (fun _ -> ValidationResult.Ok) |]
            <| [| ValidationResult.Ok |]
    
    [<TestMethod>]
    member this.``runRules - custom rule - result is warning - returns warning``() = 
        testRunRules 
            <| new Map<string, obj> [||]
            <| [| CustomRule (fun _ -> ValidationResult.Warning "Warning") |]
            <| [| ValidationResult.Warning "Warning" |]

    [<TestMethod>]
    member this.``runRules - custom rule - result is error - returns error``() = 
        testRunRules 
            <| new Map<string, obj> [||]
            <| [| CustomRule (fun _ -> ValidationResult.Error "Error") |]
            <| [| ValidationResult.Error "Error" |]
    
    [<TestMethod>]
    member this.``runRules - variable error rule - variable doesn't exist - returns ok``() = 
        testRunRules 
            <| new Map<string, obj> [||]
            <| [| VariableRule <| createVariableRule "Var" true ReturnError |]
            <| [| ValidationResult.Ok |]
            
    [<TestMethod>]
    member this.``runRules - variable error rule - is valid - returns ok``() = 
        testRunRules 
            <| new Map<string, obj> [| ("Var", 0 :> obj) |]
            <| [| VariableRule <| createVariableRule "Var" true ReturnError |]
            <| [| ValidationResult.Ok |]
            
    [<TestMethod>]
    member this.``runRules - variable error rule - is invalid - returns error``() = 
        testRunRules 
            <| new Map<string, obj> [| ("Var", 0 :> obj) |]
            <| [| VariableRule <| createVariableRule "Var" false ReturnError |]
            <| [| ValidationResult.Error "Var" |]
                
    [<TestMethod>]
    member this.``runRules - variable warning rule - variable doesn't exist - returns ok``() = 
        testRunRules 
            <| new Map<string, obj> [||]
            <| [| VariableRule <| createVariableRule "Var" true ReturnWarning |]
            <| [| ValidationResult.Ok |]
            
    [<TestMethod>]
    member this.``runRules - variable warning rule - is valid - returns ok``() = 
        testRunRules 
            <| new Map<string, obj> [| ("Var", 0 :> obj) |]
            <| [| VariableRule <| createVariableRule "Var" true ReturnWarning |]
            <| [| ValidationResult.Ok |]
            
    [<TestMethod>]
    member this.``runRules - variable warning rule - is invalid - returns warning``() = 
        testRunRules 
            <| new Map<string, obj> [| ("Var", 0 :> obj) |]
            <| [| VariableRule <| createVariableRule "Var" false ReturnWarning |]
            <| [| ValidationResult.Warning "Var" |]