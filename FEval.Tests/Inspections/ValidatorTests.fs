namespace FEval.Tests

open Microsoft.FSharp.Quotations
open Microsoft.VisualStudio.TestTools.UnitTesting
open FEval.Inspections
open FEval.Inspections.ValidationsCommon
open FEval.EvaluationTypes

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

    let createValidationContext variables evaluationEvent =
        {
            Variables = variables
            EvaluationEvent = evaluationEvent
        }

    let createVariableEvent name =
        SetVariableEvent { Variable = new Var (name, null) ; Value = null }

    let testRunRules evaluationEvent variables rule expctedResult =
        let validationResult = 
            Validator.runRule
                <| createValidationContext variables evaluationEvent
                <| rule
        Assert.AreEqual(expctedResult, validationResult)

    [<TestMethod>]
    member __.``runRule - custom rule - result is ok - returns ok``() = 
        testRunRules 
            <| createVariableEvent "Var"
            <| new Map<string, obj> [||]
            <| CustomRule (fun _ -> ValidationResult.Ok)
            <| ValidationResult.Ok
    
    [<TestMethod>]
    member __.``runRule - custom rule - result is warning - returns warning``() = 
        testRunRules 
            <| createVariableEvent "Var"
            <| new Map<string, obj> [||]
            <| CustomRule (fun _ -> ValidationResult.Warning "Warning")
            <| ValidationResult.Warning "Warning"

    [<TestMethod>]
    member __.``runRule - custom rule - result is error - returns error``() = 
        testRunRules 
            <| createVariableEvent "Var"
            <| new Map<string, obj> [||]
            <| CustomRule (fun _ -> ValidationResult.Error "Error")
            <| ValidationResult.Error "Error"
    
    [<TestMethod>]
    member __.``runRule - variable error rule - variable doesn't exist - returns ok``() = 
        testRunRules 
            <| createVariableEvent "Var"
            <| new Map<string, obj> [||]
            <| (VariableRule <| createVariableRule "Var" true ReturnError)
            <| ValidationResult.Ok
            
    [<TestMethod>]
    member __.``runRule - variable error rule - is valid - returns ok``() = 
        testRunRules 
            <| createVariableEvent "Var"
            <| new Map<string, obj> [| ("Var", 0 :> obj) |]
            <| (VariableRule <| createVariableRule "Var" true ReturnError)
            <| ValidationResult.Ok
            
    [<TestMethod>]
    member __.``runRule - variable error rule - is invalid - returns error``() = 
        testRunRules 
            <| createVariableEvent "Var"
            <| new Map<string, obj> [| ("Var", 0 :> obj) |]
            <| (VariableRule <| createVariableRule "Var" false ReturnError)
            <| ValidationResult.Error "Var"
                
    [<TestMethod>]
    member __.``runRule - variable warning rule - variable doesn't exist - returns ok``() = 
        testRunRules 
            <| createVariableEvent "Var"
            <| new Map<string, obj> [||]
            <| (VariableRule <| createVariableRule "Var" true ReturnWarning)
            <| ValidationResult.Ok
            
    [<TestMethod>]
    member __.``runRule - variable warning rule - is valid - returns ok``() = 
        testRunRules 
            <| createVariableEvent "Var"
            <| new Map<string, obj> [| ("Var", 0 :> obj) |]
            <| (VariableRule <| createVariableRule "Var" true ReturnWarning)
            <| ValidationResult.Ok
            
    [<TestMethod>]
    member __.``runRule - variable warning rule - is invalid - returns warning``() = 
        testRunRules 
            <| createVariableEvent "Var"
            <| new Map<string, obj> [| ("Var", 0 :> obj) |]
            <| (VariableRule <| createVariableRule "Var" false ReturnWarning)
            <| ValidationResult.Warning "Var"