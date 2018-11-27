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
            <| [| RuleDefinition.Variable ("Var", IsNotZero, Error) |]
            <| [| ValidationResult.Ok |]

    [<TestMethod>]
    member this.``runRules - is not zero error rule - int16 - variable is zero - returns error``() = 
        testRunRules 
            <| new Map<string, obj> [| ("Var", 0s :> obj) |]
            <| [| RuleDefinition.Variable ("Var", IsNotZero, Error) |]
            <| [| ValidationResult.Error "Variable Var should not be zero" |]
    
    [<TestMethod>]
    member this.``runRules - is not zero warning rule - int16 - variable is zero - returns warning``() = 
        testRunRules 
            <| new Map<string, obj> [| ("Var", 0s :> obj) |]
            <| [| RuleDefinition.Variable ("Var", IsNotZero, Warning) |]
            <| [| ValidationResult.Warning "Variable Var should not be zero" |]

    [<TestMethod>]
    member this.``runRules - is not zero rule - int16 - variable is not zero - returns ok``() = 
        testRunRules 
            <| new Map<string, obj> [| ("Var", 3s :> obj) |]
            <| [| RuleDefinition.Variable ("Var", IsNotZero, Error) |]
            <| [| ValidationResult.Ok |]

    [<TestMethod>]
    member this.``runRules - is not zero error rule - int32 - variable is zero - returns error``() = 
        testRunRules 
            <| new Map<string, obj> [| ("Var", 0 :> obj) |]
            <| [| RuleDefinition.Variable ("Var", IsNotZero, Error) |]
            <| [| ValidationResult.Error "Variable Var should not be zero" |]
    
    [<TestMethod>]
    member this.``runRules - is not zero warning rule - int32 - variable is zero - returns warning``() = 
        testRunRules 
            <| new Map<string, obj> [| ("Var", 0 :> obj) |]
            <| [| RuleDefinition.Variable ("Var", IsNotZero, Warning) |]
            <| [| ValidationResult.Warning "Variable Var should not be zero" |]

    [<TestMethod>]
    member this.``runRules - is not zero rule - int32 - variable is not zero - returns ok``() = 
        testRunRules 
            <| new Map<string, obj> [| ("Var", 3 :> obj) |]
            <| [| RuleDefinition.Variable ("Var", IsNotZero, Error) |]
            <| [| ValidationResult.Ok |]

    [<TestMethod>]
    member this.``runRules - is not zero error rule - int64 - variable is zero - returns error``() = 
        testRunRules 
            <| new Map<string, obj> [| ("Var", 0L :> obj) |]
            <| [| RuleDefinition.Variable ("Var", IsNotZero, Error) |]
            <| [| ValidationResult.Error "Variable Var should not be zero" |]
    
    [<TestMethod>]
    member this.``runRules - is not zero warning rule - int64 - variable is zero - returns warning``() = 
        testRunRules 
            <| new Map<string, obj> [| ("Var", 0L :> obj) |]
            <| [| RuleDefinition.Variable ("Var", IsNotZero, Warning) |]
            <| [| ValidationResult.Warning "Variable Var should not be zero" |]

    [<TestMethod>]
    member this.``runRules - is not zero rule - int64 - variable is not zero - returns ok``() = 
        testRunRules 
            <| new Map<string, obj> [| ("Var", 3L :> obj) |]
            <| [| RuleDefinition.Variable ("Var", IsNotZero, Error) |]
            <| [| ValidationResult.Ok |]
    
    [<TestMethod>]
    member this.``runRules - is not zero error rule - uint16 - variable is zero - returns error``() = 
        testRunRules 
            <| new Map<string, obj> [| ("Var", 0us :> obj) |]
            <| [| RuleDefinition.Variable ("Var", IsNotZero, Error) |]
            <| [| ValidationResult.Error "Variable Var should not be zero" |]
    
    [<TestMethod>]
    member this.``runRules - is not zero warning rule - uint16 - variable is zero - returns warning``() = 
        testRunRules 
            <| new Map<string, obj> [| ("Var", 0us :> obj) |]
            <| [| RuleDefinition.Variable ("Var", IsNotZero, Warning) |]
            <| [| ValidationResult.Warning "Variable Var should not be zero" |]

    [<TestMethod>]
    member this.``runRules - is not zero rule - uint16 - variable is not zero - returns ok``() = 
        testRunRules 
            <| new Map<string, obj> [| ("Var", 3us :> obj) |]
            <| [| RuleDefinition.Variable ("Var", IsNotZero, Error) |]
            <| [| ValidationResult.Ok |]

    [<TestMethod>]
    member this.``runRules - is not zero error rule - uint32 - variable is zero - returns error``() = 
        testRunRules 
            <| new Map<string, obj> [| ("Var", 0u :> obj) |]
            <| [| RuleDefinition.Variable ("Var", IsNotZero, Error) |]
            <| [| ValidationResult.Error "Variable Var should not be zero" |]
    
    [<TestMethod>]
    member this.``runRules - is not zero warning rule - uint32 - variable is zero - returns warning``() = 
        testRunRules 
            <| new Map<string, obj> [| ("Var", 0u :> obj) |]
            <| [| RuleDefinition.Variable ("Var", IsNotZero, Warning) |]
            <| [| ValidationResult.Warning "Variable Var should not be zero" |]

    [<TestMethod>]
    member this.``runRules - is not zero rule - uint32 - variable is not zero - returns ok``() = 
        testRunRules 
            <| new Map<string, obj> [| ("Var", 3u :> obj) |]
            <| [| RuleDefinition.Variable ("Var", IsNotZero, Error) |]
            <| [| ValidationResult.Ok |]

    [<TestMethod>]
    member this.``runRules - is not zero error rule - uint64 - variable is zero - returns error``() = 
        testRunRules 
            <| new Map<string, obj> [| ("Var", 0UL :> obj) |]
            <| [| RuleDefinition.Variable ("Var", IsNotZero, Error) |]
            <| [| ValidationResult.Error "Variable Var should not be zero" |]
    
    [<TestMethod>]
    member this.``runRules - is not zero warning rule - uint64 - variable is zero - returns warning``() = 
        testRunRules 
            <| new Map<string, obj> [| ("Var", 0UL :> obj) |]
            <| [| RuleDefinition.Variable ("Var", IsNotZero, Warning) |]
            <| [| ValidationResult.Warning "Variable Var should not be zero" |]

    [<TestMethod>]
    member this.``runRules - is not zero rule - uint64 - variable is not zero - returns ok``() = 
        testRunRules 
            <| new Map<string, obj> [| ("Var", 3UL :> obj) |]
            <| [| RuleDefinition.Variable ("Var", IsNotZero, Error) |]
            <| [| ValidationResult.Ok |]
                
    [<TestMethod>]
    member this.``runRules - is not zero error rule - byte - variable is zero - returns error``() = 
        testRunRules 
            <| new Map<string, obj> [| ("Var", 0uy :> obj) |]
            <| [| RuleDefinition.Variable ("Var", IsNotZero, Error) |]
            <| [| ValidationResult.Error "Variable Var should not be zero" |]
    
    [<TestMethod>]
    member this.``runRules - is not zero warning rule - byte - variable is zero - returns warning``() = 
        testRunRules 
            <| new Map<string, obj> [| ("Var", 0uy :> obj) |]
            <| [| RuleDefinition.Variable ("Var", IsNotZero, Warning) |]
            <| [| ValidationResult.Warning "Variable Var should not be zero" |]

    [<TestMethod>]
    member this.``runRules - is not zero rule - byte - variable is not zero - returns ok``() = 
        testRunRules 
            <| new Map<string, obj> [| ("Var", 3uy :> obj) |]
            <| [| RuleDefinition.Variable ("Var", IsNotZero, Error) |]
            <| [| ValidationResult.Ok |]

    [<TestMethod>]
    member this.``runRules - is not zero error rule - sbyte - variable is zero - returns error``() = 
        testRunRules 
            <| new Map<string, obj> [| ("Var", 0y :> obj) |]
            <| [| RuleDefinition.Variable ("Var", IsNotZero, Error) |]
            <| [| ValidationResult.Error "Variable Var should not be zero" |]
    
    [<TestMethod>]
    member this.``runRules - is not zero warning rule - sbyte - variable is zero - returns warning``() = 
        testRunRules 
            <| new Map<string, obj> [| ("Var", 0y :> obj) |]
            <| [| RuleDefinition.Variable ("Var", IsNotZero, Warning) |]
            <| [| ValidationResult.Warning "Variable Var should not be zero" |]

    [<TestMethod>]
    member this.``runRules - is not zero rule - sbyte - variable is not zero - returns ok``() = 
        testRunRules 
            <| new Map<string, obj> [| ("Var", 3y :> obj) |]
            <| [| RuleDefinition.Variable ("Var", IsNotZero, Error) |]
            <| [| ValidationResult.Ok |]
                
    [<TestMethod>]
    member this.``runRules - is not zero error rule - float - variable is zero - returns error``() = 
        testRunRules 
            <| new Map<string, obj> [| ("Var", 0.0 :> obj) |]
            <| [| RuleDefinition.Variable ("Var", IsNotZero, Error) |]
            <| [| ValidationResult.Error "Variable Var should not be zero" |]
    
    [<TestMethod>]
    member this.``runRules - is not zero warning rule - float - variable is zero - returns warning``() = 
        testRunRules 
            <| new Map<string, obj> [| ("Var", 0.0 :> obj) |]
            <| [| RuleDefinition.Variable ("Var", IsNotZero, Warning) |]
            <| [| ValidationResult.Warning "Variable Var should not be zero" |]

    [<TestMethod>]
    member this.``runRules - is not zero rule - float - variable is not zero - returns ok``() = 
        testRunRules 
            <| new Map<string, obj> [| ("Var", 3.0 :> obj) |]
            <| [| RuleDefinition.Variable ("Var", IsNotZero, Error) |]
            <| [| ValidationResult.Ok |]
    
    [<TestMethod>]
    member this.``runRules - is not zero error rule - float32 - variable is zero - returns error``() = 
        testRunRules 
            <| new Map<string, obj> [| ("Var", 0.0f :> obj) |]
            <| [| RuleDefinition.Variable ("Var", IsNotZero, Error) |]
            <| [| ValidationResult.Error "Variable Var should not be zero" |]
    
    [<TestMethod>]
    member this.``runRules - is not zero warning rule - float32 - variable is zero - returns warning``() = 
        testRunRules 
            <| new Map<string, obj> [| ("Var", 0.0f :> obj) |]
            <| [| RuleDefinition.Variable ("Var", IsNotZero, Warning) |]
            <| [| ValidationResult.Warning "Variable Var should not be zero" |]

    [<TestMethod>]
    member this.``runRules - is not zero rule - float32 - variable is not zero - returns ok``() = 
        testRunRules 
            <| new Map<string, obj> [| ("Var", 3.0f :> obj) |]
            <| [| RuleDefinition.Variable ("Var", IsNotZero, Error) |]
            <| [| ValidationResult.Ok |]
    
    [<TestMethod>]
    member this.``runRules - is not zero error rule - decimal - variable is zero - returns error``() = 
        testRunRules 
            <| new Map<string, obj> [| ("Var", 0.0m :> obj) |]
            <| [| RuleDefinition.Variable ("Var", IsNotZero, Error) |]
            <| [| ValidationResult.Error "Variable Var should not be zero" |]
    
    [<TestMethod>]
    member this.``runRules - is not zero warning rule - decimal - variable is zero - returns warning``() = 
        testRunRules 
            <| new Map<string, obj> [| ("Var", 0.0m :> obj) |]
            <| [| RuleDefinition.Variable ("Var", IsNotZero, Warning) |]
            <| [| ValidationResult.Warning "Variable Var should not be zero" |]

    [<TestMethod>]
    member this.``runRules - is not zero rule - decimal - variable is not zero - returns ok``() = 
        testRunRules 
            <| new Map<string, obj> [| ("Var", 3.0m :> obj) |]
            <| [| RuleDefinition.Variable ("Var", IsNotZero, Error) |]
            <| [| ValidationResult.Ok |]