namespace FEval.Tests

open Microsoft.FSharp.Quotations
open Microsoft.VisualStudio.TestTools.UnitTesting
open FEval.Inspections.ValidationsCommon
open FEval.Inspections.ValidationRules
open FEval.EvaluationTypes
open System

[<TestClass>]
type ValidationRulesTests() =

    let createValidationContext evaluationEvent = 
        {
            Variables = new Map<string, obj> [||]
            EvaluationEvent = evaluationEvent
        }

    let createVariableValidationContext name = 
        createValidationContext 
            <| SetVariableEvent { Variable = new Var (name, null) ; Value = null }

    let assertVariableRule rule assertion =
        match rule with
        | VariableRule definition -> assertion definition
        | _                       -> Assert.Fail("Not a variable rule")

    let assertVariableRuleErrorMessage rule value expectedErrorMessage validationContext =
        assertVariableRule 
            <| rule
            <| fun definition ->
                let actualErrorMessage = 
                    definition.Validation.FormatMessage 
                        {
                            Value = value
                            ValidationContext = validationContext
                        }
                Assert.AreEqual(expectedErrorMessage, actualErrorMessage)

    let assertVariableRuleIsValid rule value isValid validationContext =
        let validationRequest = { Value = value ; ValidationContext = validationContext }
        assertVariableRule 
            <| rule
            <| fun definition ->
                Assert.AreEqual(isValid, definition.Validation.IsValid validationRequest)

    [<TestMethod>]
    member __.``ifVariable - set variable name - reutrns rule with variable name``() = 
        assertVariableRule 
            <| ifVariable "Var" IsZero ReturnError
            <| fun definition -> Assert.AreEqual("Var", definition.VariableName)
    
    [<TestMethod>]
    member __.``ifVariable - returns error when invalid - reutrns rule that returns error``() = 
        assertVariableRule 
            <| ifVariable "Var" IsZero ReturnError 
            <| fun definition -> Assert.AreEqual(ReturnError, definition.ReturnWhenInvalid)
    
    [<TestMethod>]
    member __.``ifVariable - returns warning when invalid - reutrns rule that returns warning``() = 
        assertVariableRule 
            <| ifVariable "Var" IsZero ReturnWarning
            <| fun definition -> Assert.AreEqual(ReturnWarning, definition.ReturnWhenInvalid)

    [<TestMethod>]
    member __.``ifVariable - is zero - formats error message as error``() = 
        assertVariableRuleErrorMessage 
            <| ifVariable "Var" IsZero ReturnError
            <| 0
            <| "should not be zero"
            <| createVariableValidationContext "Var"
                
    [<TestMethod>]
    member __.``ifVariable - is zero - int16 - is zero - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsZero ReturnError
            <| 0s
            <| false
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``ifVariable - is zero - int16 - is not zero - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsZero ReturnError
            <| 3s
            <| true
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``ifVariable - is zero - int32 - is zero - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsZero ReturnError
            <| 0
            <| false
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``ifVariable - is zero - int32 - is not zero - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsZero ReturnError
            <| 3
            <| true
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``ifVariable - is zero - int64 - is zero - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsZero ReturnError
            <| 0L
            <| false
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``ifVariable - is zero - int64 - is not zero - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsZero ReturnError
            <| 3L
            <| true
            <| createVariableValidationContext "Var"
                        
    [<TestMethod>]
    member __.``ifVariable - is zero - uint16 - is zero - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsZero ReturnError
            <| 0us
            <| false
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``ifVariable - is zero - uint16 - is not zero - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsZero ReturnError
            <| 3us
            <| true
            <| createVariableValidationContext "Var"
                       
    [<TestMethod>]
    member __.``ifVariable - is zero - uint32 - is zero - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsZero ReturnError
            <| 0u
            <| false
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``ifVariable - is zero - uint32 - is not zero - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsZero ReturnError
            <| 3u
            <| true
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``ifVariable - is zero - uint64 - is zero - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsZero ReturnError
            <| 0UL
            <| false
            <| createVariableValidationContext "Var"
                       
    [<TestMethod>]
    member __.``ifVariable - is zero - uint64 - is not zero - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsZero ReturnError
            <| 3UL
            <| true
            <| createVariableValidationContext "Var"
                
    [<TestMethod>]
    member __.``ifVariable - is zero - byte - is zero - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsZero ReturnError
            <| 0uy
            <| false
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``ifVariable - is zero - byte - is not zero - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsZero ReturnError
            <| 3uy
            <| true
            <| createVariableValidationContext "Var"
            
    [<TestMethod>]
    member __.``ifVariable - is zero - sbyte - is zero - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsZero ReturnError
            <| 0y
            <| false
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``ifVariable - is zero - sbyte - is not zero - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsZero ReturnError
            <| 3y
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``ifVariable - is zero - float - is zero - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsZero ReturnError
            <| 0.0
            <| false
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``ifVariable - is zero - float - is not zero - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsZero ReturnError
            <| 3.0
            <| true
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``ifVariable - is zero - float32 - is zero - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsZero ReturnError
            <| 0.0f
            <| false
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``ifVariable - is zero - float32 - is not zero - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsZero ReturnError
            <| 3.0f
            <| true
            <| createVariableValidationContext "Var"
                        
    [<TestMethod>]
    member __.``ifVariable - is zero - decimal - is zero - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsZero ReturnError
            <| 0.0m
            <| false
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``ifVariable - is zero - decimal - is not zero - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsZero ReturnError
            <| 3.0m
            <| true
            <| createVariableValidationContext "Var"
            
    [<TestMethod>]
    member __.``ifVariable - is zero - string - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsZero ReturnError
            <| "Hello"
            <| true
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``ifVariable - is negative - formats error message as error``() = 
        assertVariableRuleErrorMessage 
            <| ifVariable "Var" IsNegative ReturnError
            <| -8
            <| "should not be negative"
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``ifVariable - is negative - int16 - is negative - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsNegative ReturnError
            <| -8s
            <| false
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``ifVariable - is negative - int16 - is not negative - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsNegative ReturnError
            <| 3s
            <| true
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``ifVariable - is negative - int32 - is negative - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsNegative ReturnError
            <| -8
            <| false
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``ifVariable - is negative - int32 - is not negative - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsNegative ReturnError
            <| 3
            <| true
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``ifVariable - is negative - int64 - is negative - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsNegative ReturnError
            <| -8L
            <| false
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``ifVariable - is negative - int64 - is not negative - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsNegative ReturnError
            <| 3L
            <| true
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``ifVariable - is negative - uint16 - is not negative - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsNegative ReturnError
            <| 3us
            <| true
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``ifVariable - is negative - uint32 - is not negative - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsNegative ReturnError
            <| 3u
            <| true
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``ifVariable - is negative - uint64 - is not negative - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsNegative ReturnError
            <| 3UL
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``ifVariable - is negative - byte - is not negative - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsNegative ReturnError
            <| 3uy
            <| true
            <| createVariableValidationContext "Var"
            
    [<TestMethod>]
    member __.``ifVariable - is negative - sbyte - is negative - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsNegative ReturnError
            <| -8y
            <| false
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``ifVariable - is negative - sbyte - is not negative - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsNegative ReturnError
            <| 3y
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``ifVariable - is negative - float - is negative - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsNegative ReturnError
            <| -8.0
            <| false
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``ifVariable - is negative - float - is not negative - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsNegative ReturnError
            <| 3.0
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``ifVariable - is negative - float32 - is negative - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsNegative ReturnError
            <| -8.0f
            <| false
            <| createVariableValidationContext "Var"
            
    [<TestMethod>]
    member __.``ifVariable - is negative - float32 - is not negative - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsNegative ReturnError
            <| 3.0f
            <| true
            <| createVariableValidationContext "Var"
            
    [<TestMethod>]
    member __.``ifVariable - is negative - decimal - is negative - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsNegative ReturnError
            <| -8.0m
            <| false
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``ifVariable - is negative - decimal - is not negative - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsNegative ReturnError
            <| 3.0m
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``ifVariable - is negative - string - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsNegative ReturnError
            <| "Hello"
            <| true
            <| createVariableValidationContext "Var"
            
    [<TestMethod>]
    member __.``ifVariable - is empty - formats error message``() = 
        assertVariableRuleErrorMessage 
            <| ifVariable "Var" IsEmpty ReturnError
            <| [||]
            <| "should not be empty"
            <| createVariableValidationContext "Var"
                
    [<TestMethod>]
    member __.``ifVariable - is empty - string - is empty - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsEmpty ReturnError
            <| ""
            <| false
            <| createVariableValidationContext "Var"
                  
    [<TestMethod>]
    member __.``ifVariable - is empty - string - is not empty - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsEmpty ReturnError
            <| "Hello"
            <| true
            <| createVariableValidationContext "Var"
                
    [<TestMethod>]
    member __.``ifVariable - is empty - int - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsEmpty ReturnError
            <| 8
            <| true
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``ifVariable - is empty - seq - is empty - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsEmpty ReturnError
            <| Seq.ofArray [||]
            <| false
            <| createVariableValidationContext "Var"
        
    [<TestMethod>]
    member __.``ifVariable - is empty - seq - is not empty - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsEmpty ReturnError
            <| seq {1 .. 10}
            <| true
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``ifVariable - is empty - array - is empty - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsEmpty ReturnError
            <| [||]
            <| false
            <| createVariableValidationContext "Var"
        
    [<TestMethod>]
    member __.``ifVariable - is empty - array - is not empty - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsEmpty ReturnError
            <| [| 1 ; 2 ; 3 |]
            <| true
            <| createVariableValidationContext "Var"
     
    [<TestMethod>]
    member __.``ifVariable - is empty - list - is empty - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsEmpty ReturnError
            <| []
            <| false
            <| createVariableValidationContext "Var"
                
    [<TestMethod>]
    member __.``ifVariable - is empty - list - is not empty - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsEmpty ReturnError
            <| [ 1 ; 2 ; 3 ]
            <| true
            <| createVariableValidationContext "Var"
            
    [<TestMethod>]
    member __.``ifVariable - is less than value - formats error message and target value as error``() = 
        assertVariableRuleErrorMessage 
            <| ifVariable "Var" (IsLessThan <| Value 4) ReturnError
            <| 3
            <| "should not be less than 4 : Int32"
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``ifVariable - is less than variable - formats error message and target namd and value as error``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4 :> obj) |]
            }
        assertVariableRuleErrorMessage 
            <| ifVariable "Var" (IsLessThan <| Variable "Other Var") ReturnError
            <| 3
            <| "should not be less than variable 'Other Var', 4 : Int32"
            <| validationContext

    [<TestMethod>]
    member __.``ifVariable - is less than variable that doesn't exist - formats error message as error``() = 
        assertVariableRuleErrorMessage 
            <| ifVariable "Var" (IsLessThan <| Variable "Other Var") ReturnError
            <| 3
            <| "should not be less than (null)"
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``ifVariable - is less than value - int16 - is more than - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Value 4s) ReturnError
            <| 6s
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``ifVariable - is less than value - int16 - is equal - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Value 4s) ReturnError
            <| 4s
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``ifVariable - is less than value - int16 - is less than - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Value 4s) ReturnError
            <| 3s
            <| false
            <| createVariableValidationContext "Var"
            
    [<TestMethod>]
    member __.``ifVariable - is less than value - int32 - is more than - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Value 4) ReturnError
            <| 6
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``ifVariable - is less than value - int32 - is equal - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Value 4) ReturnError
            <| 4
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``ifVariable - is less than value - int32 - is less than - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Value 4) ReturnError
            <| 3
            <| false
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``ifVariable - is less than value - int64 - is more than - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Value 4L) ReturnError
            <| 6L
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``ifVariable - is less than value - int64 - is equal - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Value 4L) ReturnError
            <| 4L
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``ifVariable - is less than value - int64 - is less than - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Value 4L) ReturnError
            <| 3L
            <| false
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``ifVariable - is less than value - uint16 - is more than - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Value 4us) ReturnError
            <| 6us
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``ifVariable - is less than value - uint16 - is equal - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Value 4us) ReturnError
            <| 4us
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``ifVariable - is less than value - uint16 - is less than - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Value 4us) ReturnError
            <| 3us
            <| false
            <| createVariableValidationContext "Var"
            
    [<TestMethod>]
    member __.``ifVariable - is less than value - uint32 - is more than - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Value 4u) ReturnError
            <| 6u
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``ifVariable - is less than value - uint32 - is equal - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Value 4u) ReturnError
            <| 4u
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``ifVariable - is less than value - uint32 - is less than - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Value 4u) ReturnError
            <| 3u
            <| false
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``ifVariable - is less than value - uint64 - is more than - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Value 4UL) ReturnError
            <| 6UL
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``ifVariable - is less than value - uint64 - is equal - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Value 4UL) ReturnError
            <| 4UL
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``ifVariable - is less than value - uint64 - is less than - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Value 4UL) ReturnError
            <| 3UL
            <| false
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``ifVariable - is less than value - byte - is more than - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Value 4uy) ReturnError
            <| 6uy
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``ifVariable - is less than value - byte - is equal - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Value 4uy) ReturnError
            <| 4uy
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``ifVariable - is less than value - byte - is less than - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Value 4uy) ReturnError
            <| 3uy
            <| false
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``ifVariable - is less than value - sbyte - is more than - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Value 4y) ReturnError
            <| 6y
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``ifVariable - is less than value - sbyte - is equal - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Value 4y) ReturnError
            <| 4y
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``ifVariable - is less than value - sbyte - is less than - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Value 4y) ReturnError
            <| 3y
            <| false
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``ifVariable - is less than value - float - is more than - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Value 4.0) ReturnError
            <| 6.0
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``ifVariable - is less than value - float - is equal - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Value 4.0) ReturnError
            <| 4.0
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``ifVariable - is less than value - float - is less than - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Value 4.0) ReturnError
            <| 3.0
            <| false
            <| createVariableValidationContext "Var"
            
    [<TestMethod>]
    member __.``ifVariable - is less than value - float32 - is more than - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Value 4.0f) ReturnError
            <| 6.0f
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``ifVariable - is less than value - float32 - is equal - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Value 4.0f) ReturnError
            <| 4.0f
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``ifVariable - is less than value - float32 - is less than - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Value 4.0f) ReturnError
            <| 3.0f
            <| false
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``ifVariable - is less than value - decimal - is more than - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Value 4.0m) ReturnError
            <| 6.0m
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``ifVariable - is less than value - decimal - is equal - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Value 4.0m) ReturnError
            <| 4.0m
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``ifVariable - is less than value - decimal - is less than - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Value 4.0m) ReturnError
            <| 3.0m
            <| false
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``ifVariable - is less than value - datetime - is more than - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Value (new DateTime(2018, 1, 29))) ReturnError
            <| new DateTime(2018, 2, 20)
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``ifVariable - is less than value - datetime - is equal - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Value (new DateTime(2018, 1, 29))) ReturnError
            <| new DateTime(2018, 1, 29)
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``ifVariable - is less than value - datetime - is less than - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Value (new DateTime(2018, 1, 29))) ReturnError
            <| new DateTime(2018, 1, 20)
            <| false
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``ifVariable - is less than variable - int16 - variable doesn't exist - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Variable "Other Var") ReturnError
            <| 6s
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``ifVariable - is less than variable - int16 - is more than - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4s :> obj) |]
            }
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Variable "Other Var") ReturnError
            <| 6s
            <| true
            <| validationContext 
    
    [<TestMethod>]
    member __.``ifVariable - is less than variable - int16 - is equal - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4s :> obj) |]
            }
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Variable "Other Var") ReturnError
            <| 4s
            <| true
            <| validationContext
     
    [<TestMethod>]
    member __.``ifVariable - is less than variable - int16 - is less than - returns is valid false``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4s :> obj) |]
            }
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Variable "Other Var") ReturnError
            <| 3s
            <| false
            <| validationContext

    [<TestMethod>]
    member __.``ifVariable - is less than variable - int32 - variable doesn't exist - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Variable "Other Var") ReturnError
            <| 6
            <| true
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``ifVariable - is less than variable - int32 - is more than - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4 :> obj) |]
            }
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Variable "Other Var") ReturnError
            <| 6
            <| true
            <| validationContext

    [<TestMethod>]
    member __.``ifVariable - is less than variable - int32 - is equal - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4 :> obj) |]
            }
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Variable "Other Var") ReturnError
            <| 4
            <| true
            <| validationContext

    [<TestMethod>]
    member __.``ifVariable - is less than variable - int32 - is less than - returns is valid false``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4 :> obj) |]
            }
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Variable "Other Var") ReturnError
            <| 3
            <| false
            <| validationContext
        
    [<TestMethod>]
    member __.``ifVariable - is less than variable - int64 - variable doesn't exist - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Variable "Other Var") ReturnError
            <| 6L
            <| true
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``ifVariable - is less than variable - int64 - is more than - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4L :> obj) |]
            }
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Variable "Other Var") ReturnError
            <| 6L
            <| true
            <| validationContext

    [<TestMethod>]
    member __.``ifVariable - is less than variable - int64 - is equal - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4L :> obj) |]
            }
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Variable "Other Var") ReturnError
            <| 4L
            <| true
            <| validationContext

    [<TestMethod>]
    member __.``ifVariable - is less than variable - int64 - is less than - returns is valid false``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4L :> obj) |]
            }
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Variable "Other Var") ReturnError
            <| 3L
            <| false
            <| validationContext

    [<TestMethod>]
    member __.``ifVariable - is less than variable - uint16 - variable doesn't exist - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Variable "Other Var") ReturnError
            <| 6us
            <| true
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``ifVariable - is less than variable - uint16 - is more than - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4us :> obj) |]
            }
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Variable "Other Var") ReturnError
            <| 6us
            <| true
            <| validationContext

    [<TestMethod>]
    member __.``ifVariable - is less than variable - uint16 - is equal - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4us :> obj) |]
            }
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Variable "Other Var") ReturnError
            <| 4us
            <| true
            <| validationContext

    [<TestMethod>]
    member __.``ifVariable - is less than variable - uint16 - is less than - returns is valid false``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4us :> obj) |]
            }
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Variable "Other Var") ReturnError
            <| 3us
            <| false
            <| validationContext

    [<TestMethod>]
    member __.``ifVariable - is less than variable - uint32 - variable doesn't exist - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Variable "Other Var") ReturnError
            <| 6u
            <| true
            <| createVariableValidationContext "Var"
        
    [<TestMethod>]
    member __.``ifVariable - is less than variable - uint32 - is more than - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4u :> obj) |]
            }
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Variable "Other Var") ReturnError
            <| 6u
            <| true
            <| validationContext

    [<TestMethod>]
    member __.``ifVariable - is less than variable - uint32 - is equal - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4u :> obj) |]
            }
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Variable "Other Var") ReturnError
            <| 4u
            <| true
            <| validationContext

    [<TestMethod>]
    member __.``ifVariable - is less than variable - uint32 - is less than - returns is valid false``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4u :> obj) |]
            }
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Variable "Other Var") ReturnError
            <| 3u
            <| false
            <| validationContext
        
    [<TestMethod>]
    member __.``ifVariable - is less than variable - uint64 - variable doesn't exist - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Variable "Other Var") ReturnError
            <| 6UL
            <| true
            <| createVariableValidationContext "Var"
                
    [<TestMethod>]
    member __.``ifVariable - is less than variable - uint64 - is more than - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4UL :> obj) |]
            }
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Variable "Other Var") ReturnError
            <| 6UL
            <| true
            <| validationContext

    [<TestMethod>]
    member __.``ifVariable - is less than variable - uint64 - is equal - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4UL :> obj) |]
            }
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Variable "Other Var") ReturnError
            <| 4UL
            <| true
            <| validationContext

    [<TestMethod>]
    member __.``ifVariable - is less than variable - uint64 - is less than - returns is valid false``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4UL :> obj) |]
            }
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Variable "Other Var") ReturnError
            <| 3UL
            <| false
            <| validationContext
        
    [<TestMethod>]
    member __.``ifVariable - is less than variable - byte - variable doesn't exist - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Variable "Other Var") ReturnError
            <| 6uy
            <| true
            <| createVariableValidationContext "Var"
                      
    [<TestMethod>]
    member __.``ifVariable - is less than variable - byte - is more than - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4uy :> obj) |]
            }
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Variable "Other Var") ReturnError
            <| 6uy
            <| true
            <| validationContext

    [<TestMethod>]
    member __.``ifVariable - is less than variable - byte - is equal - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4uy :> obj) |]
            }
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Variable "Other Var") ReturnError
            <| 4uy
            <| true
            <| validationContext

    [<TestMethod>]
    member __.``ifVariable - is less than variable - byte - is less than - returns is valid false``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4uy :> obj) |]
            }
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Variable "Other Var") ReturnError
            <| 3uy
            <| false
            <| validationContext

    [<TestMethod>]
    member __.``ifVariable - is less than variable - sbyte - variable doesn't exist - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Variable "Other Var") ReturnError
            <| 6y
            <| true
            <| createVariableValidationContext "Var"
              
    [<TestMethod>]
    member __.``ifVariable - is less than variable - sbyte - is more than - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4y :> obj) |]
            }
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Variable "Other Var") ReturnError
            <| 6y
            <| true
            <| validationContext

    [<TestMethod>]
    member __.``ifVariable - is less than variable - sbyte - is equal - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4y :> obj) |]
            }
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Variable "Other Var") ReturnError
            <| 4y
            <| true
            <| validationContext

    [<TestMethod>]
    member __.``ifVariable - is less than variable - sbyte - is less than - returns is valid false``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4y :> obj) |]
            }
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Variable "Other Var") ReturnError
            <| 3y
            <| false
            <| validationContext

    [<TestMethod>]
    member __.``ifVariable - is less than variable - float - variable doesn't exist - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Variable "Other Var") ReturnError
            <| 4.0
            <| true
            <| createVariableValidationContext "Var"
            
    [<TestMethod>]
    member __.``ifVariable - is less than variable - float - is more than - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4.0 :> obj) |]
            }
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Variable "Other Var") ReturnError
            <| 6.0
            <| true
            <| validationContext

    [<TestMethod>]
    member __.``ifVariable - is less than variable - float - is equal - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4.0 :> obj) |]
            }
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Variable "Other Var") ReturnError
            <| 4.0
            <| true
            <| validationContext

    [<TestMethod>]
    member __.``ifVariable - is less than variable - float - is less than - returns is valid false``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4.0 :> obj) |]
            }
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Variable "Other Var") ReturnError
            <| 3.0
            <| false
            <| validationContext

    [<TestMethod>]
    member __.``ifVariable - is less than variable - float32 - variable doesn't exist - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Variable "Other Var") ReturnError
            <| 4.0f
            <| true
            <| createVariableValidationContext "Var"
              
    [<TestMethod>]
    member __.``ifVariable - is less than variable - float32 - is more than - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4.0f :> obj) |]
            }
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Variable "Other Var") ReturnError
            <| 6.0f
            <| true
            <| validationContext

    [<TestMethod>]
    member __.``ifVariable - is less than variable - float32 - is equal - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4.0f :> obj) |]
            }
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Variable "Other Var") ReturnError
            <| 4.0f
            <| true
            <| validationContext

    [<TestMethod>]
    member __.``ifVariable - is less than variable - float32 - is less than - returns is valid false``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4.0f :> obj) |]
            }
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Variable "Other Var") ReturnError
            <| 3.0f
            <| false
            <| validationContext
        
    [<TestMethod>]
    member __.``ifVariable - is less than variable - decimal - variable doesn't exist - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Variable "Other Var") ReturnError
            <| 4.0m
            <| true
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``ifVariable - is less than variable - decimal - is more than - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4.0m :> obj) |]
            }
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Variable "Other Var") ReturnError
            <| 6.0m
            <| true
            <| validationContext

    [<TestMethod>]
    member __.``ifVariable - is less than variable - decimal - is equal - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4.0m :> obj) |]
            }
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Variable "Other Var") ReturnError
            <| 4.0m
            <| true
            <| validationContext

    [<TestMethod>]
    member __.``ifVariable - is less than variable - decimal - is less than - returns is valid false``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4.0m :> obj) |]
            }
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Variable "Other Var") ReturnError
            <| 3.0m
            <| false
            <| validationContext
              
    [<TestMethod>]
    member __.``ifVariable - is less than variable - datetime - variable doesn't exist - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Variable "Other Var") ReturnError
            <| new DateTime(2018, 1, 29)
            <| true
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``ifVariable - is less than variable - datetime - is more than - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", new DateTime(2018, 1, 29) :> obj) |]
            }
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Variable "Other Var") ReturnError
            <| new DateTime(2018, 2, 20)
            <| true
            <| validationContext

    [<TestMethod>]
    member __.``ifVariable - is less than variable - datetime - is equal - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", new DateTime(2018, 1, 29) :> obj) |]
            }
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Variable "Other Var") ReturnError
            <| new DateTime(2018, 1, 29)
            <| true
            <| validationContext

    [<TestMethod>]
    member __.``ifVariable - is less than variable - datetime - is less than - returns is valid false``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", new DateTime(2018, 1, 29) :> obj) |]
            }
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Variable "Other Var") ReturnError
            <| new DateTime(2018, 1, 20)
            <| false
            <| validationContext
            
    [<TestMethod>]
    member __.``ifVariable - is more than value - formats error message and target value as error``() = 
        assertVariableRuleErrorMessage 
            <| ifVariable "Var" (IsMoreThan <| Value 4) ReturnError
            <| 5
            <| "should not be more than 4 : Int32"
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``ifVariable - is more than variable - formats error message and target namd and value as error``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4 :> obj) |]
            }
        assertVariableRuleErrorMessage 
            <| ifVariable "Var" (IsMoreThan <| Variable "Other Var") ReturnError
            <| 5
            <| "should not be more than variable 'Other Var', 4 : Int32"
            <| validationContext

    [<TestMethod>]
    member __.``ifVariable - is more than value - int16 - is more than - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsMoreThan <| Value 4s) ReturnError
            <| 6s
            <| false
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``ifVariable - is more than value - int16 - is equal - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsMoreThan <| Value 4s) ReturnError
            <| 4s
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``ifVariable - is more than value - int16 - is less than - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsMoreThan <| Value 4s) ReturnError
            <| 3s
            <| true
            <| createVariableValidationContext "Var"
            
    [<TestMethod>]
    member __.``ifVariable - is more than value - int32 - is more than - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsMoreThan <| Value 4) ReturnError
            <| 6
            <| false
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``ifVariable - is more than value - int32 - is equal - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsMoreThan <| Value 4) ReturnError
            <| 4
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``ifVariable - is more than value - int32 - is less than - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsMoreThan <| Value 4) ReturnError
            <| 3
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``ifVariable - is more than value - int64 - is more than - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsMoreThan <| Value 4L) ReturnError
            <| 6L
            <| false
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``ifVariable - is more than value - int64 - is equal - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsMoreThan <| Value 4L) ReturnError
            <| 4L
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``ifVariable - is more than value - int64 - is less than - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsMoreThan <| Value 4L) ReturnError
            <| 3L
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``ifVariable - is more than value - uint16 - is more than - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsMoreThan <| Value 4us) ReturnError
            <| 6us
            <| false
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``ifVariable - is more than value - uint16 - is equal - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsMoreThan <| Value 4us) ReturnError
            <| 4us
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``ifVariable - is more than value - uint16 - is less than - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsMoreThan <| Value 4us) ReturnError
            <| 3us
            <| true
            <| createVariableValidationContext "Var"
            
    [<TestMethod>]
    member __.``ifVariable - is more than value - uint32 - is more than - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsMoreThan <| Value 4u) ReturnError
            <| 6u
            <| false
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``ifVariable - is more than value - uint32 - is equal - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsMoreThan <| Value 4u) ReturnError
            <| 4u
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``ifVariable - is more than value - uint32 - is less than - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsMoreThan <| Value 4u) ReturnError
            <| 3u
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``ifVariable - is more than value - uint64 - is more than - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsMoreThan <| Value 4UL) ReturnError
            <| 6UL
            <| false
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``ifVariable - is more than value - uint64 - is equal - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsMoreThan <| Value 4UL) ReturnError
            <| 4UL
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``ifVariable - is more than value - uint64 - is less than - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsMoreThan <| Value 4UL) ReturnError
            <| 3UL
            <| true
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``ifVariable - is more than value - byte - is more than - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsMoreThan <| Value 4uy) ReturnError
            <| 6uy
            <| false
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``ifVariable - is more than value - byte - is equal - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsMoreThan <| Value 4uy) ReturnError
            <| 4uy
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``ifVariable - is more than value - byte - is less than - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsMoreThan <| Value 4uy) ReturnError
            <| 3uy
            <| true
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``ifVariable - is more than value - sbyte - is more than - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsMoreThan <| Value 4y) ReturnError
            <| 6y
            <| false
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``ifVariable - is more than value - sbyte - is equal - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsMoreThan <| Value 4y) ReturnError
            <| 4y
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``ifVariable - is more than value - sbyte - is less than - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsMoreThan <| Value 4y) ReturnError
            <| 3y
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``ifVariable - is more than value - float - is more than - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsMoreThan <| Value 4.0) ReturnError
            <| 6.0
            <| false
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``ifVariable - is more than value - float - is equal - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsMoreThan <| Value 4.0) ReturnError
            <| 4.0
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``ifVariable - is more than value - float - is less than - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsMoreThan <| Value 4.0) ReturnError
            <| 3.0
            <| true
            <| createVariableValidationContext "Var"
            
    [<TestMethod>]
    member __.``ifVariable - is more than value - float32 - is more than - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsMoreThan <| Value 4.0f) ReturnError
            <| 6.0f
            <| false
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``ifVariable - is more than value - float32 - is equal - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsMoreThan <| Value 4.0f) ReturnError
            <| 4.0f
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``ifVariable - is more than value - float32 - is less than - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsMoreThan <| Value 4.0f) ReturnError
            <| 3.0f
            <| true
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``ifVariable - is more than value - decimal - is more than - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsMoreThan <| Value 4.0m) ReturnError
            <| 6.0m
            <| false
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``ifVariable - is more than value - decimal - is equal - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsMoreThan <| Value 4.0m) ReturnError
            <| 4.0m
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``ifVariable - is more than value - decimal - is less than - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsMoreThan <| Value 4.0m) ReturnError
            <| 3.0m
            <| true
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``ifVariable - is more than value - datetime - is more than - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsMoreThan <| Value (new DateTime(2018, 1, 29))) ReturnError
            <| new DateTime(2018, 2, 20)
            <| false
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``ifVariable - is more than value - datetime - is equal - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsMoreThan <| Value (new DateTime(2018, 1, 29))) ReturnError
            <| new DateTime(2018, 1, 29)
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``ifVariable - is more than value - datetime - is less than - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsMoreThan <| Value (new DateTime(2018, 1, 29))) ReturnError
            <| new DateTime(2018, 1, 20)
            <| true
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``ifVariable - is more than variable - int16 - variable doesn't exist - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsMoreThan <| Variable "Other Var") ReturnError
            <| 6s
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``ifVariable - is more than variable - int16 - is more than - returns is valid false``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4s :> obj) |]
            }
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsMoreThan <| Variable "Other Var") ReturnError
            <| 6s
            <| false
            <| validationContext 
    
    [<TestMethod>]
    member __.``ifVariable - is more than variable - int16 - is equal - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4s :> obj) |]
            }
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsMoreThan <| Variable "Other Var") ReturnError
            <| 4s
            <| true
            <| validationContext
     
    [<TestMethod>]
    member __.``ifVariable - is more than variable - int16 - is less than - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4s :> obj) |]
            }
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsMoreThan <| Variable "Other Var") ReturnError
            <| 3s
            <| true
            <| validationContext

    [<TestMethod>]
    member __.``ifVariable - is more than variable - int32 - variable doesn't exist - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsMoreThan <| Variable "Other Var") ReturnError
            <| 6
            <| true
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``ifVariable - is more than variable - int32 - is more than - returns is valid false``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4 :> obj) |]
            }
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsMoreThan <| Variable "Other Var") ReturnError
            <| 6
            <| false
            <| validationContext

    [<TestMethod>]
    member __.``ifVariable - is more than variable - int32 - is equal - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4 :> obj) |]
            }
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsMoreThan <| Variable "Other Var") ReturnError
            <| 4
            <| true
            <| validationContext

    [<TestMethod>]
    member __.``ifVariable - is more than variable - int32 - is less than - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4 :> obj) |]
            }
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsMoreThan <| Variable "Other Var") ReturnError
            <| 3
            <| true
            <| validationContext
        
    [<TestMethod>]
    member __.``ifVariable - is more than variable - int64 - variable doesn't exist - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsMoreThan <| Variable "Other Var") ReturnError
            <| 6L
            <| true
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``ifVariable - is more than variable - int64 - is more than - returns is valid false``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4L :> obj) |]
            }
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsMoreThan <| Variable "Other Var") ReturnError
            <| 6L
            <| false
            <| validationContext

    [<TestMethod>]
    member __.``ifVariable - is more than variable - int64 - is equal - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4L :> obj) |]
            }
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsMoreThan <| Variable "Other Var") ReturnError
            <| 4L
            <| true
            <| validationContext

    [<TestMethod>]
    member __.``ifVariable - is more than variable - int64 - is less than - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4L :> obj) |]
            }
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsMoreThan <| Variable "Other Var") ReturnError
            <| 3L
            <| true
            <| validationContext

    [<TestMethod>]
    member __.``ifVariable - is more than variable - uint16 - variable doesn't exist - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsMoreThan <| Variable "Other Var") ReturnError
            <| 6us
            <| true
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``ifVariable - is more than variable - uint16 - is more than - returns is valid false``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4us :> obj) |]
            }
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsMoreThan <| Variable "Other Var") ReturnError
            <| 6us
            <| false
            <| validationContext

    [<TestMethod>]
    member __.``ifVariable - is more than variable - uint16 - is equal - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4us :> obj) |]
            }
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsMoreThan <| Variable "Other Var") ReturnError
            <| 4us
            <| true
            <| validationContext

    [<TestMethod>]
    member __.``ifVariable - is more than variable - uint16 - is less than - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4us :> obj) |]
            }
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsMoreThan <| Variable "Other Var") ReturnError
            <| 3us
            <| true
            <| validationContext

    [<TestMethod>]
    member __.``ifVariable - is more than variable - uint32 - variable doesn't exist - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsMoreThan <| Variable "Other Var") ReturnError
            <| 6u
            <| true
            <| createVariableValidationContext "Var"
        
    [<TestMethod>]
    member __.``ifVariable - is more than variable - uint32 - is more than - returns is valid false``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4u :> obj) |]
            }
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsMoreThan <| Variable "Other Var") ReturnError
            <| 6u
            <| false
            <| validationContext

    [<TestMethod>]
    member __.``ifVariable - is more than variable - uint32 - is equal - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4u :> obj) |]
            }
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsMoreThan <| Variable "Other Var") ReturnError
            <| 4u
            <| true
            <| validationContext

    [<TestMethod>]
    member __.``ifVariable - is more than variable - uint32 - is less than - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4u :> obj) |]
            }
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsMoreThan <| Variable "Other Var") ReturnError
            <| 3u
            <| true
            <| validationContext
        
    [<TestMethod>]
    member __.``ifVariable - is more than variable - uint64 - variable doesn't exist - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsMoreThan <| Variable "Other Var") ReturnError
            <| 6UL
            <| true
            <| createVariableValidationContext "Var"
                
    [<TestMethod>]
    member __.``ifVariable - is more than variable - uint64 - is more than - returns is valid false``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4UL :> obj) |]
            }
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsMoreThan <| Variable "Other Var") ReturnError
            <| 6UL
            <| false
            <| validationContext

    [<TestMethod>]
    member __.``ifVariable - is more than variable - uint64 - is equal - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4UL :> obj) |]
            }
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsMoreThan <| Variable "Other Var") ReturnError
            <| 4UL
            <| true
            <| validationContext

    [<TestMethod>]
    member __.``ifVariable - is more than variable - uint64 - is less than - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4UL :> obj) |]
            }
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsMoreThan <| Variable "Other Var") ReturnError
            <| 3UL
            <| true
            <| validationContext
        
    [<TestMethod>]
    member __.``ifVariable - is more than variable - byte - variable doesn't exist - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsMoreThan <| Variable "Other Var") ReturnError
            <| 6uy
            <| true
            <| createVariableValidationContext "Var"
                      
    [<TestMethod>]
    member __.``ifVariable - is more than variable - byte - is more than - returns is valid false``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4uy :> obj) |]
            }
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsMoreThan <| Variable "Other Var") ReturnError
            <| 6uy
            <| false
            <| validationContext

    [<TestMethod>]
    member __.``ifVariable - is more than variable - byte - is equal - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4uy :> obj) |]
            }
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsMoreThan <| Variable "Other Var") ReturnError
            <| 4uy
            <| true
            <| validationContext

    [<TestMethod>]
    member __.``ifVariable - is more than variable - byte - is less than - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4uy :> obj) |]
            }
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsMoreThan <| Variable "Other Var") ReturnError
            <| 3uy
            <| true
            <| validationContext

    [<TestMethod>]
    member __.``ifVariable - is more than variable - sbyte - variable doesn't exist - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsMoreThan <| Variable "Other Var") ReturnError
            <| 6y
            <| true
            <| createVariableValidationContext "Var"
              
    [<TestMethod>]
    member __.``ifVariable - is more than variable - sbyte - is more than - returns is valid false``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4y :> obj) |]
            }
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsMoreThan <| Variable "Other Var") ReturnError
            <| 6y
            <| false
            <| validationContext

    [<TestMethod>]
    member __.``ifVariable - is more than variable - sbyte - is equal - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4y :> obj) |]
            }
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsMoreThan <| Variable "Other Var") ReturnError
            <| 4y
            <| true
            <| validationContext

    [<TestMethod>]
    member __.``ifVariable - is more than variable - sbyte - is less than - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4y :> obj) |]
            }
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsMoreThan <| Variable "Other Var") ReturnError
            <| 3y
            <| true
            <| validationContext

    [<TestMethod>]
    member __.``ifVariable - is more than variable - float - variable doesn't exist - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsMoreThan <| Variable "Other Var") ReturnError
            <| 4.0
            <| true
            <| createVariableValidationContext "Var"
            
    [<TestMethod>]
    member __.``ifVariable - is more than variable - float - is more than - returns is valid false``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4.0 :> obj) |]
            }
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsMoreThan <| Variable "Other Var") ReturnError
            <| 6.0
            <| false
            <| validationContext

    [<TestMethod>]
    member __.``ifVariable - is more than variable - float - is equal - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4.0 :> obj) |]
            }
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsMoreThan <| Variable "Other Var") ReturnError
            <| 4.0
            <| true
            <| validationContext

    [<TestMethod>]
    member __.``ifVariable - is more than variable - float - is less than - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4.0 :> obj) |]
            }
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsMoreThan <| Variable "Other Var") ReturnError
            <| 3.0
            <| true
            <| validationContext

    [<TestMethod>]
    member __.``ifVariable - is more than variable - float32 - variable doesn't exist - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsMoreThan <| Variable "Other Var") ReturnError
            <| 4.0f
            <| true
            <| createVariableValidationContext "Var"
              
    [<TestMethod>]
    member __.``ifVariable - is more than variable - float32 - is more than - returns is valid false``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4.0f :> obj) |]
            }
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsMoreThan <| Variable "Other Var") ReturnError
            <| 6.0f
            <| false
            <| validationContext

    [<TestMethod>]
    member __.``ifVariable - is more than variable - float32 - is equal - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4.0f :> obj) |]
            }
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsMoreThan <| Variable "Other Var") ReturnError
            <| 4.0f
            <| true
            <| validationContext

    [<TestMethod>]
    member __.``ifVariable - is more than variable - float32 - is less than - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4.0f :> obj) |]
            }
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsMoreThan <| Variable "Other Var") ReturnError
            <| 3.0f
            <| true
            <| validationContext
        
    [<TestMethod>]
    member __.``ifVariable - is more than variable - decimal - variable doesn't exist - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsMoreThan <| Variable "Other Var") ReturnError
            <| 4.0m
            <| true
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``ifVariable - is more than variable - decimal - is more than - returns is valid false``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4.0m :> obj) |]
            }
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsMoreThan <| Variable "Other Var") ReturnError
            <| 6.0m
            <| false
            <| validationContext

    [<TestMethod>]
    member __.``ifVariable - is more than variable - decimal - is equal - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4.0m :> obj) |]
            }
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsMoreThan <| Variable "Other Var") ReturnError
            <| 4.0m
            <| true
            <| validationContext

    [<TestMethod>]
    member __.``ifVariable - is more than variable - decimal - is less than - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4.0m :> obj) |]
            }
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsMoreThan <| Variable "Other Var") ReturnError
            <| 3.0m
            <| true
            <| validationContext
                       
    [<TestMethod>]
    member __.``ifVariable - is more than variable - datetime - variable doesn't exist - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsMoreThan <| Variable "Other Var") ReturnError
            <| new DateTime(2018, 1, 29)
            <| true
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``ifVariable - is more than variable - datetime - is more than - returns is valid false``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", new DateTime(2018, 1, 29) :> obj) |]
            }
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsMoreThan <| Variable "Other Var") ReturnError
            <| new DateTime(2018, 2, 20)
            <| false
            <| validationContext

    [<TestMethod>]
    member __.``ifVariable - is more than variable - datetime - is equal - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", new DateTime(2018, 1, 29) :> obj) |]
            }
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsMoreThan <| Variable "Other Var") ReturnError
            <| new DateTime(2018, 1, 29)
            <| true
            <| validationContext

    [<TestMethod>]
    member __.``ifVariable - is more than variable - datetime - is less than - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", new DateTime(2018, 1, 29) :> obj) |]
            }
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsMoreThan <| Variable "Other Var") ReturnError
            <| new DateTime(2018, 1, 20)
            <| true
            <| validationContext  
    
    [<TestMethod>]
    member __.``ifVariable - and rule - formats left validation message and right validation message as error``() = 
        assertVariableRuleErrorMessage 
            <| ifVariable "Var" ((IsLessThan <| Value 10) &&& IsNegative) ReturnError
            <| -10
            <| "(should not be less than 10 : Int32 AND should not be negative)"
            <| createVariableValidationContext "Var"

    [<TestMethod>]
       member __.``ifVariable - and rule - left operand is valid, right operand is valid - returns is valid true``() = 
           assertVariableRuleIsValid 
               <| ifVariable "Var" (IsZero &&& IsNegative) ReturnError
               <| 6
               <| true
               <| createVariableValidationContext "Var"
    
    [<TestMethod>]
       member __.``ifVariable - and rule - left operand is invalid, right operand is valid - returns is valid true``() = 
           assertVariableRuleIsValid 
               <| ifVariable "Var" (IsZero &&& IsNegative) ReturnError
               <| 0
               <| true
               <| createVariableValidationContext "Var"
    
    [<TestMethod>]
       member __.``ifVariable - and rule - left operand is valid, right operand is invalid - returns is valid true``() = 
           assertVariableRuleIsValid 
               <| ifVariable "Var" (IsZero &&& IsNegative) ReturnError
               <| -10
               <| true
               <| createVariableValidationContext "Var"
    
    [<TestMethod>]
       member __.``ifVariable - and rule - left operand is invalid, right operand is invalid - returns is valid false``() = 
           assertVariableRuleIsValid 
               <| ifVariable "Var" ((IsLessThan <| Value 10) &&& IsNegative) ReturnError
               <| -10
               <| false
               <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``ifVariable - or rule - left and right operands are invalid - formats left validation message and right validation message as error``() = 
        assertVariableRuleErrorMessage 
            <| ifVariable "Var" ((IsLessThan <| Value 10) ||| IsNegative) ReturnError
            <| -10
            <| "(should not be less than 10 : Int32 AND should not be negative)"
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``ifVariable - or rule - only left operand is invalid - formats left validation message as error``() = 
        assertVariableRuleErrorMessage 
            <| ifVariable "Var" (IsZero ||| IsNegative) ReturnError
            <| 0
            <| "should not be zero"
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``ifVariable - or rule - only right operand is invalid - formats right validation message as error``() = 
        assertVariableRuleErrorMessage 
            <| ifVariable "Var" (IsZero ||| IsNegative) ReturnError
            <| -10
            <| "should not be negative"
            <| createVariableValidationContext "Var"

    [<TestMethod>]
       member __.``ifVariable - or rule - left operand is valid, right operand is valid - returns is valid true``() = 
           assertVariableRuleIsValid 
               <| ifVariable "Var" (IsZero ||| IsNegative) ReturnError
               <| 6
               <| true
               <| createVariableValidationContext "Var"
    
    [<TestMethod>]
       member __.``ifVariable - or rule - left operand is invalid, right operand is valid - returns is valid false``() = 
           assertVariableRuleIsValid 
               <| ifVariable "Var" (IsZero ||| IsNegative) ReturnError
               <| 0
               <| false
               <| createVariableValidationContext "Var"
    
    [<TestMethod>]
       member __.``ifVariable - or rule - left operand is valid, right operand is invalid - returns is valid false``() = 
           assertVariableRuleIsValid 
               <| ifVariable "Var" (IsZero ||| IsNegative) ReturnError
               <| -10
               <| false
               <| createVariableValidationContext "Var"
    
    [<TestMethod>]
       member __.``ifVariable - or rule - left operand is invalid, right operand is invalid - returns is valid false``() = 
           assertVariableRuleIsValid 
               <| ifVariable "Var" ((IsLessThan <| Value 10) ||| IsNegative) ReturnError
               <| -10
               <| false
               <| createVariableValidationContext "Var"