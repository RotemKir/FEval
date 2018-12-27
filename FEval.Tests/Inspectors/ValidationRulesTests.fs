namespace FEval.Tests

open Microsoft.FSharp.Quotations
open Microsoft.VisualStudio.TestTools.UnitTesting
open FEval.Inspectors.ValidationsCommon
open FEval.Inspectors.ValidationRules
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

    let assertVariableRuleErrorMessage validation value expectedErrorMessage validationContext =
        let validationRequest = { Value = value ; ValidationContext = validationContext }
        let actualErrorMessage = validation.FormatMessage validationRequest
        Assert.AreEqual(expectedErrorMessage, actualErrorMessage)

    let assertVariableRuleIsValid validation value isValid validationContext =
        let validationRequest = { Value = value ; ValidationContext = validationContext }
        Assert.AreEqual(isValid, validation.IsValid validationRequest)

    [<TestMethod>]
    member __.``getVariableValidation - is zero - formats error message as error``() = 
        assertVariableRuleErrorMessage 
            <| getVariableValidation IsZero
            <| 0
            <| "should not be zero"
            <| createVariableValidationContext "Var"
                
    [<TestMethod>]
    member __.``getVariableValidation - is zero - int16 - is zero - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation IsZero
            <| 0s
            <| false
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``getVariableValidation - is zero - int16 - is not zero - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation IsZero
            <| 3s
            <| true
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``getVariableValidation - is zero - int32 - is zero - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation IsZero
            <| 0
            <| false
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``getVariableValidation - is zero - int32 - is not zero - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation IsZero
            <| 3
            <| true
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``getVariableValidation - is zero - int64 - is zero - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation IsZero
            <| 0L
            <| false
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``getVariableValidation - is zero - int64 - is not zero - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation IsZero
            <| 3L
            <| true
            <| createVariableValidationContext "Var"
                        
    [<TestMethod>]
    member __.``getVariableValidation - is zero - uint16 - is zero - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation IsZero
            <| 0us
            <| false
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``getVariableValidation - is zero - uint16 - is not zero - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation IsZero
            <| 3us
            <| true
            <| createVariableValidationContext "Var"
                       
    [<TestMethod>]
    member __.``getVariableValidation - is zero - uint32 - is zero - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation IsZero
            <| 0u
            <| false
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``getVariableValidation - is zero - uint32 - is not zero - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation IsZero
            <| 3u
            <| true
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``getVariableValidation - is zero - uint64 - is zero - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation IsZero
            <| 0UL
            <| false
            <| createVariableValidationContext "Var"
                       
    [<TestMethod>]
    member __.``getVariableValidation - is zero - uint64 - is not zero - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation IsZero
            <| 3UL
            <| true
            <| createVariableValidationContext "Var"
                
    [<TestMethod>]
    member __.``getVariableValidation - is zero - byte - is zero - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation IsZero
            <| 0uy
            <| false
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``getVariableValidation - is zero - byte - is not zero - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation IsZero
            <| 3uy
            <| true
            <| createVariableValidationContext "Var"
            
    [<TestMethod>]
    member __.``getVariableValidation - is zero - sbyte - is zero - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation IsZero
            <| 0y
            <| false
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``getVariableValidation - is zero - sbyte - is not zero - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation IsZero
            <| 3y
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``getVariableValidation - is zero - float - is zero - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation IsZero
            <| 0.0
            <| false
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``getVariableValidation - is zero - float - is not zero - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation IsZero
            <| 3.0
            <| true
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``getVariableValidation - is zero - float32 - is zero - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation IsZero
            <| 0.0f
            <| false
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``getVariableValidation - is zero - float32 - is not zero - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation IsZero
            <| 3.0f
            <| true
            <| createVariableValidationContext "Var"
                        
    [<TestMethod>]
    member __.``getVariableValidation - is zero - decimal - is zero - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation IsZero
            <| 0.0m
            <| false
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``getVariableValidation - is zero - decimal - is not zero - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation IsZero
            <| 3.0m
            <| true
            <| createVariableValidationContext "Var"
            
    [<TestMethod>]
    member __.``getVariableValidation - is zero - string - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation IsZero
            <| "Hello"
            <| true
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``getVariableValidation - is negative - formats error message as error``() = 
        assertVariableRuleErrorMessage 
            <| getVariableValidation IsNegative
            <| -8
            <| "should not be negative"
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``getVariableValidation - is negative - int16 - is negative - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation IsNegative
            <| -8s
            <| false
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``getVariableValidation - is negative - int16 - is not negative - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation IsNegative
            <| 3s
            <| true
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``getVariableValidation - is negative - int32 - is negative - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation IsNegative
            <| -8
            <| false
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``getVariableValidation - is negative - int32 - is not negative - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation IsNegative
            <| 3
            <| true
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``getVariableValidation - is negative - int64 - is negative - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation IsNegative
            <| -8L
            <| false
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``getVariableValidation - is negative - int64 - is not negative - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation IsNegative
            <| 3L
            <| true
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``getVariableValidation - is negative - uint16 - is not negative - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation IsNegative
            <| 3us
            <| true
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``getVariableValidation - is negative - uint32 - is not negative - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation IsNegative
            <| 3u
            <| true
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``getVariableValidation - is negative - uint64 - is not negative - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation IsNegative
            <| 3UL
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``getVariableValidation - is negative - byte - is not negative - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation IsNegative
            <| 3uy
            <| true
            <| createVariableValidationContext "Var"
            
    [<TestMethod>]
    member __.``getVariableValidation - is negative - sbyte - is negative - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation IsNegative
            <| -8y
            <| false
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``getVariableValidation - is negative - sbyte - is not negative - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation IsNegative
            <| 3y
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``getVariableValidation - is negative - float - is negative - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation IsNegative
            <| -8.0
            <| false
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``getVariableValidation - is negative - float - is not negative - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation IsNegative
            <| 3.0
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``getVariableValidation - is negative - float32 - is negative - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation IsNegative
            <| -8.0f
            <| false
            <| createVariableValidationContext "Var"
            
    [<TestMethod>]
    member __.``getVariableValidation - is negative - float32 - is not negative - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation IsNegative
            <| 3.0f
            <| true
            <| createVariableValidationContext "Var"
            
    [<TestMethod>]
    member __.``getVariableValidation - is negative - decimal - is negative - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation IsNegative
            <| -8.0m
            <| false
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``getVariableValidation - is negative - decimal - is not negative - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation IsNegative
            <| 3.0m
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``getVariableValidation - is negative - string - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation IsNegative
            <| "Hello"
            <| true
            <| createVariableValidationContext "Var"
            
    [<TestMethod>]
    member __.``getVariableValidation - is empty - formats error message``() = 
        assertVariableRuleErrorMessage 
            <| getVariableValidation IsEmpty
            <| [||]
            <| "should not be empty"
            <| createVariableValidationContext "Var"
                
    [<TestMethod>]
    member __.``getVariableValidation - is empty - string - is empty - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation IsEmpty
            <| ""
            <| false
            <| createVariableValidationContext "Var"
                  
    [<TestMethod>]
    member __.``getVariableValidation - is empty - string - is not empty - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation IsEmpty
            <| "Hello"
            <| true
            <| createVariableValidationContext "Var"
                
    [<TestMethod>]
    member __.``getVariableValidation - is empty - int - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation IsEmpty
            <| 8
            <| true
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``getVariableValidation - is empty - seq - is empty - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation IsEmpty
            <| Seq.ofArray [||]
            <| false
            <| createVariableValidationContext "Var"
        
    [<TestMethod>]
    member __.``getVariableValidation - is empty - seq - is not empty - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation IsEmpty
            <| seq {1 .. 10}
            <| true
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``getVariableValidation - is empty - array - is empty - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation IsEmpty
            <| [||]
            <| false
            <| createVariableValidationContext "Var"
        
    [<TestMethod>]
    member __.``getVariableValidation - is empty - array - is not empty - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation IsEmpty
            <| [| 1 ; 2 ; 3 |]
            <| true
            <| createVariableValidationContext "Var"
     
    [<TestMethod>]
    member __.``getVariableValidation - is empty - list - is empty - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation IsEmpty
            <| []
            <| false
            <| createVariableValidationContext "Var"
                
    [<TestMethod>]
    member __.``getVariableValidation - is empty - list - is not empty - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation IsEmpty
            <| [ 1 ; 2 ; 3 ]
            <| true
            <| createVariableValidationContext "Var"
            
    [<TestMethod>]
    member __.``getVariableValidation  - is less than value - formats error message and target value as error``() = 
        assertVariableRuleErrorMessage 
            <| getVariableValidation (IsLessThan <| Value 4)
            <| 3
            <| "should not be less than 4 : Int32"
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``getVariableValidation - is less than variable - formats error message and target namd and value as error``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4 :> obj) |]
            }
        assertVariableRuleErrorMessage 
            <| getVariableValidation (IsLessThan <| Variable "Other Var")
            <| 3
            <| "should not be less than variable 'Other Var', 4 : Int32"
            <| validationContext

    [<TestMethod>]
    member __.``getVariableValidation - is less than variable that doesn't exist - formats error message as error``() = 
        assertVariableRuleErrorMessage 
            <| getVariableValidation (IsLessThan <| Variable "Other Var")
            <| 3
            <| "should not be less than (null)"
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``getVariableValidation - is less than value - int16 - is more than - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsLessThan <| Value 4s)
            <| 6s
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``getVariableValidation - is less than value - int16 - is equal - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsLessThan <| Value 4s)
            <| 4s
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``getVariableValidation - is less than value - int16 - is less than - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsLessThan <| Value 4s)
            <| 3s
            <| false
            <| createVariableValidationContext "Var"
            
    [<TestMethod>]
    member __.``getVariableValidation - is less than value - int32 - is more than - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsLessThan <| Value 4)
            <| 6
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``getVariableValidation - is less than value - int32 - is equal - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsLessThan <| Value 4)
            <| 4
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``getVariableValidation - is less than value - int32 - is less than - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsLessThan <| Value 4)
            <| 3
            <| false
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``getVariableValidation - is less than value - int64 - is more than - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsLessThan <| Value 4L)
            <| 6L
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``getVariableValidation - is less than value - int64 - is equal - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsLessThan <| Value 4L)
            <| 4L
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``getVariableValidation - is less than value - int64 - is less than - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsLessThan <| Value 4L)
            <| 3L
            <| false
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``getVariableValidation - is less than value - uint16 - is more than - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsLessThan <| Value 4us)
            <| 6us
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``getVariableValidation - is less than value - uint16 - is equal - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsLessThan <| Value 4us)
            <| 4us
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``getVariableValidation - is less than value - uint16 - is less than - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsLessThan <| Value 4us)
            <| 3us
            <| false
            <| createVariableValidationContext "Var"
            
    [<TestMethod>]
    member __.``getVariableValidation - is less than value - uint32 - is more than - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsLessThan <| Value 4u)
            <| 6u
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``getVariableValidation - is less than value - uint32 - is equal - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsLessThan <| Value 4u)
            <| 4u
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``getVariableValidation - is less than value - uint32 - is less than - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsLessThan <| Value 4u)
            <| 3u
            <| false
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``getVariableValidation - is less than value - uint64 - is more than - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsLessThan <| Value 4UL)
            <| 6UL
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``getVariableValidation - is less than value - uint64 - is equal - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsLessThan <| Value 4UL)
            <| 4UL
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``getVariableValidation - is less than value - uint64 - is less than - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsLessThan <| Value 4UL)
            <| 3UL
            <| false
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``getVariableValidation - is less than value - byte - is more than - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsLessThan <| Value 4uy)
            <| 6uy
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``getVariableValidation - is less than value - byte - is equal - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsLessThan <| Value 4uy)
            <| 4uy
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``getVariableValidation - is less than value - byte - is less than - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsLessThan <| Value 4uy)
            <| 3uy
            <| false
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``getVariableValidation - is less than value - sbyte - is more than - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsLessThan <| Value 4y)
            <| 6y
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``getVariableValidation - is less than value - sbyte - is equal - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsLessThan <| Value 4y)
            <| 4y
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``getVariableValidation - is less than value - sbyte - is less than - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsLessThan <| Value 4y)
            <| 3y
            <| false
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``getVariableValidation - is less than value - float - is more than - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsLessThan <| Value 4.0)
            <| 6.0
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``getVariableValidation - is less than value - float - is equal - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsLessThan <| Value 4.0)
            <| 4.0
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``getVariableValidation - is less than value - float - is less than - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsLessThan <| Value 4.0)
            <| 3.0
            <| false
            <| createVariableValidationContext "Var"
            
    [<TestMethod>]
    member __.``getVariableValidation - is less than value - float32 - is more than - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsLessThan <| Value 4.0f)
            <| 6.0f
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``getVariableValidation - is less than value - float32 - is equal - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsLessThan <| Value 4.0f)
            <| 4.0f
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``getVariableValidation - is less than value - float32 - is less than - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsLessThan <| Value 4.0f)
            <| 3.0f
            <| false
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``getVariableValidation - is less than value - decimal - is more than - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsLessThan <| Value 4.0m)
            <| 6.0m
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``getVariableValidation - is less than value - decimal - is equal - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsLessThan <| Value 4.0m)
            <| 4.0m
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``getVariableValidation - is less than value - decimal - is less than - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsLessThan <| Value 4.0m)
            <| 3.0m
            <| false
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``getVariableValidation - is less than value - datetime - is more than - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsLessThan <| Value (new DateTime(2018, 1, 29)))
            <| new DateTime(2018, 2, 20)
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``getVariableValidation - is less than value - datetime - is equal - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsLessThan <| Value (new DateTime(2018, 1, 29)))
            <| new DateTime(2018, 1, 29)
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``getVariableValidation - is less than value - datetime - is less than - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsLessThan <| Value (new DateTime(2018, 1, 29)))
            <| new DateTime(2018, 1, 20)
            <| false
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``getVariableValidation - is less than variable - int16 - variable doesn't exist - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsLessThan <| Variable "Other Var")
            <| 6s
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``getVariableValidation - is less than variable - int16 - is more than - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4s :> obj) |]
            }
        assertVariableRuleIsValid 
            <| getVariableValidation (IsLessThan <| Variable "Other Var")
            <| 6s
            <| true
            <| validationContext 
    
    [<TestMethod>]
    member __.``getVariableValidation - is less than variable - int16 - is equal - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4s :> obj) |]
            }
        assertVariableRuleIsValid 
            <| getVariableValidation (IsLessThan <| Variable "Other Var")
            <| 4s
            <| true
            <| validationContext
     
    [<TestMethod>]
    member __.``getVariableValidation - is less than variable - int16 - is less than - returns is valid false``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4s :> obj) |]
            }
        assertVariableRuleIsValid 
            <| getVariableValidation (IsLessThan <| Variable "Other Var")
            <| 3s
            <| false
            <| validationContext

    [<TestMethod>]
    member __.``getVariableValidation - is less than variable - int32 - variable doesn't exist - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsLessThan <| Variable "Other Var")
            <| 6
            <| true
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``getVariableValidation - is less than variable - int32 - is more than - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4 :> obj) |]
            }
        assertVariableRuleIsValid 
            <| getVariableValidation (IsLessThan <| Variable "Other Var")
            <| 6
            <| true
            <| validationContext

    [<TestMethod>]
    member __.``getVariableValidation - is less than variable - int32 - is equal - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4 :> obj) |]
            }
        assertVariableRuleIsValid 
            <| getVariableValidation (IsLessThan <| Variable "Other Var")
            <| 4
            <| true
            <| validationContext

    [<TestMethod>]
    member __.``getVariableValidation - is less than variable - int32 - is less than - returns is valid false``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4 :> obj) |]
            }
        assertVariableRuleIsValid 
            <| getVariableValidation (IsLessThan <| Variable "Other Var")
            <| 3
            <| false
            <| validationContext
        
    [<TestMethod>]
    member __.``getVariableValidation - is less than variable - int64 - variable doesn't exist - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsLessThan <| Variable "Other Var")
            <| 6L
            <| true
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``getVariableValidation - is less than variable - int64 - is more than - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4L :> obj) |]
            }
        assertVariableRuleIsValid 
            <| getVariableValidation (IsLessThan <| Variable "Other Var")
            <| 6L
            <| true
            <| validationContext

    [<TestMethod>]
    member __.``getVariableValidation - is less than variable - int64 - is equal - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4L :> obj) |]
            }
        assertVariableRuleIsValid 
            <| getVariableValidation (IsLessThan <| Variable "Other Var")
            <| 4L
            <| true
            <| validationContext

    [<TestMethod>]
    member __.``getVariableValidation - is less than variable - int64 - is less than - returns is valid false``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4L :> obj) |]
            }
        assertVariableRuleIsValid 
            <| getVariableValidation (IsLessThan <| Variable "Other Var")
            <| 3L
            <| false
            <| validationContext

    [<TestMethod>]
    member __.``getVariableValidation - is less than variable - uint16 - variable doesn't exist - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsLessThan <| Variable "Other Var")
            <| 6us
            <| true
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``getVariableValidation - is less than variable - uint16 - is more than - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4us :> obj) |]
            }
        assertVariableRuleIsValid 
            <| getVariableValidation (IsLessThan <| Variable "Other Var")
            <| 6us
            <| true
            <| validationContext

    [<TestMethod>]
    member __.``getVariableValidation - is less than variable - uint16 - is equal - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4us :> obj) |]
            }
        assertVariableRuleIsValid 
            <| getVariableValidation (IsLessThan <| Variable "Other Var")
            <| 4us
            <| true
            <| validationContext

    [<TestMethod>]
    member __.``getVariableValidation - is less than variable - uint16 - is less than - returns is valid false``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4us :> obj) |]
            }
        assertVariableRuleIsValid 
            <| getVariableValidation (IsLessThan <| Variable "Other Var")
            <| 3us
            <| false
            <| validationContext

    [<TestMethod>]
    member __.``getVariableValidation - is less than variable - uint32 - variable doesn't exist - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsLessThan <| Variable "Other Var")
            <| 6u
            <| true
            <| createVariableValidationContext "Var"
        
    [<TestMethod>]
    member __.``getVariableValidation - is less than variable - uint32 - is more than - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4u :> obj) |]
            }
        assertVariableRuleIsValid 
            <| getVariableValidation (IsLessThan <| Variable "Other Var")
            <| 6u
            <| true
            <| validationContext

    [<TestMethod>]
    member __.``getVariableValidation - is less than variable - uint32 - is equal - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4u :> obj) |]
            }
        assertVariableRuleIsValid 
            <| getVariableValidation (IsLessThan <| Variable "Other Var")
            <| 4u
            <| true
            <| validationContext

    [<TestMethod>]
    member __.``getVariableValidation - is less than variable - uint32 - is less than - returns is valid false``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4u :> obj) |]
            }
        assertVariableRuleIsValid 
            <| getVariableValidation (IsLessThan <| Variable "Other Var")
            <| 3u
            <| false
            <| validationContext
        
    [<TestMethod>]
    member __.``getVariableValidation - is less than variable - uint64 - variable doesn't exist - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsLessThan <| Variable "Other Var")
            <| 6UL
            <| true
            <| createVariableValidationContext "Var"
                
    [<TestMethod>]
    member __.``getVariableValidation - is less than variable - uint64 - is more than - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4UL :> obj) |]
            }
        assertVariableRuleIsValid 
            <| getVariableValidation (IsLessThan <| Variable "Other Var")
            <| 6UL
            <| true
            <| validationContext

    [<TestMethod>]
    member __.``getVariableValidation - is less than variable - uint64 - is equal - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4UL :> obj) |]
            }
        assertVariableRuleIsValid 
            <| getVariableValidation (IsLessThan <| Variable "Other Var")
            <| 4UL
            <| true
            <| validationContext

    [<TestMethod>]
    member __.``getVariableValidation - is less than variable - uint64 - is less than - returns is valid false``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4UL :> obj) |]
            }
        assertVariableRuleIsValid 
            <| getVariableValidation (IsLessThan <| Variable "Other Var")
            <| 3UL
            <| false
            <| validationContext
        
    [<TestMethod>]
    member __.``getVariableValidation - is less than variable - byte - variable doesn't exist - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsLessThan <| Variable "Other Var")
            <| 6uy
            <| true
            <| createVariableValidationContext "Var"
                      
    [<TestMethod>]
    member __.``getVariableValidation - is less than variable - byte - is more than - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4uy :> obj) |]
            }
        assertVariableRuleIsValid 
            <| getVariableValidation (IsLessThan <| Variable "Other Var")
            <| 6uy
            <| true
            <| validationContext

    [<TestMethod>]
    member __.``getVariableValidation - is less than variable - byte - is equal - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4uy :> obj) |]
            }
        assertVariableRuleIsValid 
            <| getVariableValidation (IsLessThan <| Variable "Other Var")
            <| 4uy
            <| true
            <| validationContext

    [<TestMethod>]
    member __.``getVariableValidation - is less than variable - byte - is less than - returns is valid false``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4uy :> obj) |]
            }
        assertVariableRuleIsValid 
            <| getVariableValidation (IsLessThan <| Variable "Other Var")
            <| 3uy
            <| false
            <| validationContext

    [<TestMethod>]
    member __.``getVariableValidation - is less than variable - sbyte - variable doesn't exist - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsLessThan <| Variable "Other Var")
            <| 6y
            <| true
            <| createVariableValidationContext "Var"
              
    [<TestMethod>]
    member __.``getVariableValidation - is less than variable - sbyte - is more than - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4y :> obj) |]
            }
        assertVariableRuleIsValid 
            <| getVariableValidation (IsLessThan <| Variable "Other Var")
            <| 6y
            <| true
            <| validationContext

    [<TestMethod>]
    member __.``getVariableValidation - is less than variable - sbyte - is equal - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4y :> obj) |]
            }
        assertVariableRuleIsValid 
            <| getVariableValidation (IsLessThan <| Variable "Other Var")
            <| 4y
            <| true
            <| validationContext

    [<TestMethod>]
    member __.``getVariableValidation - is less than variable - sbyte - is less than - returns is valid false``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4y :> obj) |]
            }
        assertVariableRuleIsValid 
            <| getVariableValidation (IsLessThan <| Variable "Other Var")
            <| 3y
            <| false
            <| validationContext

    [<TestMethod>]
    member __.``getVariableValidation - is less than variable - float - variable doesn't exist - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsLessThan <| Variable "Other Var")
            <| 4.0
            <| true
            <| createVariableValidationContext "Var"
            
    [<TestMethod>]
    member __.``getVariableValidation - is less than variable - float - is more than - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4.0 :> obj) |]
            }
        assertVariableRuleIsValid 
            <| getVariableValidation (IsLessThan <| Variable "Other Var")
            <| 6.0
            <| true
            <| validationContext

    [<TestMethod>]
    member __.``getVariableValidation - is less than variable - float - is equal - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4.0 :> obj) |]
            }
        assertVariableRuleIsValid 
            <| getVariableValidation (IsLessThan <| Variable "Other Var")
            <| 4.0
            <| true
            <| validationContext

    [<TestMethod>]
    member __.``getVariableValidation - is less than variable - float - is less than - returns is valid false``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4.0 :> obj) |]
            }
        assertVariableRuleIsValid 
            <| getVariableValidation (IsLessThan <| Variable "Other Var")
            <| 3.0
            <| false
            <| validationContext

    [<TestMethod>]
    member __.``getVariableValidation - is less than variable - float32 - variable doesn't exist - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsLessThan <| Variable "Other Var")
            <| 4.0f
            <| true
            <| createVariableValidationContext "Var"
              
    [<TestMethod>]
    member __.``getVariableValidation - is less than variable - float32 - is more than - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4.0f :> obj) |]
            }
        assertVariableRuleIsValid 
            <| getVariableValidation (IsLessThan <| Variable "Other Var")
            <| 6.0f
            <| true
            <| validationContext

    [<TestMethod>]
    member __.``getVariableValidation - is less than variable - float32 - is equal - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4.0f :> obj) |]
            }
        assertVariableRuleIsValid 
            <| getVariableValidation (IsLessThan <| Variable "Other Var")
            <| 4.0f
            <| true
            <| validationContext

    [<TestMethod>]
    member __.``getVariableValidation - is less than variable - float32 - is less than - returns is valid false``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4.0f :> obj) |]
            }
        assertVariableRuleIsValid 
            <| getVariableValidation (IsLessThan <| Variable "Other Var")
            <| 3.0f
            <| false
            <| validationContext
        
    [<TestMethod>]
    member __.``getVariableValidation - is less than variable - decimal - variable doesn't exist - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsLessThan <| Variable "Other Var")
            <| 4.0m
            <| true
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``getVariableValidation - is less than variable - decimal - is more than - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4.0m :> obj) |]
            }
        assertVariableRuleIsValid 
            <| getVariableValidation (IsLessThan <| Variable "Other Var")
            <| 6.0m
            <| true
            <| validationContext

    [<TestMethod>]
    member __.``getVariableValidation - is less than variable - decimal - is equal - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4.0m :> obj) |]
            }
        assertVariableRuleIsValid 
            <| getVariableValidation (IsLessThan <| Variable "Other Var")
            <| 4.0m
            <| true
            <| validationContext

    [<TestMethod>]
    member __.``getVariableValidation - is less than variable - decimal - is less than - returns is valid false``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4.0m :> obj) |]
            }
        assertVariableRuleIsValid 
            <| getVariableValidation (IsLessThan <| Variable "Other Var")
            <| 3.0m
            <| false
            <| validationContext
              
    [<TestMethod>]
    member __.``getVariableValidation - is less than variable - datetime - variable doesn't exist - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsLessThan <| Variable "Other Var")
            <| new DateTime(2018, 1, 29)
            <| true
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``getVariableValidation - is less than variable - datetime - is more than - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", new DateTime(2018, 1, 29) :> obj) |]
            }
        assertVariableRuleIsValid 
            <| getVariableValidation (IsLessThan <| Variable "Other Var")
            <| new DateTime(2018, 2, 20)
            <| true
            <| validationContext

    [<TestMethod>]
    member __.``getVariableValidation - is less than variable - datetime - is equal - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", new DateTime(2018, 1, 29) :> obj) |]
            }
        assertVariableRuleIsValid 
            <| getVariableValidation (IsLessThan <| Variable "Other Var")
            <| new DateTime(2018, 1, 29)
            <| true
            <| validationContext

    [<TestMethod>]
    member __.``getVariableValidation - is less than variable - datetime - is less than - returns is valid false``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", new DateTime(2018, 1, 29) :> obj) |]
            }
        assertVariableRuleIsValid 
            <| getVariableValidation (IsLessThan <| Variable "Other Var")
            <| new DateTime(2018, 1, 20)
            <| false
            <| validationContext
            
    [<TestMethod>]
    member __.``getVariableValidation - is more than value - formats error message and target value as error``() = 
        assertVariableRuleErrorMessage 
            <| getVariableValidation (IsMoreThan <| Value 4)
            <| 5
            <| "should not be more than 4 : Int32"
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``getVariableValidation - is more than variable - formats error message and target namd and value as error``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4 :> obj) |]
            }
        assertVariableRuleErrorMessage 
            <| getVariableValidation (IsMoreThan <| Variable "Other Var")
            <| 5
            <| "should not be more than variable 'Other Var', 4 : Int32"
            <| validationContext

    [<TestMethod>]
    member __.``getVariableValidation - is more than value - int16 - is more than - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsMoreThan <| Value 4s)
            <| 6s
            <| false
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``getVariableValidation - is more than value - int16 - is equal - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsMoreThan <| Value 4s)
            <| 4s
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``getVariableValidation - is more than value - int16 - is less than - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsMoreThan <| Value 4s)
            <| 3s
            <| true
            <| createVariableValidationContext "Var"
            
    [<TestMethod>]
    member __.``getVariableValidation - is more than value - int32 - is more than - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsMoreThan <| Value 4)
            <| 6
            <| false
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``getVariableValidation - is more than value - int32 - is equal - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsMoreThan <| Value 4)
            <| 4
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``getVariableValidation - is more than value - int32 - is less than - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsMoreThan <| Value 4)
            <| 3
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``getVariableValidation - is more than value - int64 - is more than - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsMoreThan <| Value 4L)
            <| 6L
            <| false
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``getVariableValidation - is more than value - int64 - is equal - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsMoreThan <| Value 4L)
            <| 4L
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``getVariableValidation - is more than value - int64 - is less than - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsMoreThan <| Value 4L)
            <| 3L
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``getVariableValidation - is more than value - uint16 - is more than - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsMoreThan <| Value 4us)
            <| 6us
            <| false
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``getVariableValidation - is more than value - uint16 - is equal - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsMoreThan <| Value 4us)
            <| 4us
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``getVariableValidation - is more than value - uint16 - is less than - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsMoreThan <| Value 4us)
            <| 3us
            <| true
            <| createVariableValidationContext "Var"
            
    [<TestMethod>]
    member __.``getVariableValidation - is more than value - uint32 - is more than - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsMoreThan <| Value 4u)
            <| 6u
            <| false
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``getVariableValidation - is more than value - uint32 - is equal - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsMoreThan <| Value 4u)
            <| 4u
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``getVariableValidation - is more than value - uint32 - is less than - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsMoreThan <| Value 4u)
            <| 3u
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``getVariableValidation - is more than value - uint64 - is more than - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsMoreThan <| Value 4UL)
            <| 6UL
            <| false
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``getVariableValidation - is more than value - uint64 - is equal - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsMoreThan <| Value 4UL)
            <| 4UL
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``getVariableValidation - is more than value - uint64 - is less than - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsMoreThan <| Value 4UL)
            <| 3UL
            <| true
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``getVariableValidation - is more than value - byte - is more than - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsMoreThan <| Value 4uy)
            <| 6uy
            <| false
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``getVariableValidation - is more than value - byte - is equal - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsMoreThan <| Value 4uy)
            <| 4uy
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``getVariableValidation - is more than value - byte - is less than - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsMoreThan <| Value 4uy)
            <| 3uy
            <| true
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``getVariableValidation - is more than value - sbyte - is more than - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsMoreThan <| Value 4y)
            <| 6y
            <| false
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``getVariableValidation - is more than value - sbyte - is equal - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsMoreThan <| Value 4y)
            <| 4y
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``getVariableValidation - is more than value - sbyte - is less than - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsMoreThan <| Value 4y)
            <| 3y
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``getVariableValidation - is more than value - float - is more than - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsMoreThan <| Value 4.0)
            <| 6.0
            <| false
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``getVariableValidation - is more than value - float - is equal - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsMoreThan <| Value 4.0)
            <| 4.0
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``getVariableValidation - is more than value - float - is less than - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsMoreThan <| Value 4.0)
            <| 3.0
            <| true
            <| createVariableValidationContext "Var"
            
    [<TestMethod>]
    member __.``getVariableValidation - is more than value - float32 - is more than - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsMoreThan <| Value 4.0f)
            <| 6.0f
            <| false
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``getVariableValidation - is more than value - float32 - is equal - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsMoreThan <| Value 4.0f)
            <| 4.0f
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``getVariableValidation - is more than value - float32 - is less than - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsMoreThan <| Value 4.0f)
            <| 3.0f
            <| true
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``getVariableValidation - is more than value - decimal - is more than - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsMoreThan <| Value 4.0m)
            <| 6.0m
            <| false
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``getVariableValidation - is more than value - decimal - is equal - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsMoreThan <| Value 4.0m)
            <| 4.0m
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``getVariableValidation - is more than value - decimal - is less than - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsMoreThan <| Value 4.0m)
            <| 3.0m
            <| true
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``getVariableValidation - is more than value - datetime - is more than - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsMoreThan <| Value (new DateTime(2018, 1, 29)))
            <| new DateTime(2018, 2, 20)
            <| false
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``getVariableValidation - is more than value - datetime - is equal - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsMoreThan <| Value (new DateTime(2018, 1, 29)))
            <| new DateTime(2018, 1, 29)
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``getVariableValidation - is more than value - datetime - is less than - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsMoreThan <| Value (new DateTime(2018, 1, 29)))
            <| new DateTime(2018, 1, 20)
            <| true
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``getVariableValidation - is more than variable - int16 - variable doesn't exist - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsMoreThan <| Variable "Other Var")
            <| 6s
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``getVariableValidation - is more than variable - int16 - is more than - returns is valid false``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4s :> obj) |]
            }
        assertVariableRuleIsValid 
            <| getVariableValidation (IsMoreThan <| Variable "Other Var")
            <| 6s
            <| false
            <| validationContext 
    
    [<TestMethod>]
    member __.``getVariableValidation - is more than variable - int16 - is equal - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4s :> obj) |]
            }
        assertVariableRuleIsValid 
            <| getVariableValidation (IsMoreThan <| Variable "Other Var")
            <| 4s
            <| true
            <| validationContext
     
    [<TestMethod>]
    member __.``getVariableValidation - is more than variable - int16 - is less than - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4s :> obj) |]
            }
        assertVariableRuleIsValid 
            <| getVariableValidation (IsMoreThan <| Variable "Other Var")
            <| 3s
            <| true
            <| validationContext

    [<TestMethod>]
    member __.``getVariableValidation - is more than variable - int32 - variable doesn't exist - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsMoreThan <| Variable "Other Var")
            <| 6
            <| true
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``getVariableValidation - is more than variable - int32 - is more than - returns is valid false``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4 :> obj) |]
            }
        assertVariableRuleIsValid 
            <| getVariableValidation (IsMoreThan <| Variable "Other Var")
            <| 6
            <| false
            <| validationContext

    [<TestMethod>]
    member __.``getVariableValidation - is more than variable - int32 - is equal - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4 :> obj) |]
            }
        assertVariableRuleIsValid 
            <| getVariableValidation (IsMoreThan <| Variable "Other Var")
            <| 4
            <| true
            <| validationContext

    [<TestMethod>]
    member __.``getVariableValidation - is more than variable - int32 - is less than - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4 :> obj) |]
            }
        assertVariableRuleIsValid 
            <| getVariableValidation (IsMoreThan <| Variable "Other Var")
            <| 3
            <| true
            <| validationContext
        
    [<TestMethod>]
    member __.``getVariableValidation - is more than variable - int64 - variable doesn't exist - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsMoreThan <| Variable "Other Var")
            <| 6L
            <| true
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``getVariableValidation - is more than variable - int64 - is more than - returns is valid false``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4L :> obj) |]
            }
        assertVariableRuleIsValid 
            <| getVariableValidation (IsMoreThan <| Variable "Other Var")
            <| 6L
            <| false
            <| validationContext

    [<TestMethod>]
    member __.``getVariableValidation - is more than variable - int64 - is equal - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4L :> obj) |]
            }
        assertVariableRuleIsValid 
            <| getVariableValidation (IsMoreThan <| Variable "Other Var")
            <| 4L
            <| true
            <| validationContext

    [<TestMethod>]
    member __.``getVariableValidation - is more than variable - int64 - is less than - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4L :> obj) |]
            }
        assertVariableRuleIsValid 
            <| getVariableValidation (IsMoreThan <| Variable "Other Var")
            <| 3L
            <| true
            <| validationContext

    [<TestMethod>]
    member __.``getVariableValidation - is more than variable - uint16 - variable doesn't exist - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsMoreThan <| Variable "Other Var")
            <| 6us
            <| true
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``getVariableValidation - is more than variable - uint16 - is more than - returns is valid false``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4us :> obj) |]
            }
        assertVariableRuleIsValid 
            <| getVariableValidation (IsMoreThan <| Variable "Other Var")
            <| 6us
            <| false
            <| validationContext

    [<TestMethod>]
    member __.``getVariableValidation - is more than variable - uint16 - is equal - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4us :> obj) |]
            }
        assertVariableRuleIsValid 
            <| getVariableValidation (IsMoreThan <| Variable "Other Var")
            <| 4us
            <| true
            <| validationContext

    [<TestMethod>]
    member __.``getVariableValidation - is more than variable - uint16 - is less than - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4us :> obj) |]
            }
        assertVariableRuleIsValid 
            <| getVariableValidation (IsMoreThan <| Variable "Other Var")
            <| 3us
            <| true
            <| validationContext

    [<TestMethod>]
    member __.``getVariableValidation - is more than variable - uint32 - variable doesn't exist - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsMoreThan <| Variable "Other Var")
            <| 6u
            <| true
            <| createVariableValidationContext "Var"
        
    [<TestMethod>]
    member __.``getVariableValidation - is more than variable - uint32 - is more than - returns is valid false``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4u :> obj) |]
            }
        assertVariableRuleIsValid 
            <| getVariableValidation (IsMoreThan <| Variable "Other Var")
            <| 6u
            <| false
            <| validationContext

    [<TestMethod>]
    member __.``getVariableValidation - is more than variable - uint32 - is equal - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4u :> obj) |]
            }
        assertVariableRuleIsValid 
            <| getVariableValidation (IsMoreThan <| Variable "Other Var")
            <| 4u
            <| true
            <| validationContext

    [<TestMethod>]
    member __.``getVariableValidation - is more than variable - uint32 - is less than - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4u :> obj) |]
            }
        assertVariableRuleIsValid 
            <| getVariableValidation (IsMoreThan <| Variable "Other Var")
            <| 3u
            <| true
            <| validationContext
        
    [<TestMethod>]
    member __.``getVariableValidation - is more than variable - uint64 - variable doesn't exist - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsMoreThan <| Variable "Other Var")
            <| 6UL
            <| true
            <| createVariableValidationContext "Var"
                
    [<TestMethod>]
    member __.``getVariableValidation - is more than variable - uint64 - is more than - returns is valid false``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4UL :> obj) |]
            }
        assertVariableRuleIsValid 
            <| getVariableValidation (IsMoreThan <| Variable "Other Var")
            <| 6UL
            <| false
            <| validationContext

    [<TestMethod>]
    member __.``getVariableValidation - is more than variable - uint64 - is equal - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4UL :> obj) |]
            }
        assertVariableRuleIsValid 
            <| getVariableValidation (IsMoreThan <| Variable "Other Var")
            <| 4UL
            <| true
            <| validationContext

    [<TestMethod>]
    member __.``getVariableValidation - is more than variable - uint64 - is less than - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4UL :> obj) |]
            }
        assertVariableRuleIsValid 
            <| getVariableValidation (IsMoreThan <| Variable "Other Var")
            <| 3UL
            <| true
            <| validationContext
        
    [<TestMethod>]
    member __.``getVariableValidation - is more than variable - byte - variable doesn't exist - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsMoreThan <| Variable "Other Var")
            <| 6uy
            <| true
            <| createVariableValidationContext "Var"
                      
    [<TestMethod>]
    member __.``getVariableValidation - is more than variable - byte - is more than - returns is valid false``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4uy :> obj) |]
            }
        assertVariableRuleIsValid 
            <| getVariableValidation (IsMoreThan <| Variable "Other Var")
            <| 6uy
            <| false
            <| validationContext

    [<TestMethod>]
    member __.``getVariableValidation - is more than variable - byte - is equal - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4uy :> obj) |]
            }
        assertVariableRuleIsValid 
            <| getVariableValidation (IsMoreThan <| Variable "Other Var")
            <| 4uy
            <| true
            <| validationContext

    [<TestMethod>]
    member __.``getVariableValidation - is more than variable - byte - is less than - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4uy :> obj) |]
            }
        assertVariableRuleIsValid 
            <| getVariableValidation (IsMoreThan <| Variable "Other Var")
            <| 3uy
            <| true
            <| validationContext

    [<TestMethod>]
    member __.``getVariableValidation - is more than variable - sbyte - variable doesn't exist - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsMoreThan <| Variable "Other Var")
            <| 6y
            <| true
            <| createVariableValidationContext "Var"
              
    [<TestMethod>]
    member __.``getVariableValidation - is more than variable - sbyte - is more than - returns is valid false``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4y :> obj) |]
            }
        assertVariableRuleIsValid 
            <| getVariableValidation (IsMoreThan <| Variable "Other Var")
            <| 6y
            <| false
            <| validationContext

    [<TestMethod>]
    member __.``getVariableValidation - is more than variable - sbyte - is equal - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4y :> obj) |]
            }
        assertVariableRuleIsValid 
            <| getVariableValidation (IsMoreThan <| Variable "Other Var")
            <| 4y
            <| true
            <| validationContext

    [<TestMethod>]
    member __.``getVariableValidation - is more than variable - sbyte - is less than - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4y :> obj) |]
            }
        assertVariableRuleIsValid 
            <| getVariableValidation (IsMoreThan <| Variable "Other Var")
            <| 3y
            <| true
            <| validationContext

    [<TestMethod>]
    member __.``getVariableValidation - is more than variable - float - variable doesn't exist - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsMoreThan <| Variable "Other Var")
            <| 4.0
            <| true
            <| createVariableValidationContext "Var"
            
    [<TestMethod>]
    member __.``getVariableValidation - is more than variable - float - is more than - returns is valid false``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4.0 :> obj) |]
            }
        assertVariableRuleIsValid 
            <| getVariableValidation (IsMoreThan <| Variable "Other Var")
            <| 6.0
            <| false
            <| validationContext

    [<TestMethod>]
    member __.``getVariableValidation - is more than variable - float - is equal - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4.0 :> obj) |]
            }
        assertVariableRuleIsValid 
            <| getVariableValidation (IsMoreThan <| Variable "Other Var")
            <| 4.0
            <| true
            <| validationContext

    [<TestMethod>]
    member __.``getVariableValidation - is more than variable - float - is less than - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4.0 :> obj) |]
            }
        assertVariableRuleIsValid 
            <| getVariableValidation (IsMoreThan <| Variable "Other Var")
            <| 3.0
            <| true
            <| validationContext

    [<TestMethod>]
    member __.``getVariableValidation - is more than variable - float32 - variable doesn't exist - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsMoreThan <| Variable "Other Var")
            <| 4.0f
            <| true
            <| createVariableValidationContext "Var"
              
    [<TestMethod>]
    member __.``getVariableValidation - is more than variable - float32 - is more than - returns is valid false``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4.0f :> obj) |]
            }
        assertVariableRuleIsValid 
            <| getVariableValidation (IsMoreThan <| Variable "Other Var")
            <| 6.0f
            <| false
            <| validationContext

    [<TestMethod>]
    member __.``getVariableValidation - is more than variable - float32 - is equal - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4.0f :> obj) |]
            }
        assertVariableRuleIsValid 
            <| getVariableValidation (IsMoreThan <| Variable "Other Var")
            <| 4.0f
            <| true
            <| validationContext

    [<TestMethod>]
    member __.``getVariableValidation - is more than variable - float32 - is less than - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4.0f :> obj) |]
            }
        assertVariableRuleIsValid 
            <| getVariableValidation (IsMoreThan <| Variable "Other Var")
            <| 3.0f
            <| true
            <| validationContext
        
    [<TestMethod>]
    member __.``getVariableValidation - is more than variable - decimal - variable doesn't exist - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsMoreThan <| Variable "Other Var")
            <| 4.0m
            <| true
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``getVariableValidation - is more than variable - decimal - is more than - returns is valid false``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4.0m :> obj) |]
            }
        assertVariableRuleIsValid 
            <| getVariableValidation (IsMoreThan <| Variable "Other Var")
            <| 6.0m
            <| false
            <| validationContext

    [<TestMethod>]
    member __.``getVariableValidation - is more than variable - decimal - is equal - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4.0m :> obj) |]
            }
        assertVariableRuleIsValid 
            <| getVariableValidation (IsMoreThan <| Variable "Other Var")
            <| 4.0m
            <| true
            <| validationContext

    [<TestMethod>]
    member __.``getVariableValidation - is more than variable - decimal - is less than - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 4.0m :> obj) |]
            }
        assertVariableRuleIsValid 
            <| getVariableValidation (IsMoreThan <| Variable "Other Var")
            <| 3.0m
            <| true
            <| validationContext
                       
    [<TestMethod>]
    member __.``getVariableValidation - is more than variable - datetime - variable doesn't exist - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (IsMoreThan <| Variable "Other Var")
            <| new DateTime(2018, 1, 29)
            <| true
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``getVariableValidation - is more than variable - datetime - is more than - returns is valid false``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", new DateTime(2018, 1, 29) :> obj) |]
            }
        assertVariableRuleIsValid 
            <| getVariableValidation (IsMoreThan <| Variable "Other Var")
            <| new DateTime(2018, 2, 20)
            <| false
            <| validationContext

    [<TestMethod>]
    member __.``getVariableValidation - is more than variable - datetime - is equal - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", new DateTime(2018, 1, 29) :> obj) |]
            }
        assertVariableRuleIsValid 
            <| getVariableValidation (IsMoreThan <| Variable "Other Var")
            <| new DateTime(2018, 1, 29)
            <| true
            <| validationContext

    [<TestMethod>]
    member __.``getVariableValidation - is more than variable - datetime - is less than - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", new DateTime(2018, 1, 29) :> obj) |]
            }
        assertVariableRuleIsValid 
            <| getVariableValidation (IsMoreThan <| Variable "Other Var")
            <| new DateTime(2018, 1, 20)
            <| true
            <| validationContext
    
    [<TestMethod>]
    member __.``getVariableValidation - and rule - formats left validation message and right validation message as error``() = 
        assertVariableRuleErrorMessage 
            <| getVariableValidation ((IsLessThan <| Value 10) &&& IsNegative)
            <| -10
            <| "should not be less than 10 : Int32 AND should not be negative"
            <| createVariableValidationContext "Var"

    [<TestMethod>]
       member __.``getVariableValidation - and rule - left operand is valid, right operand is valid - returns is valid true``() = 
           assertVariableRuleIsValid 
               <| getVariableValidation (IsZero &&& IsNegative)
               <| 6
               <| true
               <| createVariableValidationContext "Var"
    
    [<TestMethod>]
       member __.``getVariableValidation - and rule - left operand is invalid, right operand is valid - returns is valid true``() = 
           assertVariableRuleIsValid 
               <| getVariableValidation (IsZero &&& IsNegative)
               <| 0
               <| true
               <| createVariableValidationContext "Var"
    
    [<TestMethod>]
       member __.``getVariableValidation - and rule - left operand is valid, right operand is invalid - returns is valid true``() = 
           assertVariableRuleIsValid 
               <| getVariableValidation (IsZero &&& IsNegative)
               <| -10
               <| true
               <| createVariableValidationContext "Var"
    
    [<TestMethod>]
       member __.``getVariableValidation - and rule - left operand is invalid, right operand is invalid - returns is valid false``() = 
           assertVariableRuleIsValid 
               <| getVariableValidation ((IsLessThan <| Value 10) &&& IsNegative)
               <| -10
               <| false
               <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``getVariableValidation - or rule - left and right operands are invalid - formats left validation message and right validation message as error``() = 
        assertVariableRuleErrorMessage 
            <| getVariableValidation ((IsLessThan <| Value 10) ||| IsNegative)
            <| -10
            <| "should not be less than 10 : Int32 AND should not be negative"
            <| createVariableValidationContext "Var"

    [<TestMethod>]
    member __.``getVariableValidation - or rule - only left operand is invalid - formats left validation message as error``() = 
        assertVariableRuleErrorMessage 
            <| getVariableValidation (IsZero ||| IsNegative)
            <| 0
            <| "should not be zero"
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``getVariableValidation - or rule - only right operand is invalid - formats right validation message as error``() = 
        assertVariableRuleErrorMessage 
            <| getVariableValidation (IsZero ||| IsNegative)
            <| -10
            <| "should not be negative"
            <| createVariableValidationContext "Var"

    [<TestMethod>]
       member __.``getVariableValidation - or rule - left operand is valid, right operand is valid - returns is valid true``() = 
           assertVariableRuleIsValid 
               <| getVariableValidation (IsZero ||| IsNegative)
               <| 6
               <| true
               <| createVariableValidationContext "Var"
    
    [<TestMethod>]
       member __.``getVariableValidation - or rule - left operand is invalid, right operand is valid - returns is valid false``() = 
           assertVariableRuleIsValid 
               <| getVariableValidation (IsZero ||| IsNegative)
               <| 0
               <| false
               <| createVariableValidationContext "Var"
    
    [<TestMethod>]
       member __.``getVariableValidation - or rule - left operand is valid, right operand is invalid - returns is valid false``() = 
           assertVariableRuleIsValid 
               <| getVariableValidation (IsZero ||| IsNegative)
               <| -10
               <| false
               <| createVariableValidationContext "Var"
    
    [<TestMethod>]
       member __.``getVariableValidation - or rule - left operand is invalid, right operand is invalid - returns is valid false``() = 
           assertVariableRuleIsValid 
               <| getVariableValidation ((IsLessThan <| Value 10) ||| IsNegative)
               <| -10
               <| false
               <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``getVariableValidation - is value - formats error message and target value as error``() = 
        assertVariableRuleErrorMessage 
            <| getVariableValidation (Is <| Value 5)
            <| 5
            <| "should not equal 5 : Int32"
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``getVariableValidation - is variable - formats error message and target namd and value as error``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 5 :> obj) |]
            }
        assertVariableRuleErrorMessage 
            <| getVariableValidation (Is <| Variable "Other Var")
            <| 5
            <| "should not equal variable 'Other Var', 5 : Int32"
            <| validationContext

    [<TestMethod>]
    member __.``getVariableValidation - is value - equals to value - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (Is <| Value 4)
            <| 4
            <| false
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``getVariableValidation - is value - not equals to value - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (Is <| Value 4)
            <| 3
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``getVariableValidation - is variable - variable doesn't exist - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| getVariableValidation (Is <| Variable "Other Var")
            <| 6
            <| true
            <| createVariableValidationContext "Var"
    
    [<TestMethod>]
    member __.``getVariableValidation - is variable - equals to variable - returns is valid false``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 10 :> obj) |]
            }
        assertVariableRuleIsValid 
            <| getVariableValidation (Is <| Variable "Other Var")
            <| 10
            <| false
            <| validationContext
            
    [<TestMethod>]
    member __.``getVariableValidation - is variable - not equals to variable - returns is valid true``() = 
        let validationContext = 
            { 
                createVariableValidationContext "Var" 
                with Variables = new Map<string, obj> [| ("Other Var", 10 :> obj) |]
            }
        assertVariableRuleIsValid 
            <| getVariableValidation (Is <| Variable "Other Var")
            <| 5
            <| true
            <| validationContext