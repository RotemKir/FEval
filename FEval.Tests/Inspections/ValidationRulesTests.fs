namespace FEval.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open FEval.Inspections.ValidationsCommon
open FEval.Inspections.ValidationRules

[<TestClass>]
type ValidationRulesTests() =

    let emptyValidationContext = 
        {
            Variables = new Map<string, obj> [||]
        }

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
                            VariableName = definition.VariableName
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
    member __.``ifVariable - is zero - formats name and type as error``() = 
        assertVariableRuleErrorMessage 
            <| ifVariable "Var" IsZero ReturnError
            <| 0
            <| "Variable 'Var', 0 : Int32, should not be zero"
            <| emptyValidationContext
                
    [<TestMethod>]
    member __.``ifVariable - is zero - int16 - is zero - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsZero ReturnError
            <| 0s
            <| false
            <| emptyValidationContext

    [<TestMethod>]
    member __.``ifVariable - is zero - int16 - is not zero - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsZero ReturnError
            <| 3s
            <| true
            <| emptyValidationContext

    [<TestMethod>]
    member __.``ifVariable - is zero - int32 - is zero - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsZero ReturnError
            <| 0
            <| false
            <| emptyValidationContext

    [<TestMethod>]
    member __.``ifVariable - is zero - int32 - is not zero - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsZero ReturnError
            <| 3
            <| true
            <| emptyValidationContext

    [<TestMethod>]
    member __.``ifVariable - is zero - int64 - is zero - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsZero ReturnError
            <| 0L
            <| false
            <| emptyValidationContext

    [<TestMethod>]
    member __.``ifVariable - is zero - int64 - is not zero - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsZero ReturnError
            <| 3L
            <| true
            <| emptyValidationContext
                        
    [<TestMethod>]
    member __.``ifVariable - is zero - uint16 - is zero - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsZero ReturnError
            <| 0us
            <| false
            <| emptyValidationContext

    [<TestMethod>]
    member __.``ifVariable - is zero - uint16 - is not zero - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsZero ReturnError
            <| 3us
            <| true
            <| emptyValidationContext
                       
    [<TestMethod>]
    member __.``ifVariable - is zero - uint32 - is zero - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsZero ReturnError
            <| 0u
            <| false
            <| emptyValidationContext

    [<TestMethod>]
    member __.``ifVariable - is zero - uint32 - is not zero - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsZero ReturnError
            <| 3u
            <| true
            <| emptyValidationContext

    [<TestMethod>]
    member __.``ifVariable - is zero - uint64 - is zero - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsZero ReturnError
            <| 0UL
            <| false
            <| emptyValidationContext
                       
    [<TestMethod>]
    member __.``ifVariable - is zero - uint64 - is not zero - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsZero ReturnError
            <| 3UL
            <| true
            <| emptyValidationContext
                
    [<TestMethod>]
    member __.``ifVariable - is zero - byte - is zero - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsZero ReturnError
            <| 0uy
            <| false
            <| emptyValidationContext

    [<TestMethod>]
    member __.``ifVariable - is zero - byte - is not zero - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsZero ReturnError
            <| 3uy
            <| true
            <| emptyValidationContext
            
    [<TestMethod>]
    member __.``ifVariable - is zero - sbyte - is zero - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsZero ReturnError
            <| 0y
            <| false
            <| emptyValidationContext

    [<TestMethod>]
    member __.``ifVariable - is zero - sbyte - is not zero - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsZero ReturnError
            <| 3y
            <| true
            <| emptyValidationContext
    
    [<TestMethod>]
    member __.``ifVariable - is zero - float - is zero - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsZero ReturnError
            <| 0.0
            <| false
            <| emptyValidationContext

    [<TestMethod>]
    member __.``ifVariable - is zero - float - is not zero - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsZero ReturnError
            <| 3.0
            <| true
            <| emptyValidationContext

    [<TestMethod>]
    member __.``ifVariable - is zero - float32 - is zero - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsZero ReturnError
            <| 0.0f
            <| false
            <| emptyValidationContext

    [<TestMethod>]
    member __.``ifVariable - is zero - float32 - is not zero - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsZero ReturnError
            <| 3.0f
            <| true
            <| emptyValidationContext
                        
    [<TestMethod>]
    member __.``ifVariable - is zero - decimal - is zero - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsZero ReturnError
            <| 0.0m
            <| false
            <| emptyValidationContext

    [<TestMethod>]
    member __.``ifVariable - is zero - decimal - is not zero - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsZero ReturnError
            <| 3.0m
            <| true
            <| emptyValidationContext
            
    [<TestMethod>]
    member __.``ifVariable - is zero - string - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsZero ReturnError
            <| "Hello"
            <| true
            <| emptyValidationContext

    [<TestMethod>]
    member __.``ifVariable - is negative - formats name and type as error``() = 
        assertVariableRuleErrorMessage 
            <| ifVariable "Var" IsNegative ReturnError
            <| -8
            <| "Variable 'Var', -8 : Int32, should not be negative"
            <| emptyValidationContext

    [<TestMethod>]
    member __.``ifVariable - is negative - int16 - is negative - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsNegative ReturnError
            <| -8s
            <| false
            <| emptyValidationContext

    [<TestMethod>]
    member __.``ifVariable - is negative - int16 - is not negative - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsNegative ReturnError
            <| 3s
            <| true
            <| emptyValidationContext

    [<TestMethod>]
    member __.``ifVariable - is negative - int32 - is negative - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsNegative ReturnError
            <| -8
            <| false
            <| emptyValidationContext

    [<TestMethod>]
    member __.``ifVariable - is negative - int32 - is not negative - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsNegative ReturnError
            <| 3
            <| true
            <| emptyValidationContext

    [<TestMethod>]
    member __.``ifVariable - is negative - int64 - is negative - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsNegative ReturnError
            <| -8L
            <| false
            <| emptyValidationContext

    [<TestMethod>]
    member __.``ifVariable - is negative - int64 - is not negative - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsNegative ReturnError
            <| 3L
            <| true
            <| emptyValidationContext

    [<TestMethod>]
    member __.``ifVariable - is negative - uint16 - is not negative - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsNegative ReturnError
            <| 3us
            <| true
            <| emptyValidationContext

    [<TestMethod>]
    member __.``ifVariable - is negative - uint32 - is not negative - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsNegative ReturnError
            <| 3u
            <| true
            <| emptyValidationContext

    [<TestMethod>]
    member __.``ifVariable - is negative - uint64 - is not negative - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsNegative ReturnError
            <| 3UL
            <| true
            <| emptyValidationContext
    
    [<TestMethod>]
    member __.``ifVariable - is negative - byte - is not negative - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsNegative ReturnError
            <| 3uy
            <| true
            <| emptyValidationContext
            
    [<TestMethod>]
    member __.``ifVariable - is negative - sbyte - is negative - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsNegative ReturnError
            <| -8y
            <| false
            <| emptyValidationContext
    
    [<TestMethod>]
    member __.``ifVariable - is negative - sbyte - is not negative - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsNegative ReturnError
            <| 3y
            <| true
            <| emptyValidationContext
    
    [<TestMethod>]
    member __.``ifVariable - is negative - float - is negative - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsNegative ReturnError
            <| -8.0
            <| false
            <| emptyValidationContext

    [<TestMethod>]
    member __.``ifVariable - is negative - float - is not negative - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsNegative ReturnError
            <| 3.0
            <| true
            <| emptyValidationContext
    
    [<TestMethod>]
    member __.``ifVariable - is negative - float32 - is negative - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsNegative ReturnError
            <| -8.0f
            <| false
            <| emptyValidationContext
            
    [<TestMethod>]
    member __.``ifVariable - is negative - float32 - is not negative - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsNegative ReturnError
            <| 3.0f
            <| true
            <| emptyValidationContext
            
    [<TestMethod>]
    member __.``ifVariable - is negative - decimal - is negative - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsNegative ReturnError
            <| -8.0m
            <| false
            <| emptyValidationContext

    [<TestMethod>]
    member __.``ifVariable - is negative - decimal - is not negative - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsNegative ReturnError
            <| 3.0m
            <| true
            <| emptyValidationContext
    
    [<TestMethod>]
    member __.``ifVariable - is negative - string - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsNegative ReturnError
            <| "Hello"
            <| true
            <| emptyValidationContext
            
    [<TestMethod>]
    member __.``ifVariable - is empty - formats name and type as error``() = 
        assertVariableRuleErrorMessage 
            <| ifVariable "Var" IsEmpty ReturnError
            <| [||]
            <| "Variable 'Var', Object[], should not be empty"
            <| emptyValidationContext
                
    [<TestMethod>]
    member __.``ifVariable - is empty - string - is empty - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsEmpty ReturnError
            <| ""
            <| false
            <| emptyValidationContext
                  
    [<TestMethod>]
    member __.``ifVariable - is empty - string - is not empty - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsEmpty ReturnError
            <| "Hello"
            <| true
            <| emptyValidationContext
                
    [<TestMethod>]
    member __.``ifVariable - is empty - int - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsEmpty ReturnError
            <| 8
            <| true
            <| emptyValidationContext

    [<TestMethod>]
    member __.``ifVariable - is empty - seq - is empty - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsEmpty ReturnError
            <| Seq.ofArray [||]
            <| false
            <| emptyValidationContext
        
    [<TestMethod>]
    member __.``ifVariable - is empty - seq - is not empty - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsEmpty ReturnError
            <| seq {1 .. 10}
            <| true
            <| emptyValidationContext

    [<TestMethod>]
    member __.``ifVariable - is empty - array - is empty - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsEmpty ReturnError
            <| [||]
            <| false
            <| emptyValidationContext
        
    [<TestMethod>]
    member __.``ifVariable - is empty - array - is not empty - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsEmpty ReturnError
            <| [| 1 ; 2 ; 3 |]
            <| true
            <| emptyValidationContext
     
    [<TestMethod>]
    member __.``ifVariable - is empty - list - is empty - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsEmpty ReturnError
            <| []
            <| false
            <| emptyValidationContext
                
    [<TestMethod>]
    member __.``ifVariable - is empty - list - is not empty - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsEmpty ReturnError
            <| [ 1 ; 2 ; 3 ]
            <| true
            <| emptyValidationContext
            
    [<TestMethod>]
    member __.``ifVariable - is less than value - formats name, type and target value as error``() = 
        assertVariableRuleErrorMessage 
            <| ifVariable "Var" (IsLessThan <| Value 4) ReturnError
            <| 3
            <| "Variable 'Var', 3 : Int32, should not be less than 4 : Int32"
            <| emptyValidationContext
    
    [<TestMethod>]
    member __.``ifVariable - is less than variable - formats name, type and target namd and value as error``() = 
        let validationContext = 
            { 
                Variables = new Map<string, obj> [| ("Other Var", 4 :> obj) |]
            }
        assertVariableRuleErrorMessage 
            <| ifVariable "Var" (IsLessThan <| Variable "Other Var") ReturnError
            <| 3
            <| "Variable 'Var', 3 : Int32, should not be less than variable 'Other Var', 4 : Int32"
            <| validationContext

    [<TestMethod>]
    member __.``ifVariable - is less than variable that doesn't exist - formats name and type as error``() = 
        assertVariableRuleErrorMessage 
            <| ifVariable "Var" (IsLessThan <| Variable "Other Var") ReturnError
            <| 3
            <| "Variable 'Var', 3 : Int32, should not be less than (null)"
            <| emptyValidationContext
    
    [<TestMethod>]
    member __.``ifVariable - is less than value - int16 - is more than - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Value 4s) ReturnError
            <| 6s
            <| true
            <| emptyValidationContext
    
    [<TestMethod>]
    member __.``ifVariable - is less than value - int16 - is equal - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Value 4s) ReturnError
            <| 4s
            <| true
            <| emptyValidationContext
    
    [<TestMethod>]
    member __.``ifVariable - is less than value - int16 - is less than - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Value 4s) ReturnError
            <| 3s
            <| false
            <| emptyValidationContext
            
    [<TestMethod>]
    member __.``ifVariable - is less than value - int32 - is more than - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Value 4) ReturnError
            <| 6
            <| true
            <| emptyValidationContext
    
    [<TestMethod>]
    member __.``ifVariable - is less than value - int32 - is equal - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Value 4) ReturnError
            <| 4
            <| true
            <| emptyValidationContext
    
    [<TestMethod>]
    member __.``ifVariable - is less than value - int32 - is less than - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Value 4) ReturnError
            <| 3
            <| false
            <| emptyValidationContext
    
    [<TestMethod>]
    member __.``ifVariable - is less than value - int64 - is more than - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Value 4L) ReturnError
            <| 6L
            <| true
            <| emptyValidationContext
    
    [<TestMethod>]
    member __.``ifVariable - is less than value - int64 - is equal - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Value 4L) ReturnError
            <| 4L
            <| true
            <| emptyValidationContext
    
    [<TestMethod>]
    member __.``ifVariable - is less than value - int64 - is less than - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Value 4L) ReturnError
            <| 3L
            <| false
            <| emptyValidationContext
    
    [<TestMethod>]
    member __.``ifVariable - is less than value - uint16 - is more than - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Value 4us) ReturnError
            <| 6us
            <| true
            <| emptyValidationContext
    
    [<TestMethod>]
    member __.``ifVariable - is less than value - uint16 - is equal - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Value 4us) ReturnError
            <| 4us
            <| true
            <| emptyValidationContext
    
    [<TestMethod>]
    member __.``ifVariable - is less than value - uint16 - is less than - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Value 4us) ReturnError
            <| 3us
            <| false
            <| emptyValidationContext
            
    [<TestMethod>]
    member __.``ifVariable - is less than value - uint32 - is more than - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Value 4u) ReturnError
            <| 6u
            <| true
            <| emptyValidationContext
    
    [<TestMethod>]
    member __.``ifVariable - is less than value - uint32 - is equal - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Value 4u) ReturnError
            <| 4u
            <| true
            <| emptyValidationContext
    
    [<TestMethod>]
    member __.``ifVariable - is less than value - uint32 - is less than - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Value 4u) ReturnError
            <| 3u
            <| false
            <| emptyValidationContext
    
    [<TestMethod>]
    member __.``ifVariable - is less than value - uint64 - is more than - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Value 4UL) ReturnError
            <| 6UL
            <| true
            <| emptyValidationContext
    
    [<TestMethod>]
    member __.``ifVariable - is less than value - uint64 - is equal - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Value 4UL) ReturnError
            <| 4UL
            <| true
            <| emptyValidationContext
    
    [<TestMethod>]
    member __.``ifVariable - is less than value - uint64 - is less than - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Value 4UL) ReturnError
            <| 3UL
            <| false
            <| emptyValidationContext

    [<TestMethod>]
    member __.``ifVariable - is less than value - byte - is more than - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Value 4uy) ReturnError
            <| 6uy
            <| true
            <| emptyValidationContext
    
    [<TestMethod>]
    member __.``ifVariable - is less than value - byte - is equal - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Value 4uy) ReturnError
            <| 4uy
            <| true
            <| emptyValidationContext
    
    [<TestMethod>]
    member __.``ifVariable - is less than value - byte - is less than - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Value 4uy) ReturnError
            <| 3uy
            <| false
            <| emptyValidationContext

    [<TestMethod>]
    member __.``ifVariable - is less than value - sbyte - is more than - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Value 4y) ReturnError
            <| 6y
            <| true
            <| emptyValidationContext
    
    [<TestMethod>]
    member __.``ifVariable - is less than value - sbyte - is equal - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Value 4y) ReturnError
            <| 4y
            <| true
            <| emptyValidationContext
    
    [<TestMethod>]
    member __.``ifVariable - is less than value - sbyte - is less than - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Value 4y) ReturnError
            <| 3y
            <| false
            <| emptyValidationContext
    
    [<TestMethod>]
    member __.``ifVariable - is less than value - float - is more than - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Value 4.0) ReturnError
            <| 6.0
            <| true
            <| emptyValidationContext
    
    [<TestMethod>]
    member __.``ifVariable - is less than value - float - is equal - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Value 4.0) ReturnError
            <| 4.0
            <| true
            <| emptyValidationContext
    
    [<TestMethod>]
    member __.``ifVariable - is less than value - float - is less than - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Value 4.0) ReturnError
            <| 3.0
            <| false
            <| emptyValidationContext
            
    [<TestMethod>]
    member __.``ifVariable - is less than value - float32 - is more than - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Value 4.0f) ReturnError
            <| 6.0f
            <| true
            <| emptyValidationContext
    
    [<TestMethod>]
    member __.``ifVariable - is less than value - float32 - is equal - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Value 4.0f) ReturnError
            <| 4.0f
            <| true
            <| emptyValidationContext
    
    [<TestMethod>]
    member __.``ifVariable - is less than value - float32 - is less than - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Value 4.0f) ReturnError
            <| 3.0f
            <| false
            <| emptyValidationContext

    [<TestMethod>]
    member __.``ifVariable - is less than value - decimal - is more than - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Value 4.0m) ReturnError
            <| 6.0m
            <| true
            <| emptyValidationContext
    
    [<TestMethod>]
    member __.``ifVariable - is less than value - decimal - is equal - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Value 4.0m) ReturnError
            <| 4.0m
            <| true
            <| emptyValidationContext
    
    [<TestMethod>]
    member __.``ifVariable - is less than value - decimal - is less than - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Value 4.0m) ReturnError
            <| 3.0m
            <| false
            <| emptyValidationContext

    [<TestMethod>]
    member __.``ifVariable - is less than variable - int16 - variable doesn't exist - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Variable "Other Var") ReturnError
            <| 6s
            <| true
            <| emptyValidationContext
    
    [<TestMethod>]
    member __.``ifVariable - is less than variable - int16 - is more than - returns is valid true``() = 
        let validationContext = 
            { 
                Variables = new Map<string, obj> [| ("Other Var", 4s :> obj) |]
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
                Variables = new Map<string, obj> [| ("Other Var", 4s :> obj) |]
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
                Variables = new Map<string, obj> [| ("Other Var", 4s :> obj) |]
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
            <| emptyValidationContext

    [<TestMethod>]
    member __.``ifVariable - is less than variable - int32 - is more than - returns is valid true``() = 
        let validationContext = 
            { 
                Variables = new Map<string, obj> [| ("Other Var", 4 :> obj) |]
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
                Variables = new Map<string, obj> [| ("Other Var", 4 :> obj) |]
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
                Variables = new Map<string, obj> [| ("Other Var", 4 :> obj) |]
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
            <| emptyValidationContext

    [<TestMethod>]
    member __.``ifVariable - is less than variable - int64 - is more than - returns is valid true``() = 
        let validationContext = 
            { 
                Variables = new Map<string, obj> [| ("Other Var", 4L :> obj) |]
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
                Variables = new Map<string, obj> [| ("Other Var", 4L :> obj) |]
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
                Variables = new Map<string, obj> [| ("Other Var", 4L :> obj) |]
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
            <| emptyValidationContext

    [<TestMethod>]
    member __.``ifVariable - is less than variable - uint16 - is more than - returns is valid true``() = 
        let validationContext = 
            { 
                Variables = new Map<string, obj> [| ("Other Var", 4us :> obj) |]
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
                Variables = new Map<string, obj> [| ("Other Var", 4us :> obj) |]
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
                Variables = new Map<string, obj> [| ("Other Var", 4us :> obj) |]
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
            <| emptyValidationContext
        
    [<TestMethod>]
    member __.``ifVariable - is less than variable - uint32 - is more than - returns is valid true``() = 
        let validationContext = 
            { 
                Variables = new Map<string, obj> [| ("Other Var", 4u :> obj) |]
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
                Variables = new Map<string, obj> [| ("Other Var", 4u :> obj) |]
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
                Variables = new Map<string, obj> [| ("Other Var", 4u :> obj) |]
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
            <| emptyValidationContext
                
    [<TestMethod>]
    member __.``ifVariable - is less than variable - uint64 - is more than - returns is valid true``() = 
        let validationContext = 
            { 
                Variables = new Map<string, obj> [| ("Other Var", 4UL :> obj) |]
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
                Variables = new Map<string, obj> [| ("Other Var", 4UL :> obj) |]
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
                Variables = new Map<string, obj> [| ("Other Var", 4UL :> obj) |]
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
            <| emptyValidationContext
                      
    [<TestMethod>]
    member __.``ifVariable - is less than variable - byte - is more than - returns is valid true``() = 
        let validationContext = 
            { 
                Variables = new Map<string, obj> [| ("Other Var", 4uy :> obj) |]
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
                Variables = new Map<string, obj> [| ("Other Var", 4uy :> obj) |]
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
                Variables = new Map<string, obj> [| ("Other Var", 4uy :> obj) |]
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
            <| emptyValidationContext
              
    [<TestMethod>]
    member __.``ifVariable - is less than variable - sbyte - is more than - returns is valid true``() = 
        let validationContext = 
            { 
                Variables = new Map<string, obj> [| ("Other Var", 4y :> obj) |]
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
                Variables = new Map<string, obj> [| ("Other Var", 4y :> obj) |]
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
                Variables = new Map<string, obj> [| ("Other Var", 4y :> obj) |]
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
            <| emptyValidationContext
            
    [<TestMethod>]
    member __.``ifVariable - is less than variable - float - is more than - returns is valid true``() = 
        let validationContext = 
            { 
                Variables = new Map<string, obj> [| ("Other Var", 4.0 :> obj) |]
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
                Variables = new Map<string, obj> [| ("Other Var", 4.0 :> obj) |]
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
                Variables = new Map<string, obj> [| ("Other Var", 4.0 :> obj) |]
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
            <| emptyValidationContext
              
    [<TestMethod>]
    member __.``ifVariable - is less than variable - float32 - is more than - returns is valid true``() = 
        let validationContext = 
            { 
                Variables = new Map<string, obj> [| ("Other Var", 4.0f :> obj) |]
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
                Variables = new Map<string, obj> [| ("Other Var", 4.0f :> obj) |]
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
                Variables = new Map<string, obj> [| ("Other Var", 4.0f :> obj) |]
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
            <| emptyValidationContext

    [<TestMethod>]
    member __.``ifVariable - is less than variable - decimal - is more than - returns is valid true``() = 
        let validationContext = 
            { 
                Variables = new Map<string, obj> [| ("Other Var", 4.0m :> obj) |]
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
                Variables = new Map<string, obj> [| ("Other Var", 4.0m :> obj) |]
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
                Variables = new Map<string, obj> [| ("Other Var", 4.0m :> obj) |]
            }
        assertVariableRuleIsValid 
            <| ifVariable "Var" (IsLessThan <| Variable "Other Var") ReturnError
            <| 3.0m
            <| false
            <| validationContext