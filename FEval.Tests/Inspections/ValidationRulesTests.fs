namespace FEval.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open FEval.Inspections.ValidationTypes
open FEval.Inspections.ValidationRules

[<TestClass>]
type ValidationRulesTests() =

    let validationContext = 
        {
            Variables = new Map<string, obj> [||]
        }

    let assertVariableRule rule assertion =
        match rule with
        | VariableRule definition -> assertion definition
        | _                       -> Assert.Fail("Not a variable rule")

    let assertVariableRuleErrorMessage rule value expectedErrorMessage =
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

    let assertVariableRuleIsValid rule value isValid =
        assertVariableRule 
            <| rule
            <| fun definition ->
                Assert.AreEqual(isValid, definition.Validation.IsValid value)

    [<TestMethod>]
    member this.``ifVariable - set variable name - reutrns rule with variable name``() = 
        assertVariableRule 
            <| ifVariable "Var" IsZero ReturnError
            <| fun definition -> Assert.AreEqual("Var", definition.VariableName)
    
    [<TestMethod>]
    member this.``ifVariable - returns error when invalid - reutrns rule that returns error``() = 
        assertVariableRule 
            <| ifVariable "Var" IsZero ReturnError 
            <| fun definition -> Assert.AreEqual(ReturnError, definition.ReturnWhenInvalid)
    
    [<TestMethod>]
    member this.``ifVariable - returns warning when invalid - reutrns rule that returns warning``() = 
        assertVariableRule 
            <| ifVariable "Var" IsZero ReturnWarning
            <| fun definition -> Assert.AreEqual(ReturnWarning, definition.ReturnWhenInvalid)

    [<TestMethod>]
    member this.``ifVariable - is zero - formats name and type as error``() = 
        assertVariableRuleErrorMessage 
            <| ifVariable "Var" IsZero ReturnError
            <| 0
            <| "Variable 'Var', 0 : Int32, should not be zero"
                
    [<TestMethod>]
    member this.``ifVariable - is zero - int16 - is zero - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsZero ReturnError
            <| 0s
            <| false

    [<TestMethod>]
    member this.``ifVariable - is zero - int16 - is not zero - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsZero ReturnError
            <| 3s
            <| true

    [<TestMethod>]
    member this.``ifVariable - is zero - int32 - is zero - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsZero ReturnError
            <| 0
            <| false

    [<TestMethod>]
    member this.``ifVariable - is zero - int32 - is not zero - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsZero ReturnError
            <| 3
            <| true

    [<TestMethod>]
    member this.``ifVariable - is zero - int64 - is zero - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsZero ReturnError
            <| 0L
            <| false

    [<TestMethod>]
    member this.``ifVariable - is zero - int64 - is not zero - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsZero ReturnError
            <| 3L
            <| true
                             
    [<TestMethod>]
    member this.``ifVariable - is zero - uint16 - is zero - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsZero ReturnError
            <| 0us
            <| false

    [<TestMethod>]
    member this.``ifVariable - is zero - uint16 - is not zero - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsZero ReturnError
            <| 3us
            <| true

    [<TestMethod>]
    member this.``ifVariable - is zero - uint32 - is zero - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsZero ReturnError
            <| 0u
            <| false

    [<TestMethod>]
    member this.``ifVariable - is zero - uint32 - is not zero - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsZero ReturnError
            <| 3u
            <| true

    [<TestMethod>]
    member this.``ifVariable - is zero - uint64 - is zero - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsZero ReturnError
            <| 0UL
            <| false

    [<TestMethod>]
    member this.``ifVariable - is zero - uint64 - is not zero - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsZero ReturnError
            <| 3UL
            <| true
    
    [<TestMethod>]
    member this.``ifVariable - is zero - byte - is zero - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsZero ReturnError
            <| 0uy
            <| false

    [<TestMethod>]
    member this.``ifVariable - is zero - byte - is not zero - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsZero ReturnError
            <| 3uy
            <| true
            
    [<TestMethod>]
    member this.``ifVariable - is zero - sbyte - is zero - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsZero ReturnError
            <| 0y
            <| false

    [<TestMethod>]
    member this.``ifVariable - is zero - sbyte - is not zero - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsZero ReturnError
            <| 3y
            <| true
    
    [<TestMethod>]
    member this.``ifVariable - is zero - float - is zero - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsZero ReturnError
            <| 0.0
            <| false

    [<TestMethod>]
    member this.``ifVariable - is zero - float - is not zero - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsZero ReturnError
            <| 3.0
            <| true

    [<TestMethod>]
    member this.``ifVariable - is zero - float32 - is zero - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsZero ReturnError
            <| 0.0f
            <| false

    [<TestMethod>]
    member this.``ifVariable - is zero - float32 - is not zero - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsZero ReturnError
            <| 3.0f
            <| true
            
    [<TestMethod>]
    member this.``ifVariable - is zero - decimal - is zero - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsZero ReturnError
            <| 0.0m
            <| false

    [<TestMethod>]
    member this.``ifVariable - is zero - decimal - is not zero - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsZero ReturnError
            <| 3.0m
            <| true
            
    [<TestMethod>]
    member this.``ifVariable - is zero - string - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsZero ReturnError
            <| "Hello"
            <| true

    [<TestMethod>]
    member this.``ifVariable - is negative - formats name and type as error``() = 
        assertVariableRuleErrorMessage 
            <| ifVariable "Var" IsNegative ReturnError
            <| -8
            <| "Variable 'Var', -8 : Int32, should not be negative"

    [<TestMethod>]
    member this.``ifVariable - is negative - int16 - is negative - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsNegative ReturnError
            <| -8s
            <| false

    [<TestMethod>]
    member this.``ifVariable - is negative - int16 - is not negative - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsNegative ReturnError
            <| 3s
            <| true

    [<TestMethod>]
    member this.``ifVariable - is negative - int32 - is negative - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsNegative ReturnError
            <| -8
            <| false

    [<TestMethod>]
    member this.``ifVariable - is negative - int32 - is not negative - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsNegative ReturnError
            <| 3
            <| true

    [<TestMethod>]
    member this.``ifVariable - is negative - int64 - is negative - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsNegative ReturnError
            <| -8L
            <| false

    [<TestMethod>]
    member this.``ifVariable - is negative - int64 - is not negative - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsNegative ReturnError
            <| 3L
            <| true

    [<TestMethod>]
    member this.``ifVariable - is negative - uint16 - is not negative - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsNegative ReturnError
            <| 3us
            <| true

    [<TestMethod>]
    member this.``ifVariable - is negative - uint32 - is not negative - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsNegative ReturnError
            <| 3u
            <| true

    [<TestMethod>]
    member this.``ifVariable - is negative - uint64 - is not negative - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsNegative ReturnError
            <| 3UL
            <| true
    
    [<TestMethod>]
    member this.``ifVariable - is negative - byte - is not negative - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsNegative ReturnError
            <| 3uy
            <| true
            
    [<TestMethod>]
    member this.``ifVariable - is negative - sbyte - is negative - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsNegative ReturnError
            <| -8y
            <| false

    [<TestMethod>]
    member this.``ifVariable - is negative - sbyte - is not negative - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsNegative ReturnError
            <| 3y
            <| true
    
    [<TestMethod>]
    member this.``ifVariable - is negative - float - is negative - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsNegative ReturnError
            <| -8.0
            <| false

    [<TestMethod>]
    member this.``ifVariable - is negative - float - is not negative - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsNegative ReturnError
            <| 3.0
            <| true

    [<TestMethod>]
    member this.``ifVariable - is negative - float32 - is negative - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsNegative ReturnError
            <| -8.0f
            <| false

    [<TestMethod>]
    member this.``ifVariable - is negative - float32 - is not negative - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsNegative ReturnError
            <| 3.0f
            <| true
            
    [<TestMethod>]
    member this.``ifVariable - is negative - decimal - is negative - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsNegative ReturnError
            <| -8.0m
            <| false

    [<TestMethod>]
    member this.``ifVariable - is negative - decimal - is not negative - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsNegative ReturnError
            <| 3.0m
            <| true
            
    [<TestMethod>]
    member this.``ifVariable - is negative - string - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsNegative ReturnError
            <| "Hello"
            <| true
            
    [<TestMethod>]
    member this.``ifVariable - is empty - formats name and type as error``() = 
        assertVariableRuleErrorMessage 
            <| ifVariable "Var" IsEmpty ReturnError
            <| [||]
            <| "Variable 'Var', Object[], should not be empty"
                
    [<TestMethod>]
    member this.``ifVariable - is empty - string - is empty - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsEmpty ReturnError
            <| ""
            <| false
                  
    [<TestMethod>]
    member this.``ifVariable - is empty - string - is not empty - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsEmpty ReturnError
            <| "Hello"
            <| true
                
    [<TestMethod>]
    member this.``ifVariable - is empty - int - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsEmpty ReturnError
            <| 8
            <| true

    [<TestMethod>]
    member this.``ifVariable - is empty - seq - is empty - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsEmpty ReturnError
            <| Seq.ofArray [||]
            <| false
                
    [<TestMethod>]
    member this.``ifVariable - is empty - seq - is not empty - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsEmpty ReturnError
            <| seq {1 .. 10}
            <| true

    [<TestMethod>]
    member this.``ifVariable - is empty - array - is empty - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsEmpty ReturnError
            <| [||]
            <| false
                
    [<TestMethod>]
    member this.``ifVariable - is empty - array - is not empty - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsEmpty ReturnError
            <| [| 1 ; 2 ; 3 |]
            <| true
     
    [<TestMethod>]
    member this.``ifVariable - is empty - list - is empty - returns is valid false``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsEmpty ReturnError
            <| []
            <| false
                
    [<TestMethod>]
    member this.``ifVariable - is empty - list - is not empty - returns is valid true``() = 
        assertVariableRuleIsValid 
            <| ifVariable "Var" IsEmpty ReturnError
            <| [ 1 ; 2 ; 3 ]
            <| true