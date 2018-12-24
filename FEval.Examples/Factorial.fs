namespace FEval.Examples

open FEval.Evaluations
open FEval.Inspections
open FEval.Inspectors.ValidationRules
open FEval.Inspectors.ValidationsCommon
open System

module Factorial =

    let private factorialExpr = 
        <@ 
            Console.WriteLine("Enter a number to calculate factorial for:")
            let number = Int32.Parse <| Console.ReadLine()
            let mutable x = 1

            for i = 1 to number do
                x <- x * i

            x
        @>

    let private runFactorialWithInspections inspections =
        let result = evalWith factorialExpr inspections
        Console.WriteLine("Result is: {0}", result)
            
    let private runFactorialWithInspection inspector =
        runFactorialWithInspections [| inspector |]

    let runFactorialWithPerformance() =
        inspectionOf Performance <| LogToTextFile @"Logs\Perfromance.txt"
        |> runFactorialWithInspection

    let runFactorialWithSetValue() =
        inspectionOf SettingValues <| LogToTextFile @"Logs\SetValue.txt"
        |> runFactorialWithInspection

    let runFactorialWithMethodCall() =
        inspectionOf MethodCalls <| LogToTextFile @"Logs\MethodCall.txt"
        |> runFactorialWithInspection

    let runFactorialWithMethodCallAndSetValue() =
        [| 
            inspectionOf SettingValues <| LogToTextFile @"Logs\MethodCallAndSetValue.txt"
            inspectionOf MethodCalls <| LogToTextFile @"Logs\MethodCallAndSetValue.txt"
        |]
        |> runFactorialWithInspections

    let runFactorialWithInputValidations() =
        [| 
            inspectionOf 
                <| Validation 
                    [|
                        ifVariable "number" (Is <| Value 1) ReturnWarning
                        ifVariable "number" (IsLessThan <| Value 1) ReturnError
                    |]
                <| LogToTextFile @"Logs\Validations.txt"
        |]
        |> runFactorialWithInspections