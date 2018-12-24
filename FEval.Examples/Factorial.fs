namespace FEval.Examples

open FEval.Evaluations
open FEval.Inspections
open System

module Factorial =

    let private factorialExpr number = 
        <@ 
            let mutable x = 1

            for i = 1 to number do
                x <- x * i

            x
        @>

    let private runFactorialWithInspections inspections =
        Console.WriteLine("Enter a number to calculate factorial for:")
        let number = Int32.Parse <| Console.ReadLine()
        let result = 
            evalWith 
                <| factorialExpr number
                <| inspections
        Console.WriteLine("Result is: {0}", result)
        Console.WriteLine("Press any key to continue")
        Console.ReadKey() |> ignore
    
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