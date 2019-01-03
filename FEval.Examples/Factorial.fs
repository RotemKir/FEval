namespace FEval.Examples

open FEval.Evaluations
open FEval.Inspections
open FEval.Inspectors.ValidationsCommon
open System

module Factorial =

    [<ReflectedDefinition>]
    let private factorial number =
        let mutable x = 1

        for i = 1 to number do
            x <- x * i

        x

    let private runFactorialWithInspections inspections =
        Console.WriteLine("Enter a number to calculate factorial for:")
        let number = Int32.Parse <| Console.ReadLine()
        let result = evalWith "factorial" <@ factorial number @> inspections
        Console.WriteLine("Result is: {0}", result)
        
    let runFactorialWithPerformance() =
        inspectionOf Performance <| LogToTextFile @"Logs\Perfromance.txt"
        |> Array.singleton
        |> runFactorialWithInspections

    let runFactorialWithSetValue() =
        inspectionOf SettingValues <| LogToTextFile @"Logs\SetValue.txt"
        |> Array.singleton
        |> runFactorialWithInspections

    let runFactorialWithMethodCall() =
        inspectionOf MethodCalls <| LogToTextFile @"Logs\MethodCall.txt"
        |> Array.singleton
        |> runFactorialWithInspections

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
                        ifVariable "number" (Is <| Value 1) ``Return Warning``
                        ifVariable "number" (``Is Less Than`` <| Value 1) ``Return Error``
                    |]
                <| LogToTextFile @"Logs\Validations.txt"
        |]
        |> runFactorialWithInspections

    let runFactorialWithAllInspections() =
        let logger = LogToTextFile @"Logs\FullLog.txt"
        [| 
            inspectionOf Performance logger 
            inspectionOf SettingValues logger 
            inspectionOf MethodCalls logger
            inspectionOf 
                <| Validation 
                    [|
                        ifVariable "number" (Is <| Value 1) ``Return Warning``
                        ifVariable "number" (``Is Less Than`` <| Value 1) ``Return Error``
                    |]
                <| logger
        |]
        |> runFactorialWithInspections
