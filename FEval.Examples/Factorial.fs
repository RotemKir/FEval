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
        Console.WriteLine()
        Console.WriteLine("Enter a number to calculate factorial for:")
        let number = Int32.Parse <| Console.ReadLine()
        let result = evalWith "Examples" <@ factorial number @> inspections
        Console.WriteLine("Result is: {0}", result)
        
    let runFactorialWithPerformance loggerCreator =
        inspectionOf Performance <| loggerCreator @"Logs\Perfromance"
        |> Array.singleton
        |> runFactorialWithInspections

    let runFactorialWithSetValue loggerCreator =
        inspectionOf SettingValues <| loggerCreator @"Logs\SetValue"
        |> Array.singleton
        |> runFactorialWithInspections

    let runFactorialWithMethodCall loggerCreator =
        inspectionOf MethodCalls <| loggerCreator @"Logs\MethodCall"
        |> Array.singleton
        |> runFactorialWithInspections

    let runFactorialWithMethodCallAndSetValue loggerCreator =
        [| 
            inspectionOf SettingValues <| loggerCreator @"Logs\MethodCallAndSetValue."
            inspectionOf MethodCalls <| loggerCreator @"Logs\MethodCallAndSetValue"
        |]
        |> runFactorialWithInspections

    let runFactorialWithInputValidations loggerCreator =
        [| 
            inspectionOf 
                <| Validation 
                    [|
                        ifVariable "number" (Is <| Value 1) ``Return Warning``
                        ifVariable "number" (``Is Less Than`` <| Value 1) ``Return Error``
                    |]
                <| loggerCreator @"Logs\Validations"
        |]
        |> runFactorialWithInspections

    let runFactorialWithAllInspections loggerCreator =
        let logger = loggerCreator @"Logs\FullLog"
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
