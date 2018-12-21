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
    
    let private runFactorialWithInspection inspector =
        Console.WriteLine("Enter a number to calculate factorial for:")
        let number = Int32.Parse <| Console.ReadLine()
        let result = 
            evalWith 
                <| factorialExpr number
                <| [| inspector |]
        Console.WriteLine("Result is: {0}", result)
        Console.WriteLine("Press any key to continue")
        Console.Read() |> ignore
        Console.ReadLine() |> ignore

    let runFactorialWithPerformance() =
        PerformanceInspector.createNew <| PerformanceInspector.createTxtLogger @"Logs\Perfromance.txt"
        |> runFactorialWithInspection

    let runFactorialWithDataSet() =
        DataSetInspector.createNew <| DataSetInspector.createTxtLogger @"Logs\DataSet.txt"
        |> runFactorialWithInspection