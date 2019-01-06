namespace FEval.Examples

open System
open System.IO
open FEval.EvaluationTypes
open FEval.Examples.Factorial
open FEval.Examples.Menu

module Main =
    
    let private menuOptions =
        [|
            {
                Value = "1" 
                Name = "Factorial with performance inspection" 
                Action = runFactorialWithPerformance
            }
            {
                Value = "2" 
                Name = "Factorial with set value inspection" 
                Action = runFactorialWithPerformance
            }
            {
                Value = "3" 
                Name = "Factorial with method call inspection" 
                Action = runFactorialWithPerformance
            }
            {
                Value = "4" 
                Name = "Factorial with method call and set value inspection" 
                Action = runFactorialWithPerformance
            }
            {
                Value = "5" 
                Name = "Factorial with input validations" 
                Action = runFactorialWithPerformance
            }
            {
                Value = "6" 
                Name = "Factorial with all inspections" 
                Action = runFactorialWithPerformance
            }
        |]
                
    let private showMenu() =
        Console.Clear()
        Console.WriteLine("Enter the number of the option to run, any other key will exit:")
        printMenuOptions menuOptions
        Console.WriteLine()
        
    let private createLogFolder() =
        match Directory.Exists("Logs") with
        | false -> Directory.CreateDirectory("Logs") |> ignore
        | true  -> ignore()

    let private runOption option =
        try
            try
                option()
            with
            | EvaluationException (ex, _) 
                 -> Console.WriteLine(ex.Message)
            | ex -> Console.WriteLine(ex.Message)
        finally 
            Console.WriteLine("Press any key to continue")
            Console.ReadKey() |> ignore
        
    let rec private run() =
        createLogFolder()
        showMenu()

        match Console.ReadLine() |> getOptionToRun menuOptions with
        | Some option -> 
            runOption option
            run()
        | None -> ignore()

    [<EntryPoint>]
    let main _ = 
        run()

        0 // return an integer exit code
