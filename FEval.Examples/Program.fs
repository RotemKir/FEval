namespace FEval.Examples

open System
open System.IO
open FEval.EvaluationTypes
open FEval.Examples.Factorial
open FEval.Examples.Menu
open FEval.Inspections

module Main =
    
    let private addFileType fileType fileName =
        sprintf "%s.%s" fileName fileType 

    let private loggerMenuOptions =
        [|
            {
                Value = "1" 
                Name = "Text Logger" 
                Action = fun() -> (addFileType "txt") >> LogToTextFile
            }
            {
                Value = "2" 
                Name = "Csv Logger" 
                Action = fun() -> (addFileType "csv") >> LogToCsvFile
            }
        |]

    let private selectLogger() =
        Console.WriteLine()
        Console.WriteLine("Choose the logger, any other key will choose text logger:")
        printMenuOptions loggerMenuOptions
        Console.WriteLine()
        
        match Console.ReadLine() |> getOptionToRun loggerMenuOptions with
        | Some option -> option()
        | None        -> LogToTextFile

    let private mainMenuOptions =
        [|
            {
                Value = "1" 
                Name = "Factorial with performance inspection" 
                Action = selectLogger >> runFactorialWithPerformance
            }
            {
                Value = "2" 
                Name = "Factorial with set value inspection" 
                Action = selectLogger >> runFactorialWithSetValue
            }
            {
                Value = "3" 
                Name = "Factorial with method call inspection" 
                Action = selectLogger >> runFactorialWithMethodCall
            }
            {
                Value = "4" 
                Name = "Factorial with method call and set value inspection" 
                Action = selectLogger >> runFactorialWithMethodCallAndSetValue
            }
            {
                Value = "5" 
                Name = "Factorial with input validations" 
                Action = selectLogger >> runFactorialWithInputValidations
            }
            {
                Value = "6" 
                Name = "Factorial with all inspections" 
                Action = selectLogger >> runFactorialWithAllInspections
            }
        |]
                
    let private showMainMenu() =
        Console.Clear()
        Console.WriteLine("Enter the number of the option to run, any other key will exit:")
        printMenuOptions mainMenuOptions
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
        showMainMenu()

        match Console.ReadLine() |> getOptionToRun mainMenuOptions with
        | Some option -> 
            runOption option
            run()
        | None -> ignore()

    [<EntryPoint>]
    let main _ = 
        run()

        0 // return an integer exit code