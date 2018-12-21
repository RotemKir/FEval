namespace FEval.Examples

open System
open Factorial
open System.IO

module Main =
    
    let private showMenu() =
        Console.Clear()
        Console.WriteLine("Enter the number of the option to run, any other key will exit:")
        Console.WriteLine("1. Factorial with performance inspection")
        Console.WriteLine("2. Factorial with data set inspection")
        Console.WriteLine()

    let private getOptionToRun optionValue =
        match optionValue with
        | "1" -> Some <| runFactorialWithPerformance
        | "2" -> Some <| runFactorialWithDataSet
        | _   -> None

    let private createLogFolder() =
        match Directory.Exists("Logs") with
        | false -> Directory.CreateDirectory("Logs") |> ignore
        | true  -> ignore()

    let rec private run() =
        createLogFolder()
        showMenu()

        match Console.ReadLine() |> getOptionToRun with
        | Some option -> 
            option()
            run()
        | None -> ignore()

    [<EntryPoint>]
    let main _ = 
        run()

        0 // return an integer exit code
