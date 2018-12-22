namespace FEval.Examples

open System
open System.IO
open Factorial

module Main =
    
    let private showMenu() =
        Console.Clear()
        Console.WriteLine("Enter the number of the option to run, any other key will exit:")
        Console.WriteLine("1. Factorial with performance inspection")
        Console.WriteLine("2. Factorial with set value inspection")
        Console.WriteLine("3. Factorial with method call inspection")
        Console.WriteLine("4. Factorial with method call and set value inspection")
        Console.WriteLine()

    let private getOptionToRun optionValue =
        match optionValue with
        | "1" -> Some <| runFactorialWithPerformance
        | "2" -> Some <| runFactorialWithSetValue
        | "3" -> Some <| runFactorialWithMethodCall
        | "4" -> Some <| runFactorialWithMethodCallAndSetValue
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
