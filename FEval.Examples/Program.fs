namespace FEval.Examples

open System

module Main =
    
    let private showMenu() =
        Console.Clear()
        Console.WriteLine("Enter the number of the option to run:")
        Console.WriteLine("1. Factorial")
        Console.WriteLine()
        Console.WriteLine("Any other key will exit.")

    let private getOptionToRun optionValue =
        match optionValue with
        | "1" -> Some <| fun () -> Console.WriteLine("Factorial") ; Console.ReadLine() |> ignore
        | _   -> None

    let rec private run() =
        showMenu()

        match Console.ReadLine() |> getOptionToRun with
        | Some option -> option() ; run()
        | None        -> ignore()

    [<EntryPoint>]
    let main _ = 
        run()

        0 // return an integer exit code
