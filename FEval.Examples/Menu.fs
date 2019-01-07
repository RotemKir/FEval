namespace FEval.Examples

open System

module internal Menu =
    
    type MenuOption<'a> =
        {
            Value : string
            Name : string
            Action : unit -> 'a
        }

    let private getMenuOptionAction menuOption =
        menuOption.Action

    let private getSelectedMenuOption menuOptions selectedValue =
        Array.tryFind 
            <| fun opt -> opt.Value = selectedValue
            <| menuOptions
    
    let private printMenuOption menuOption =
        sprintf "%s. %s" menuOption.Value menuOption.Name
        |> Console.WriteLine
    
    let getOptionToRun menuOptions optionValue =
        Option.map
            <| getMenuOptionAction 
            <| getSelectedMenuOption menuOptions optionValue

    let printMenuOptions menuOptions =
        Seq.iter printMenuOption menuOptions        