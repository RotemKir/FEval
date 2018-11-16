namespace FEval.Inspections

module Logging =
    open System.IO
    open System

    type Formatter<'a> = 'a -> string

    type FileConfig<'a> =
        {
            Formatter : Formatter<'a>
            Header : string option
        }

    // Private functions

    let private appendEndOfLine line =
        line + "\r\n"

    let private appendLineToFile fileName formatter data =
        File.AppendAllText (fileName, appendEndOfLine <| formatter data)
    
    // Public functions
    
    let formatCsvLine (line : string) =
        line.Replace("\"", "\"\"")

    let formatDateTimeForLog (time : DateTime) =
        time.ToString("dd/MM/yyyy hh:mm:ss.fff")

    let setFileHeader fileName header =
        if not <| File.Exists(fileName)
        then appendLineToFile fileName id header

    let saveToFile fileName fileConfig =
        if fileConfig.Header.IsSome 
        then setFileHeader fileName fileConfig.Header.Value
        
        appendLineToFile fileName fileConfig.Formatter