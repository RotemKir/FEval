namespace FEval.Inspections

module Persistance =
    open System.IO
    open System

    let appendEndOfLine line =
        line + "\r\n"
    
    let appendLineToFile fileName formatter data =
        File.AppendAllText (fileName, appendEndOfLine <| formatter data)

    let formatCsvLine (line : string) =
        line.Replace("\"", "\"\"")

    let formatTimeForLog (time : DateTime) =
        time.ToString("dd/MM/yyyy hh:mm:ss.fff")

    let setFileHeader fileName header =
        if not <| File.Exists(fileName)
        then appendLineToFile fileName id header