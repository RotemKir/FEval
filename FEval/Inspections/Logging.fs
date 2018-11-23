namespace FEval.Inspections

module Logging =
    open System.IO
    open System
    open FEval.EvaluationTypes

    type Formatter<'a> = LogEvent<'a> -> string

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

    let createStringFormatter formatter logEvent =
        sprintf "%A - %s - Process %s (%i) - Thread %i - %s"
            <| logEvent.RunDetails.RunId
            <| formatDateTimeForLog logEvent.Time
            <| logEvent.RunDetails.ProcessName
            <| logEvent.RunDetails.ProcessId
            <| logEvent.RunDetails.ThreadId
            <| formatter logEvent.InspectionResult

    let createCsvFormatter formatter logEvent =
        sprintf "%A,%s,%s,%i,%i,%s"
            <| logEvent.RunDetails.RunId
            <| formatDateTimeForLog logEvent.Time
            <| logEvent.RunDetails.ProcessName
            <| logEvent.RunDetails.ProcessId
            <| logEvent.RunDetails.ThreadId
            <| formatter logEvent.InspectionResult

    let createCsvFileHeader inspectionHeader =
        sprintf "Run Id,Time,Process Name,Process Id,ThreadId,%s" inspectionHeader