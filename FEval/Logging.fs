namespace FEval

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

    type FileAppenderMessage =
        | AddLineToFile of fileName : string * line : string
        | Sync of replyChannel : AsyncReplyChannel<unit>

    // Private functions

    let private fileAppender = MailboxProcessor<FileAppenderMessage>.Start(fun inbox ->
        let rec loop() =
            async {
                    let! message = inbox.Receive();
                    match message with
                    | AddLineToFile (fileName, line) -> File.AppendAllText (fileName, line)
                    | FileAppenderMessage.Sync reply -> reply.Reply()
                    do! loop ()
            }
        loop ())

    let private appendEndOfLine line =
        line + "\r\n"

    let private appendLineToFile fileName formatter data =
        fileAppender.Post <| AddLineToFile (fileName, appendEndOfLine <| formatter data)
    
    let private syncFileApender() =
        fileAppender.PostAndReply (fun r -> FileAppenderMessage.Sync r)

    // Public functions
    
    let syncLoggers() = 
        syncFileApender()

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
        sprintf "%s - %A - %s (%i) - Thread %i - %s"
            <| formatDateTimeForLog logEvent.Time
            <| logEvent.RunDetails.RunId
            <| logEvent.RunDetails.ProcessName
            <| logEvent.RunDetails.ProcessId
            <| logEvent.RunDetails.ThreadId
            <| formatter logEvent.InspectionResult

    let createCsvFormatter formatter logEvent =
        sprintf "%s,%A,%s,%i,%i,%s"
            <| formatDateTimeForLog logEvent.Time
            <| logEvent.RunDetails.RunId
            <| logEvent.RunDetails.ProcessName
            <| logEvent.RunDetails.ProcessId
            <| logEvent.RunDetails.ThreadId
            <| formatter logEvent.InspectionResult

    let createCsvFileHeader inspectionHeader =
        sprintf "Time,Run Id,Process Name,Process Id,ThreadId,%s" inspectionHeader