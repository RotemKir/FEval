namespace FEval

module Logging =
    open System.IO
    open System
    open FEval.EvaluationTypes

    type SingleLineFormatter<'a> = LogEvent<'a> -> string
    type MultiLineFormatter<'a> = LogEvent<'a> -> string seq

    type Formatter<'a> =
        | SingleLine of SingleLineFormatter<'a>
        | MultiLine of MultiLineFormatter<'a>

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


    let private postAddLineToFile fileName line =
        fileAppender.Post <| AddLineToFile (fileName, appendEndOfLine line)

    let private appendLineToFile formatter fileName data =
        postAddLineToFile fileName <| formatter data
    
    let private appendLinesToFile formatter fileName data =
        Seq.iter 
            <| fun line -> postAddLineToFile fileName line
            <| formatter data

    let private syncFileAppender() =
        fileAppender.PostAndReply (fun r -> FileAppenderMessage.Sync r)

    // Public functions
    
    let syncLoggers() = 
        syncFileAppender()

    let formatCsvLine (line : string) =
        line.Replace("\"", "\"\"")

    let formatDateTimeForLog (time : DateTime) =
        time.ToString("dd/MM/yyyy hh:mm:ss.fff")

    let setFileHeader fileConfig fileName =
        if fileConfig.Header.IsSome && not <| File.Exists(fileName)
        then appendLineToFile id fileName fileConfig.Header.Value

    let createStringLogPrefix logEvent =
        sprintf "%s - %A - %s (%i) - Thread %i - %s"
            <| formatDateTimeForLog logEvent.Time
            <| logEvent.RunDetails.RunId
            <| logEvent.RunDetails.ProcessName
            <| logEvent.RunDetails.ProcessId
            <| logEvent.RunDetails.ThreadId

    let createCsvLogPrefix logEvent =
        sprintf "%s,%A,%s,%i,%i,%s"
            <| formatDateTimeForLog logEvent.Time
            <| logEvent.RunDetails.RunId
            <| logEvent.RunDetails.ProcessName
            <| logEvent.RunDetails.ProcessId
            <| logEvent.RunDetails.ThreadId

    let createStringFormatter formatter logEvent =
        createStringLogPrefix 
            <| logEvent
            <| formatter logEvent.InspectionResult

    let createCsvFormatter formatter logEvent =
        createCsvLogPrefix
            <| logEvent
            <| formatter logEvent.InspectionResult

    let createCsvFileHeader inspectionHeader =
        sprintf "Time,Run Id,Process Name,Process Id,ThreadId,%s" inspectionHeader

    let createLogger fileConfig fileName =
        setFileHeader fileConfig fileName    
        
        match fileConfig.Formatter with
        | SingleLine formatter -> appendLineToFile formatter fileName
        | MultiLine formatter  -> appendLinesToFile formatter fileName
        