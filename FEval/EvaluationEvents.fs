namespace FEval

module EvaluationEvents =
    open FEval.EvaluationTypes
    open FEval.Logging
    open System

    // Private functions

    let createPreInspectionContext evaluationEvent evaluationState =
            {
                InspectionStage = InspectionStage.Pre
                EvaluationEvent = evaluationEvent
                Time = DateTime.Now
                EvaluationState =  evaluationState
            }
            
    let createPostInspectionContext evaluationEvent evaluationState =
            {
                InspectionStage = InspectionStage.Post
                EvaluationEvent = evaluationEvent
                Time = DateTime.Now
                EvaluationState =  evaluationState
            }
    
    let private getInpectionContext inspectionMessage =
        match inspectionMessage with
        | PreInspectionMessage inspectionContext       -> Some inspectionContext
        | PostInspectionMessage (_, inspectionContext) -> Some inspectionContext
        | _                                            -> None

    let private getEvaluationEvent inspectionMessage =
        getInpectionContext inspectionMessage
        |> Option.bind (fun context -> Some context.EvaluationEvent)

    let private disposeInspector (inspector : Inspector) =
        let disposeAction reply = Dispose(reply)
        inspector.PostAndReply disposeAction 

    let private createLogEvent inspectionMessage inspectionResult =
        let inspectionContext = Option.get <| getInpectionContext inspectionMessage
        {
            Time = inspectionContext.Time
            RunDetails = inspectionContext.EvaluationState.RunDetails
            InspectionResult = inspectionResult
        }

    let private logInspectionResult inspectionMessage inspectionResult logger =
        match inspectionResult with
        | Some result -> logger <| createLogEvent inspectionMessage result
        | None        -> ignore()

    let private inspectionLoop messageHandler logger (inspector : Inspector) =
        let rec loop() = 
            async {
                let! message = inspector.Receive()
                match message with
                | Dispose reply       
                    -> return reply.Reply()
                | InspectionMessage.Sync reply 
                    -> reply.Reply()
                | _ -> logInspectionResult message <| messageHandler message <| logger
                return! loop()
            }
        loop()

    let private postMessage message state =
        Seq.iter 
            <| (fun (i : Inspector) -> i.Post message)
            <| state.Inspectors
        
    let private syncInspectors state =
        let syncAction reply = InspectionMessage.Sync reply 
        Seq.iter 
            <| (fun (i : Inspector) -> i.PostAndReply syncAction)
            <| state.Inspectors

    let private runPreInspections preInspectionContext =
        postMessage <| PreInspectionMessage preInspectionContext
        
    let private runPostInspections preInspectionContext postInspectionContext =
        postMessage <| PostInspectionMessage (preInspectionContext, postInspectionContext)

    // Public functions
    
    let disposeInspectors inspectors =
        Seq.iter disposeInspector inspectors

    let createInspector messageHandler logger =
        MailboxProcessor.Start(inspectionLoop messageHandler logger)

    let inspect state createPreEvent action createPostEvent =
        let preInspectionContext = createPreInspectionContext <| createPreEvent() <| state
        runPreInspections preInspectionContext state
        let (result, postState) = action()
        let postInspectionContext = createPostInspectionContext <| createPostEvent result <| postState
        runPostInspections preInspectionContext postInspectionContext state
        syncInspectors state
        syncLoggers()
        result

    let (|IsPreInspection|_|) inspectionMessage =
        match inspectionMessage with
        | PreInspectionMessage preInspectionContext -> Some preInspectionContext
        | _                                         -> None

    let (|IsPostInspection|_|) inspectionMessage =
        match inspectionMessage with
        | PostInspectionMessage (preInspectionContext, postInspectionConext) 
            -> Some (preInspectionContext, postInspectionConext)
        | _ -> None
    
    let (|IsExprEvent|_|) inspectionMessage =
        match getEvaluationEvent inspectionMessage with 
        | Some (ExprEvent expr) -> Some expr
        | _                     -> None

    let (|IsMethodEvent|_|) inspectionMessage =
        match getEvaluationEvent inspectionMessage with 
        | Some (MethodEvent eventDetails) -> Some eventDetails
        | _                               -> None
    
    let (|IsSetVariableEvent|_|) inspectionMessage =
        match getEvaluationEvent inspectionMessage with 
        | Some (SetVariableEvent eventDetails) -> Some eventDetails
        | _                                    -> None
        
    let (|IsSetPropertyEvent|_|) inspectionMessage =
        match getEvaluationEvent inspectionMessage with 
        | Some (SetPropertyEvent eventDetails) -> Some eventDetails
        | _                                    -> None
        
    let (|IsSetFieldEvent|_|) inspectionMessage =
        match getEvaluationEvent inspectionMessage with 
        | Some (SetFieldEvent eventDetails) -> Some eventDetails
        | _                                 -> None