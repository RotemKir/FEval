namespace FEval

module InspectionEvents =
    open FEval.EvaluationTypes
    open System

    // Private functions

    let createPreInspectionContext inspectionEvent evaluationState =
            {
                InspectionStage = InspectionStage.Pre
                InspectionEvent = inspectionEvent
                Time = DateTime.Now
                EvaluationState =  evaluationState
            }
            
    let createPostInspectionContext inspectionEvent evaluationState =
            {
                InspectionStage = InspectionStage.Post
                InspectionEvent = inspectionEvent
                Time = DateTime.Now
                EvaluationState =  evaluationState
            }
    
    let private getInspectionEvent inspectionMessage =
        match inspectionMessage with
        | PreInspectionMessage inspectionContext       -> Some inspectionContext.InspectionEvent
        | PostInspectionMessage (_, inspectionContext) -> Some inspectionContext.InspectionEvent
        | _                                            -> None

    let private disposeInspector (inspector : Inspector) =
        let disposeAction reply = Dispose(reply)
        inspector.PostAndReply disposeAction 

    let private inspectionLoop messageHandler (inspector : Inspector) =
        let rec loop() = 
            async {
                let! message = inspector.Receive()
                match message with
                | Dispose reply -> return reply.Reply()
                | _             -> messageHandler message 
                return! loop()
            }
        loop()

    let private postMessage message state =
        Seq.iter 
            <| (fun (i : Inspector) -> i.Post message)
            <| state.Inspectors
        
    let private runPreInspections preInspectionContext =
        postMessage <| PreInspectionMessage preInspectionContext
        
    let private runPostInspections preInspectionContext postInspectionContext =
        postMessage <| PostInspectionMessage (preInspectionContext, postInspectionContext)

    // Public functions
    
    let disposeInspectors inspectors =
        Seq.iter disposeInspector inspectors

    let createInspector messageHandler =
        MailboxProcessor.Start(inspectionLoop messageHandler)

    let inspect state createPreEvent action createPostEvent =
        let preInspectionContext = createPreInspectionContext <| createPreEvent() <| state
        runPreInspections preInspectionContext state
        let (result, postState) = action()
        let postInspectionContext = createPostInspectionContext <| createPostEvent result <| postState
        runPostInspections preInspectionContext postInspectionContext state
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
        match getInspectionEvent inspectionMessage with 
        | Some (ExprEvent expr) -> Some expr
        | _                     -> None

    let (|IsMethodEvent|_|) inspectionMessage =
        match getInspectionEvent inspectionMessage with 
        | Some (MethodEvent eventDetails) -> Some eventDetails
        | _                               -> None
    
    let (|IsSetVariableEvent|_|) inspectionMessage =
        match getInspectionEvent inspectionMessage with 
        | Some (SetVariableEvent eventDetails) -> Some eventDetails
        | _                                    -> None
        
    let (|IsSetPropertyEvent|_|) inspectionMessage =
        match getInspectionEvent inspectionMessage with 
        | Some (SetPropertyEvent eventDetails) -> Some eventDetails
        | _                                    -> None