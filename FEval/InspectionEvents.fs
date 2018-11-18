namespace FEval

module InspectionEvents =
    open FEval.EvaluationTypes
    open System

    // Private functions

    let private runPreInspectors inspectionContext =
        Seq.map (fun i -> i inspectionContext) inspectionContext.EvaluationState.Inspectors
        |> Seq.filter Option.isSome
        |> Seq.map Option.get
        |> Seq.toArray
    
    let private runPostInspectors inspectors inspectionContext =
        Seq.iter (fun i -> i inspectionContext) inspectors

    let private createPreInspectionContext inspectionEvent evaluationState =
            {
                InspectionStage = InspectionStage.Pre
                InspectionEvent = inspectionEvent
                Time = DateTime.Now
                EvaluationState =  evaluationState
            }
            
    let private createPostInspectionContext inspectionEvent evaluationState =
            {
                InspectionStage = InspectionStage.Post
                InspectionEvent = inspectionEvent
                Time = DateTime.Now
                EvaluationState =  evaluationState
            }

    let private createPreMethodEventDetails instance methodInfo parameters =
        {
            Method = methodInfo 
            Instance = instance
            Parameters = parameters 
            Result = None
        }
    
    let private createPostMethodEventDetails instance methodInfo parameters result =
        { 
            Method = methodInfo 
            Instance = instance
            Parameters = parameters 
            Result = Some result 
        }

    let private emptyPostInspections _ _ _ = ignore()

    let private runWithInspections preAction postAction data action =
        let preActionResult = preAction data
        let actionResult = action()
        postAction data preActionResult actionResult
        actionResult

    let private preExprInspection (expr, evaluationState) =
        let preInspectionContext = createPreInspectionContext <| ExprEvent expr <| evaluationState
        runPreInspectors preInspectionContext

    let private postExprInspection (expr, _) postInspectors postEvaluationState =
        let postInspectionContext = createPostInspectionContext <| ExprEvent expr <| postEvaluationState
        runPostInspectors postInspectors postInspectionContext

    let private preInvokeMethodInspections (instance, methodInfo, parameters, evaluationState) =
        let preMethodEventDetails = createPreMethodEventDetails instance methodInfo parameters
        let methodEvent = MethodEvent(preMethodEventDetails)
        let preInspectionContext = createPreInspectionContext methodEvent evaluationState
        runPreInspectors preInspectionContext
    
    let private postInvokeMethodInspections (instance, methodInfo, parameters, evaluationState) postInspectors result =
        let postMethodEventDetails = createPostMethodEventDetails instance methodInfo parameters result
        let methodEvent = MethodEvent(postMethodEventDetails)
        let postInspectionContext = createPostInspectionContext methodEvent evaluationState
        runPostInspectors postInspectors postInspectionContext

    let private preSetVariableInspections (variable, value, evaluationState) =
        let setVariableEventDetails = { Variable = variable ; Value = value }
        let setVariableInspectionEvent = SetVariableEvent (setVariableEventDetails)
        runPreInspectors <| createPreInspectionContext setVariableInspectionEvent evaluationState

    let private preSetPropertyInspections (instance, propertyinfo, value, indexerParameters, evaluationState) =
        let setPropertyEventDetails = 
            { 
                Property = propertyinfo
                Instance = instance
                Value = value 
                IndexerParameters = indexerParameters
            }
        let setPropertyInspectionEvent = SetPropertyEvent (setPropertyEventDetails)
        runPreInspectors <| createPreInspectionContext setPropertyInspectionEvent evaluationState
  
    // Public functions
    
    let evalExprWithInspections data action =
        runWithInspections preExprInspection postExprInspection data action
    
    let invokeMethodWithInspections data action =
        runWithInspections preInvokeMethodInspections postInvokeMethodInspections data action

    let setVariableWithInspections data action =
        runWithInspections preSetVariableInspections emptyPostInspections data action 
    
    let invokeSetPropertyWithInspections data action =
        runWithInspections preSetPropertyInspections emptyPostInspections data action 