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

    let private runWithInspections preInspection postInspection data action =
        let postInspectors = preInspection data
        let actionResult = action()
        postInspection data postInspectors actionResult
        actionResult

    let private preExprInspection (expr, evaluationState) =
        let preInspectionContext = createPreInspectionContext <| ExprEvent expr <| evaluationState
        runPreInspectors preInspectionContext

    let private postExprInspection (expr, _) postInspectors postEvaluationState =
        let postInspectionContext = createPostInspectionContext <| ExprEvent expr <| postEvaluationState
        runPostInspectors postInspectors postInspectionContext

    let private preInvokeMethodInspections (instance, methodInfo, parameters, evaluationState) =
        let methodEvent = MethodEvent <| createPreMethodEventDetails instance methodInfo parameters
        runPreInspectors <| createPreInspectionContext methodEvent evaluationState
    
    let private postInvokeMethodInspections (instance, methodInfo, parameters, evaluationState) postInspectors result =
        let methodEvent = MethodEvent <| createPostMethodEventDetails instance methodInfo parameters result
        let postInspectionContext = createPostInspectionContext methodEvent evaluationState
        runPostInspectors postInspectors postInspectionContext

    let private preSetVariableInspections (variable, value, evaluationState) =
        let setVariableInspectionEvent = SetVariableEvent { Variable = variable ; Value = value }
        runPreInspectors <| createPreInspectionContext setVariableInspectionEvent evaluationState

    let private preSetPropertyInspections (instance, propertyinfo, value, indexerParameters, evaluationState) =
        let setPropertyInspectionEvent = 
            SetPropertyEvent 
                { 
                    Property = propertyinfo
                    Instance = instance
                    Value = value 
                    IndexerParameters = indexerParameters
                }
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