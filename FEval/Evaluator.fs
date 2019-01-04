namespace FEval

[<RequireQualifiedAccess>]
module  internal Evaluator =
    open System    
    open System.Collections.Generic
    open System.Diagnostics
    open System.Threading
    open Microsoft.FSharp.Quotations
    open FEval.EvaluationTypes
    open FEval.EvaluationEvents
    
    // Private Functions

    let private mergeVariables oldVariables newVariables =
        Map.map 
            (fun name value -> 
                match Map.tryFind name newVariables with
                | Some newValue -> newValue
                | None          -> value
            ) 
            oldVariables
            
    let private setRecVariable (variable : Var) value state =
        state.RecVariables.[variable.Name] <- value
        state

    let private actOnVariable (variable : Var) recVarAction varAction state =
        if state.RecVariables.ContainsKey(variable.Name)
        then recVarAction()
        else varAction()
            
    let private setVarInternal variable value state =
        actOnVariable variable
            (fun() -> setRecVariable variable value state)
            (fun() -> { state with Variables = Map.add variable.Name value state.Variables })
            state

    let private createRunDetails name =
        let currentProcess = Process.GetCurrentProcess()
        let currentThread = Thread.CurrentThread
        
        {
            RunId = Guid.NewGuid()
            RunName = name
            ProcessId = currentProcess.Id
            ProcessName = currentProcess.ProcessName
            ThreadId = currentThread.ManagedThreadId
        }

    let private formatRunName runDetails nextLevel =
        match runDetails.RunName with
        | ""      -> nextLevel
        | runName -> sprintf "%s -> %s" runName nextLevel 

    // Public Functions

    let getLastValue state =
        state.LastValue
        
    let getLastValueAndState state =
        (getLastValue state, state)

    let setLastValue state value =
        { state with LastValue = value }

    let varExists variable state =
        actOnVariable variable
            (fun() -> state.RecVariables.ContainsKey variable.Name)
            (fun() -> Map.containsKey variable.Name state.Variables)
            state

    let setVar (variable : Var) value state =
        let setVariableEventDetails = { Variable = variable ; Value = value }
        inspect 
            <| state
            <| (fun _ -> SetVariableEvent setVariableEventDetails)
            <| (fun _ -> 
                let postState = setVarInternal variable value state 
                in (postState, postState))
            <| (fun _ -> SetVariableEvent setVariableEventDetails)

    let getVar (variable : Var) state =
        actOnVariable variable
            (fun() -> state.RecVariables.Item variable.Name)
            (fun() -> state.Variables.Item variable.Name)
            state

    let setLastValueAsVar (variable : Var) state =
        setVar variable state.LastValue state
    
    let setLastValueAsUnit state =
        setLastValue state ()

    let updateVar (variable : Var) newValueFunc state =
        let newValue = getVar variable state |> newValueFunc
        setVar variable newValue state

    let updateVariables state newState =
        { state with Variables = mergeVariables state.Variables newState.Variables }

    let declareRecVariable (variable : Var) =
        setRecVariable variable <| new obj() 

    let clearRecVariable (variable : Var) state =
        state.RecVariables.Remove(variable.Name) |> ignore

    let evalExpr expr state =
        inspect 
            <| state
            <| (fun () -> ExprEvent expr)
            <| (fun () -> 
                let postState = state.EvalFunc expr state 
                in (postState, postState))
            <| (fun _  -> ExprEvent expr)

    let evalExprAndGetLastValue expr state =
        evalExpr expr state
        |> getLastValue 
        
    let evalExprs exprs state =
        exprs 
        |> Seq.toArray 
        |> Array.map (fun expr -> evalExprAndGetLastValue expr state) 
    
    let evalSingleExpr exprs state =
        evalExprAndGetLastValue <| Seq.head exprs <| state 

    let createNewState name evalFunc inspectors =
        {
            RunDetails = createRunDetails name
            LastValue = ()
            Variables = Map.empty<string, obj>
            RecVariables = new Dictionary<string, obj>()
            EvalFunc = evalFunc
            Inspectors = inspectors
        }

    let invokeMethod instance methodInfo parameters state =
        let methodEventDetails = 
            { 
                Method = methodInfo
                Instance = instance 
                Parameters = parameters
                Result = None 
            }
        inspect 
            <| state
            <| (fun _ -> MethodEvent methodEventDetails)
            <| (fun _ -> 
                let result = Reflection.invokeMethod instance methodInfo parameters 
                in (result, state))
            <| (fun result -> MethodEvent { methodEventDetails with Result = Some result})

    let invokeSetProperty instance propertyInfo value indexerParameters state = 
        let setPropertyEventDetails = 
            {
                Property = propertyInfo
                Instance = instance
                Value = value
                IndexerParameters = indexerParameters
            }
        inspect 
            <| state
            <| (fun _ -> SetPropertyEvent setPropertyEventDetails)
            <| (fun _ -> 
                let result = Reflection.invokeSetProperty instance propertyInfo value indexerParameters 
                in (result, state))
            <| (fun _ -> SetPropertyEvent setPropertyEventDetails)

    let invokeSetField instance fieldInfo value state = 
        let setFieldEventDetails = 
            {
                Field = fieldInfo
                Instance = instance
                Value = value
            }
        inspect 
            <| state
            <| (fun _ -> SetFieldEvent setFieldEventDetails)
            <| (fun _ -> 
                let result = Reflection.invokeSetField instance fieldInfo value
                in (result, state))
            <| (fun _ -> SetFieldEvent setFieldEventDetails)

    let addLevelToRunName levelName (state : EvaluationState) =
        let newRunDetails = 
            { 
                state.RunDetails with RunName = formatRunName state.RunDetails levelName
            }
        { state with RunDetails = newRunDetails }