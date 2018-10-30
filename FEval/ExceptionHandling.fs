namespace FEval

module ExceptionHandling =
    open System
    open System.Reflection
    open Microsoft.FSharp.Quotations

    exception EvaluationException of Exception * EvaluationState

    // Private functions

    let private handleTargetInvocationException (ex : TargetInvocationException) state =
        let innerException = ex.InnerException
        // If the inner exception is EvaluationException then we bubble the exception.
        // Otherwise we raise a new evaluation exception for the current state.
        match innerException with
        | :? EvaluationException -> raise innerException 
        | _                      -> raise (EvaluationException (innerException, state))


    // Public functions

    let withExceptionHandling action (expr :Expr) state =
        try 
            action expr state 
        with
        | :? TargetInvocationException as ex -> handleTargetInvocationException ex state
        