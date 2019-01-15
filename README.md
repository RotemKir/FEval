# FEval
F# code quotations evaluator

FEval is an evaluator / interpreter for F# code quotations.

You can use it to run the code in the code quotations:

```F#
let result = eval <@ 1 + 1 @> // Will return 2
```

The main feature, however, is evaluating code with inspections.
Inspections allow us to log as we evaluate the code:


```F#
// Log start and end time for each expression we evaluate
let result1 = evalWith "Example" <@ 1 + 1 @> [| inspectionOf Performance <| LogToTextFile "Log.txt" |]

// Log when we're setting values to variables (including values)
let result2 = evalWith "Example" <@ 1 + 1 @> [| inspectionOf SettingValues <| LogToTextFile "Log.txt" |]

// Log when we're calling methods (including parameters and return value)
let result3 = evalWith "Example" <@ 1 + 1 @> [| inspectionOf MethodCalls <| LogToTextFile "Log.txt" |]

// Log both setting values and method calls
let result4 = evalWith "Example" <@ 1 + 1 @> [| 
	inspectionOf SettingValues <| LogToTextFile "Log.txt" 
	inspectionOf MethodCalls <| LogToTextFile "Log.txt" 
|]
```