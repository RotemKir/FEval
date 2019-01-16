# FEval
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

Another type of inspection performs validations:

```F#
// Warnings will only log when invalid
let result1 = evalWith "Example" <@ 1 + 1 @> [| 
	// Check that when a variable named "number" is 1 then we log a warning 
	inspectionOf 
		<| Validation [| ifVariable "number" (Is <| Value 1) ``Return Warning`` |]
        <| LogToTextFile "Log.txt"
	|]

// Errors will also raise exceptions
let result2 = evalWith "Example" <@ 1 + 1 @> [| 
	// Check that when a variable named "number" equals other variable called "other" 
	// then we log and raise exception
	inspectionOf 
		<| Validation [| ifVariable "number" (Is <| Variable "other") ``Return Error`` |]
        <| LogToTextFile "Log.txt"
	|]

|]
```

Validations include the following checks:
1. ``Is`` (as seen above)
2. ``Is Zero``
3. ``Is Negative``
4. ``Is Empty``
5. ``Is Less Than`` value / variable
6. ``Is More Than`` value / variable

You can also combine validation using ``&&&`` and ``|||`` to create complex conditions.