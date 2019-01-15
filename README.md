# FEval
F# code quotations evaluator

FEval is an evaluator / interpreter for F# code quotations.

You can use it to run the code in the code quotations:

```
open FEval.Evaluations


let result = eval <@ 1 + 1 @> // Will return 2
```
