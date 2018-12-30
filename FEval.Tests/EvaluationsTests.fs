namespace FEval.Tests

open System
open FEval.Evaluations
open FEval.Tests.TestHelpers
open Microsoft.VisualStudio.TestTools.UnitTesting
open FEval.EvaluationTypes

[<TestClass>]
type EvaluationsTest() =
    let assertEval expr expectedResult =
        Assert.AreEqual(expectedResult, eval expr)
        
    let collectionAssertEval expr expectedResult =
        CollectionAssert.AreEqual(expectedResult, eval expr)

    let listAssertEval expr expectedResult =
        CollectionAssert.AreEqual(expectedResult |> List.toArray, eval expr |> List.toArray)
            
    let evalWithTry expr exceptionHandler =
        try 
            eval expr |> ignore
        with
        | EvaluationException(ex, _) -> exceptionHandler ex

    (*
    Value (4)
    *)
    [<TestMethod>]
    member __.``Evaluate const Int32``() = 
        assertEval <@ 4 @> 4

    (*
    Value ("Hello World")
    *)
    [<TestMethod>]
    member __.``Evaluate const string``() = 
        assertEval <@ "Hello World" @> "Hello World"

    (*
    Value (true)
    *)
    [<TestMethod>]
    member __.``Evaluate const bool``() = 
        assertEval <@ true @> true

    (*
    Value (0.87)
    *)
    [<TestMethod>]
    member __.``Evaluate const float``() = 
        assertEval <@ 0.87 @> 0.87
 
    (*
    Call (None, MakeDecimal,
      [Value (87), Value (0), Value (0), Value (false), Value (2uy)])
    *)
    [<TestMethod>]
    member __.``Evaluate const decimal``() = 
        assertEval <@ 0.87m @> 0.87m

    (*
    NewUnionCase (None)
    *)
    [<TestMethod>]
    member __.``Evaluate none``() = 
        assertEval <@ None @> None

    (*
    NewUnionCase (Some, Value (4))
    *)
    [<TestMethod>]
    member __.``Evaluate some number``() = 
        assertEval <@ Some 4 @> <| Some 4
    
    (*
    NewUnionCase (Some, Call (None, op_Addition, [Value (6), Value (9)]))
    *)
    [<TestMethod>]
    member __.``Evaluate some simple addition``() = 
        assertEval <@ Some (6 + 9) @> <| Some 15

    (*
    Call (None, Abs, [Value (-3)])
    *)
    [<TestMethod>]
    member __.``Evaluate static method call with one parameter``() = 
        assertEval <@ abs(-3) @> 3

    (*
    Call (None, Ceiling, [Call (None, Abs, [Value (-5.67)])])
    *)
    [<TestMethod>]
    member __.``Evaluate 2 static method calls with one parameter``() = 
        assertEval <@ ceil(abs(-5.67)) @> <| double 6
        
    (*
    Call (None, Max, [Value (10), Value (75)])
    *)
    [<TestMethod>]
    member __.``Evaluate static method call with 2 parameters of same type``() = 
        assertEval <@ max 10 75 @> 75

    (*
    Call (None, Round, [Value (0.1234), Value (2)])
    *)
    [<TestMethod>]
    member __.``Evaluate static method call with 2 parameters of different type``() = 
        assertEval <@ Math.Round(0.1234, 2) @> 0.12
    
    (*
    Call (None, Not, [Value (true)])
    *)
    [<TestMethod>]
    member __.``Evaluate unary operator``() = 
        assertEval <@ not true @> false

    (*
    Call (None, op_Addition, [Value (7), Value (14)])
    *)
    [<TestMethod>]
    member __.``Evaluate binary operator``() = 
        assertEval <@ 7 + 14 @> 21
    
    (*
    Call (None, op_Addition,
      [Value (7), Call (None, op_Multiply, [Value (14), Value (9)])])
    *)
    [<TestMethod>]
    member __.``Evaluate several operators chain``() = 
        assertEval <@ 7 + 14 * 9 @> 133
    
    (*
    NewRecord (Person, Value ("First"), Value ("Last"))
    *)
    [<TestMethod>]
    member __.``Evaluate a record type``() = 
        assertEval <@ { FirstName = "First" ; LastName = "Last" } @> { FirstName = "First" ; LastName = "Last" } 
        
    (*
    NewTuple (Value (16), Value ("String"), Value (true))
    *)
    [<TestMethod>]
    member __.``Evaluate a tuple``() = 
        assertEval <@ (16, "String", true) @> (16, "String", true) 

    (*
    Let (x, Value (3), x)
    *)
    [<TestMethod>]
    member __.``Evaluate a simple let statement``() = 
        assertEval <@ let x = 3 in x @> 3 

    (*
    Let (x, Value (3), Call (None, op_Addition, [x, Value (5)]))
    *)
    [<TestMethod>]
    member __.``Evaluate a let statement with addition``() = 
        assertEval <@ let x = 3 in x + 5 @> 8

    (*
    Call (None, op_PipeRight,
      [Value (3),
       Let (count, Value (5),
            Lambda (value, Call (None, Create, [count, value])))])
    *)
    [<TestMethod>]
    member __.``Evaluate a statement with right pipeline``() = 
        collectionAssertEval <@ 3 |> Array.create 5 @> [| 3 ; 3 ; 3 ; 3 ; 3 |]
        
    (*
    Call (None, op_PipeLeft,
      [Let (count, Value (5),
            Lambda (value, Call (None, Create, [count, value]))), Value (true)])
    *)
    [<TestMethod>]
    member __.``Evaluate a statement with left pipeline``() = 
        collectionAssertEval <@ Array.create 5 <| true @> [| true ; true; true ; true ; true |]
    
    (*
    NewArray (Int32, Value (1), Value (2), Value (3), Value (4), Value (5))
    *)
    [<TestMethod>]
    member __.``Evaluate new array``() = 
        collectionAssertEval <@ [|1;2;3;4;5|] @> [|1;2;3;4;5|]
    
    (*
    NewUnionCase (Cons, Value (1),
              NewUnionCase (Cons, Value (2),
                            NewUnionCase (Cons, Value (3),
                                          NewUnionCase (Cons, Value (4),
                                                        NewUnionCase (Cons, Value (5),
                                                                      NewUnionCase (Empty))))))
    *)
    [<TestMethod>]
    member __.``Evaluate new list``() = 
        listAssertEval <@ [1;2;3;4;5] @> [1;2;3;4;5]

    (*
    Let (x, Value (3), Call (Some (x), ToString, []))
    *)    
    [<TestMethod>]
    member __.``Evaluate instance method call with no parameters``() = 
        assertEval <@ let x = 3 in x.ToString() @> "3"

    (*
    Let (x, Value (1738), Call (Some (x), ToString, [Value ("0,000 USD")]))
    *)
    [<TestMethod>]
    member __.``Evaluate instance method call one parameter``() = 
        assertEval <@ let x = 1738 in x.ToString("0,000 USD") @> "1,738 USD"

    (*
    Let (f, Lambda (x, Call (None, op_Addition, [x, Value (1)])),
     Application (f, Value (3)))
    *)
    [<TestMethod>]
    member __.``Evaluate function application with one parameter``() = 
        assertEval <@ let f x = x + 1 in f 3 @> 4
        
    (*
    Let (f, Lambda (x, Call (None, op_Addition, [x, Value (1)])),
     Application (f, Application (f, Value (3))))
    *)
    [<TestMethod>]
    member __.``Evaluate function application twice with one parameter``() = 
        assertEval <@ let f x = x + 1 in f (f 3) @> 5
    
    (*
    Let (f,
     Lambda (x,
             Lambda (y,
                     Call (None, op_Addition,
                           [Call (None, op_Addition, [x, y]), Value (1)]))),
     Application (Application (f, Value (3)), Value (8)))
    *)
    [<TestMethod>]
    member __.``Evaluate function application with two parameters``() = 
        assertEval <@ let f x y = x + y + 1 in f 3 8 @> 12
          
    (*
    Call (None, ToString, [Value (3)])
    *)
    [<TestMethod>]
    member __.``Evaluate convert int to string``() = 
        assertEval <@ string 3 @> "3"
        
    (*
    Call (None, ToByte, [Value (3)])
    *)
    [<TestMethod>]
    member __.``Evaluate convert int to byte``() = 
        assertEval <@ byte 3 @> 3uy
        
    (*
    Call (None, ToChar, [Value (3)])
    *)
    [<TestMethod>]
    member __.``Evaluate convert int to char``() = 
        assertEval <@ char 51 @> '3'
        
    (*
    Call (None, ToDecimal, [Value (13)])
    *)
    [<TestMethod>]
    member __.``Evaluate convert int to decimal``() = 
        assertEval <@ decimal 13 @> 13m
        
    (*
    Call (None, ToDouble, [Value (12)])
    *)
    [<TestMethod>]
    member __.``Evaluate convert int to float``() = 
        assertEval <@ float 12 @> 12.0
    
    (*
    Call (None, ToSingle, [Value (12)])
    *)
    [<TestMethod>]
    member __.``Evaluate convert int to float32``() = 
        assertEval <@ float32 12 @> 12.0f
        
    (*
    Call (None, ToInt, [Value ("82")])
    *)
    [<TestMethod>]
    member __.``Evaluate convert string to int``() = 
        assertEval <@ int "82" @> 82
        
    (*
    Call (None, ToInt16, [Value ("17")])
    *)
    [<TestMethod>]
    member __.``Evaluate convert string to int16``() = 
        assertEval <@ int16 "17" @> 17s

    (*
    Call (None, ToInt32, [Value ("29")])
    *)
    [<TestMethod>]
    member __.``Evaluate convert string to int32``() = 
        assertEval <@ int32 "29" @> 29
        
    (*
    Call (None, ToInt64, [Value ("18")])
    *)
    [<TestMethod>]
    member __.``Evaluate convert string to int64``() = 
        assertEval <@ int64 "18" @> 18L
         
    (*
    Call (None, ToSByte, [Value (65)])
    *)
    [<TestMethod>]
    member __.``Evaluate convert int to sbyte``() = 
        assertEval <@ sbyte 65 @> 65y
        
    (*
    Call (None, ToUInt16, [Value (28)])
    *)
    [<TestMethod>]
    member __.``Evaluate convert int to uint16``() = 
        assertEval <@ uint16 28 @> (uint16 28)
        
    (*
    Call (None, ToUInt32, [Value (34)])
    *)
    [<TestMethod>]
    member __.``Evaluate convert int to uint32``() = 
        assertEval <@ uint32 34 @> 34u
        
    (*
    Call (None, ToUInt64, [Value (91)])
    *)
    [<TestMethod>]
    member __.``Evaluate convert int to uint64``() = 
        assertEval <@ uint64 91 @> 91UL
           
    (*
    Let (x, Value ("Hello"), Coerce (x, Object))
    *)
    [<TestMethod>]
    member __.``Evaluate cast string to obj``() = 
        assertEval <@ let x = "Hello" in x :> obj @> ("Hello" :> obj)
         
    (*
    Let (child, NewObject (ChildClass, Value ("Hello")),
     Let (base1, Coerce (child, BaseClass), base1))
    *)
    [<TestMethod>]
    member __.``Evaluate upcasting child class to base class``() = 
        assertEval 
            <@ 
            let child = new ChildClass("Hello") 
            let base1 = child :> BaseClass
            base1 @> 
            (new BaseClass("Hello"))

    (*
    Let (child, Coerce (NewObject (ChildClass, Value ("Hello")), Object),
     Let (base1, Call (None, UnboxGeneric, [child]), base1))
    *)
    [<TestMethod>]
    member __.``Evaluate downcasting child class to base class``() = 
        assertEval 
            <@ 
            let child = new ChildClass("Hello") :> obj
            let base1 = child :?> BaseClass 
            base1 @> 
            (new BaseClass("Hello"))

    (*
    Let (x, Call (None, ToEnum, [Value (2)]), x)
    *)
    [<TestMethod>]
    member __.``Evaluate cast int to enum``() = 
        assertEval <@ let x : ConsoleColor = enum 2 in x @> ConsoleColor.DarkGreen
    
    (*
    Let (child, NewObject (ChildClass, Value ("Hello")),
     PropertyGet (Some (child), NameProperty, []))
    *)
    [<TestMethod>]
    member __.``Evaluate get property``() = 
        assertEval 
            <@ 
            let child = new ChildClass("Hello")
            child.NameProperty
            @> "Hello"
    
    (*
    Let (child, NewObject (ChildClass, Value ("Hello")),
     Sequential (PropertySet (Some (child), NameProperty, [Value ("World")]),
                 PropertyGet (Some (child), NameProperty, [])))
    *)
    [<TestMethod>]
    member __.``Evaluate set property``() = 
        assertEval 
            <@ 
            let child = new ChildClass("Hello")
            child.NameProperty <- "World"
            child.NameProperty 
            @> "World"
    
    (*
    Let (indexerClass, NewObject (IndexerClass),
     PropertyGet (Some (indexerClass), Item, [Value (2)]))
    *)
    [<TestMethod>]
    member __.``Evaluate get indexer property``() = 
        assertEval 
            <@ 
            let indexerClass = new IndexerClass()
            indexerClass.[2]
            @> "three"

    (*
    Let (indexerClass, NewObject (IndexerClass),
     Sequential (PropertySet (Some (indexerClass), Item,
                              [Value (2), Value ("Lovely Two")]),
                 PropertyGet (Some (indexerClass), Item, [Value (2)])))
    *)
    [<TestMethod>]
    member __.``Evaluate set indexer property``() = 
        assertEval 
            <@ 
            let indexerClass = new IndexerClass()
            indexerClass.[2] <- "Lovely Two"
            indexerClass.[2]
            @> "Lovely Two"
    
    (*
    Let (struct1, DefaultValue (Struct), struct1)
    *)
    [<TestMethod>]
    member __.``Evaluate struct default value``() = 
        assertEval <@ let struct1 = new Struct() in struct1 @> (new Struct())
    
    (*
    Let (field, NewObject (FieldClass, Value (54)), FieldGet (Some (field), number))
    *)
    [<TestMethod>]
    member __.``Evaluate get field``() = 
        assertEval <@ let field = new FieldClass(54) in field.number @> 54

    (*
    Let (field, NewObject (FieldClass, Value (54)),
     Sequential (FieldSet (Some (field), number, Value (37)),
                 FieldGet (Some (field), number)))
    *)
    [<TestMethod>]
    member __.``Evaluate set field``() = 
        assertEval 
            <@ 
            let field = new FieldClass(54)
            field.number <- 37
            field.number 
            @> 37

    (*
    Let (x, Value (3), Sequential (VarSet (x, Value (4)), x))
    *)
    [<TestMethod>]
    member __.``Evaluate set mutable variable``() = 
        assertEval 
            <@ 
            let mutable x = 3
            x <- 4
            x
            @> 4

    (*
    Let (x, Value (0),
     Sequential (ForIntegerRangeLoop (i, Value (1), Value (10),
                                      VarSet (x,
                                              Call (None, op_Addition, [x, i]))),
                 x))
    *)
    [<TestMethod>]
    member __.``Evaluate for up to loop``() = 
        assertEval 
            <@ 
            let mutable x = 0
            
            for i = 1 to 10 do
                x <- x + i

            x
            @> 55

    (*
    Let (x, Value (0),
     Sequential (ForIntegerRangeLoop (i, Value (8), Value (3),
                                      VarSet (x,
                                              Call (None, op_Addition, [x, i]))),
                 x))
    *)
    [<TestMethod>]
    member __.``Evaluate for down to loop``() = 
        assertEval 
            <@ 
            let mutable x = 0
            
            for i = 8 to 3 do
                x <- x + i

            x
            @> 33

    (*
    Let (x, Value (0),
     Sequential (ForIntegerRangeLoop (i, Value (1), Value (6),
                                      VarSet (x,
                                              Call (None, op_Addition, [x, i]))),
                 x))
    *)
    [<TestMethod>]
    member __.``Evaluate for in loop``() = 
        assertEval 
            <@ 
            let mutable x = 0
            
            for i in 1 .. 6 do
                x <- x + i

            x
            @> 21
    
    (*
    Let (x, Value (6),
     Sequential (IfThenElse (Value (true), VarSet (x, Value (5)), Value (<null>)),
                 x))
    *)
    [<TestMethod>]
    member __.``Evaluate if is true``() = 
        assertEval 
            <@ 
            let mutable x = 6
            
            if true then x <- 5

            x
            @> 5
    
    (*
    Let (x, Value (6),
     Sequential (IfThenElse (Value (false), VarSet (x, Value (5)),
                             Value (<null>)), x))
    *)
    [<TestMethod>]
    member __.``Evaluate if is false``() = 
        assertEval 
            <@ 
            let mutable x = 6
            
            if false then x <- 5

            x
            @> 6

    (*
    IfThenElse (Value (false), Value (5), Value (7))
    *)
    [<TestMethod>]
    member __.``Evaluate if else``() = 
        assertEval <@ if false then 5 else 7 @> 7

    (*
    Let (x, Value (4),
     IfThenElse (Call (None, op_Equality, [x, Value (6)]), Value (1),
                 IfThenElse (Call (None, op_Equality, [x, Value (4)]), Value (9),
                             Value (12))))
    *)
    [<TestMethod>]
    member __.``Evaluate if else if``() = 
        assertEval 
            <@ 
            let x = 4
            
            if x = 6 then 1 elif x = 4 then 9 else 12 
            @> 9

    (*
    Let (patternInput, NewTuple (Value (1), Value (2)),
        Let (a, TupleGet (patternInput, 0), a))
    *)
    [<TestMethod>]
    member __.``Evaluate tuple get``() = 
        assertEval <@ let (a, _) = (1, 2) in a @> 1

    (*
    Let (matchValue, Value (true), IfThenElse (matchValue, Value (1), Value (2)))
    *)
    [<TestMethod>]
    member __.``Evaluate bool pattern matching``() = 
        assertEval 
            <@ 
            match true with 
            | true -> 1
            | false -> 2
            @> 1

    (*
    Let (u, NewUnionCase (UnionB),
     IfThenElse (UnionCaseTest (u, UnionB), Value (2),
                 IfThenElse (UnionCaseTest (u, UnionC), Value (3), Value (1))))
    *)
    [<TestMethod>]
    member __.``Evaluate union pattern matching``() = 
        assertEval 
            <@ 
            let u = UnionB
            match u with
            | UnionA -> 1
            | UnionB -> 2
            | UnionC -> 3
            @> 2

    (*
    Let (x, NewUnionCase (Some, Value (4)),
     IfThenElse (UnionCaseTest (x, None), Value (0),
                 Let (n, PropertyGet (Some (x), Value, []), n)))
    *)
    [<TestMethod>]
    member __.``Evaluate identifier pattern matching``() = 
        assertEval 
            <@ 
            let x = Some 4
            match x with
            | Some n -> n
            | None -> 0
            @> 4

    (*
    Let (u, NewUnionCase (UnionB),
     IfThenElse (UnionCaseTest (u, UnionB), Value (1),
                 IfThenElse (UnionCaseTest (u, UnionC), Value (2), Value (1))))
    *)
    [<TestMethod>]
    member __.``Evaluate or pattern matching``() = 
        assertEval 
            <@ 
            let u = UnionB
            match u with
            | UnionA | UnionB -> 1
            | UnionC -> 2
            @> 1

    (*
    Let (x, Value (5),
     IfThenElse (Call (None, op_Equality, [x, Value (6)]),
                 IfThenElse (Call (None, op_Equality, [x, Value (5)]), Value (1),
                             IfThenElse (Call (None, op_Equality, [x, Value (5)]),
                                         Value (2), Value (0))),
                 IfThenElse (Call (None, op_Equality, [x, Value (5)]), Value (2),
                             Value (0))))
    *)   
    [<TestMethod>]
    member __.``Evaluate and pattern matching``() = 
        assertEval 
            <@ 
            let x = 5
            match x with
            | 6 & 5 -> 1
            | 5 -> 2
            | _ -> 0
            @> 2
            
    (*
    Let (x,
     NewUnionCase (Cons, Value (1),
                   NewUnionCase (Cons, Value (2),
                                 NewUnionCase (Cons, Value (3),
                                               NewUnionCase (Empty)))),
     IfThenElse (UnionCaseTest (x, Cons),
                 Let (tail, PropertyGet (Some (x), Tail, []),
                      Let (head, PropertyGet (Some (x), Head, []), head)),
                 Value (0)))
    *)
    [<TestMethod>]
    member __.``Evaluate cons pattern matching``() = 
        assertEval 
            <@ 
            let x = [1 ; 2 ; 3]
            match x with
            | head :: _ -> head
            | _ -> 0
            @> 1

    
    (*
    Let (x,
     NewUnionCase (Cons, Value (1),
                   NewUnionCase (Cons, Value (2),
                                 NewUnionCase (Cons, Value (3),
                                               NewUnionCase (Empty)))),
     IfThenElse (UnionCaseTest (x, Cons),
                 IfThenElse (UnionCaseTest (PropertyGet (Some (x), Tail, []),
                                            Cons),
                             IfThenElse (UnionCaseTest (PropertyGet (Some (PropertyGet (Some (x),
                                                                                        Tail,
                                                                                        [])),
                                                                     Tail, []),
                                                        Cons),
                                         IfThenElse (UnionCaseTest (PropertyGet (Some (PropertyGet (Some (PropertyGet (Some (x),
                                                                                                                       Tail,
                                                                                                                       [])),
                                                                                                    Tail,
                                                                                                    [])),
                                                                                 Tail,
                                                                                 []),
                                                                    Empty),
                                                     Let (n,
                                                          PropertyGet (Some (PropertyGet (Some (x),
                                                                                          Tail,
                                                                                          [])),
                                                                       Head, []),
                                                          n), Value (0)),
                                         Value (0)), Value (0)), Value (0)))
    *)
    [<TestMethod>]
    member __.``Evaluate list pattern matching``() = 
        assertEval 
            <@ 
            let x = [1 ; 2 ; 3]
            match x with
            | [ _ ; n ; _ ] -> n
            | _ -> 0
            @> 2
    
    (*
    Let (x, NewTuple (Value (7), Value (8)),
     IfThenElse (Call (None, op_Equality, [TupleGet (x, 0), Value (7)]),
                 Let (n, TupleGet (x, 1), n), Value (0)))
    *)
    [<TestMethod>]
    member __.``Evaluate tuple pattern matching``() = 
        assertEval 
            <@ 
            let x = (7, 8)
            match x with
            | (7, n) -> n
            | _ -> 0
            @> 8

    
    (*
    Let (x, NewRecord (Person, Value ("First"), Value ("Last")),
     IfThenElse (Call (None, op_Equality,
                       [PropertyGet (Some (x), FirstName, []), Value ("First")]),
                 Let (name, PropertyGet (Some (x), LastName, []), name),
                 Value ("Empty")))
    *)
    [<TestMethod>]
    member __.``Evaluate record pattern matching``() = 
        assertEval 
            <@ 
            let x = { FirstName = "First" ; LastName = "Last" }
            match x with
            | {FirstName = "First" ; LastName = name } -> name
            | _ -> "Empty"
            @> "Last"

    (*
    Let (x, Coerce (Value (6), Object),
     IfThenElse (TypeTest (Int32, x), Value (true), Value (false)))
    *)
    [<TestMethod>]
    member __.``Evaluate type test pattern matching``() = 
        assertEval 
            <@ 
            let x : obj = 6 :> obj
            match x with
            | :? int -> true
            | _ -> false
            @> true

    (*
    Let (x, NewTuple (Value (10), Value (8)),
        IfThenElse (Let (b, TupleGet (x, 1),
                            Call (None, op_Equality, [b, Value (8)])),
                    Let (b, TupleGet (x, 1), Value (true)), Value (false)))
    *)
    [<TestMethod>]
    member __.``Evaluate pattern matching with condition``() = 
        assertEval 
            <@ 
            let x = (10, 8)
            match x with
            | (_, b) when b = 8 -> true
            | _ -> false
            @> true

    (*
    Let (x, NewRecord (Person, Value ("First"), Value ("Last")),
     Let (LastName, Value ("New Last"),
          NewRecord (Person, PropertyGet (Some (x), FirstName, []), LastName)))
    *)
    [<TestMethod>]
    member __.``Evaluate update record``() = 
        assertEval 
            <@ 
            let x = { FirstName = "First" ; LastName = "Last" }
            {
                x with LastName = "New Last"
            }
            @> { FirstName = "First" ; LastName = "New Last" }

    (*
    Let (tried, Value (false),
     Let (finalized, Value (false),
          Sequential (TryFinally (VarSet (tried, Value (true)),
                                  VarSet (finalized, Value (true))),
                      NewTuple (tried, finalized))))
    *)    
    [<TestMethod>]
    member __.``Evaluate try finally``() = 
        assertEval 
            <@
            let mutable tried = false
            let mutable finalized = false

            try
                tried <- true
            finally
                finalized <- true
                
            (tried , finalized)
            @> (true, true)

    (*
    Let (tried, Value (false),
     Let (caught, Value (false),
          Sequential (TryWith (Sequential (VarSet (tried, Value (true)),
                                           Call (None, Raise,
                                                 [Coerce (NewObject (InvalidOperationException,
                                                                     Value ("Error")),
                                                          Exception)])),
                               matchValue,
                               IfThenElse (TypeTest (InvalidOperationException,
                                                     matchValue), Value (1),
                                           Value (0)), matchValue,
                               IfThenElse (TypeTest (InvalidOperationException,
                                                     matchValue),
                                           VarSet (caught, Value (true)),
                                           Call (None, Reraise, []))),
                      NewTuple (tried, caught))))
    *)
    [<TestMethod>]
    member __.``Evaluate try with catch by type``() = 
        assertEval 
            <@
            let mutable tried = false
            let mutable caught = false

            try
                tried <- true
                raise (InvalidOperationException("Error"))
            with
            | :? InvalidOperationException -> caught <- true
                
            (tried , caught)
            @> (true, true)

    (*
    Let (message, Value (""),
     Sequential (TryWith (Sequential (VarSet (message,
                                              Call (None, op_Addition,
                                                    [message, Value ("Tried,")])),
                                      Call (None, Raise,
                                            [Coerce (NewObject (InvalidOperationException,
                                                                Value ("Error")),
                                                     Exception)])), matchValue,
                          IfThenElse (TypeTest (InvalidOperationException,
                                                matchValue),
                                      Let (ex,
                                           Call (None, UnboxGeneric,
                                                 [matchValue]), Value (1)),
                                      Value (0)), matchValue,
                          IfThenElse (TypeTest (InvalidOperationException,
                                                matchValue),
                                      Let (ex,
                                           Call (None, UnboxGeneric,
                                                 [matchValue]),
                                           VarSet (message,
                                                   Call (None, op_Addition,
                                                         [message,
                                                          PropertyGet (Some (ex),
                                                                       Message,
                                                                       [])]))),
                                      Call (None, Reraise, []))), message))
    *)
    [<TestMethod>]
    member __.``Evaluate try with catch by type and identifier``() = 
        assertEval 
            <@
            let mutable message = ""

            try
                message <- message + "Tried,"
                raise (InvalidOperationException("Error"))
            with
            | :? InvalidOperationException as ex -> message <- message + ex.Message
                
            message
            @> "Tried,Error"
            
    (*
    Let (message, Value (""),
     Sequential (TryWith (Sequential (VarSet (message,
                                              Call (None, op_Addition,
                                                    [message, Value ("Tried,")])),
                                      Call (None, Raise,
                                            [Coerce (NewObject (TestException,
                                                                Value ("Error")),
                                                     Exception)])), matchValue,
                          IfThenElse (TypeTest (TestException, matchValue),
                                      Let (errorMessage,
                                           PropertyGet (Some (Coerce (matchValue,
                                                                      TestException)),
                                                        Data0, []), Value (1)),
                                      Value (0)), matchValue,
                          IfThenElse (TypeTest (TestException, matchValue),
                                      Let (errorMessage,
                                           PropertyGet (Some (Coerce (matchValue,
                                                                      TestException)),
                                                        Data0, []),
                                           VarSet (message,
                                                   Call (None, op_Addition,
                                                         [message, errorMessage]))),
                                      Call (None, Reraise, []))), message))
    *)
    [<TestMethod>]
    member __.``Evaluate try with catch by name``() = 
        assertEval 
            <@
            let mutable message = ""

            try
                message <- message + "Tried,"
                raise (TestException("Error"))
            with
            | TestException (errorMessage) -> message <- message + errorMessage
                
            message
            @> "Tried,Error"
    
    (*
    Let (message, Value (""),
     Sequential (TryWith (Sequential (VarSet (message,
                                              Call (None, op_Addition,
                                                    [message, Value ("Tried,")])),
                                      Call (None, Raise,
                                            [Coerce (NewObject (TestException,
                                                                Value ("Error")),
                                                     Exception)])), ex,
                          Value (1), ex,
                          VarSet (message,
                                  Call (None, op_Addition,
                                        [message,
                                         PropertyGet (Some (ex), Message, [])]))),
                 message))
    *)
    [<TestMethod>]
    member __.``Evaluate try with catch by identifier``() = 
        assertEval 
            <@
            let mutable message = ""

            try
                message <- message + "Tried,"
                raise (TestException("Error"))
            with
            | ex -> message <- message + ex.Message
                
            message
            @> "Tried,Exception of type 'FEval.Tests.TestHelpers+TestException' was thrown."
    
    (*
    Let (message, Value (""),
     Let (b, Value (true),
          Sequential (TryWith (Sequential (VarSet (message,
                                                   Call (None, op_Addition,
                                                         [message,
                                                          Value ("Tried,")])),
                                           Call (None, Raise,
                                                 [Coerce (NewObject (TestException,
                                                                     Value ("Error")),
                                                          Exception)])),
                               matchValue,
                               IfThenElse (Call (None, op_Equality,
                                                 [b, Value (false)]), Value (1),
                                           IfThenElse (Call (None, op_Equality,
                                                             [b, Value (true)]),
                                                       Value (1), Value (0))),
                               matchValue,
                               IfThenElse (Call (None, op_Equality,
                                                 [b, Value (false)]),
                                           VarSet (message,
                                                   Call (None, op_Addition,
                                                         [message,
                                                          Value ("False")])),
                                           IfThenElse (Call (None, op_Equality,
                                                             [b, Value (true)]),
                                                       VarSet (message,
                                                               Call (None,
                                                                     op_Addition,
                                                                     [message,
                                                                      Value ("True")])),
                                                       Call (None, Reraise, [])))),
                      message)))
    *)
    [<TestMethod>]
    member __.``Evaluate try with catch by identifier and condition``() = 
        assertEval 
            <@
            let mutable message = ""
            let b = true

            try
                message <- message + "Tried,"
                raise (TestException("Error"))
            with
            | _ when b = false -> message <- message + "False"
            | _ when b = true -> message <- message + "True"
                            
            message
            @> "Tried,True"
        
    (*
    Let (message, Value (""),
     Sequential (TryWith (TryFinally (Sequential (VarSet (message,
                                                          Call (None,
                                                                op_Addition,
                                                                [message,
                                                                 Value ("Tried,")])),
                                                  Sequential (Call (None, Raise,
                                                                    [Coerce (NewObject (TestException,
                                                                                        Value ("Error")),
                                                                             Exception)]),
                                                              VarSet (message,
                                                                      Call (None,
                                                                            op_Addition,
                                                                            [message,
                                                                             Value ("Shouldnt be here,")])))),
                                      VarSet (message,
                                              Call (None, op_Addition,
                                                    [message, Value ("Finally,")]))),
                          matchValue,
                          IfThenElse (TypeTest (TestException, matchValue),
                                      Value (1), Value (0)), matchValue,
                          IfThenElse (TypeTest (TestException, matchValue),
                                      VarSet (message,
                                              Call (None, op_Addition,
                                                    [message, Value ("Caught,")])),
                                      Call (None, Reraise, []))), message))
    *)
    [<TestMethod>]
    member __.``Evaluate try with finally``() = 
        assertEval 
            <@
            let mutable message = ""

            try
                try
                    message <- message + "Tried,"
                    raise (TestException("Error"))
                    message <- message + "Shouldnt be here,"
                finally
                    message <- message + "Finally,"
            with
            | :? TestException -> message <- message + "Caught"
                            
            message
            @> "Tried,Finally,Caught"

    (*
    Let (x, Value (0),
     Sequential (Let (inputSequence,
                      Call (None, op_RangeStep,
                            [Value (1), Value (2), Value (6)]),
                      Let (enumerator,
                           Call (Some (inputSequence), GetEnumerator, []),
                           TryFinally (WhileLoop (Call (Some (enumerator),
                                                        MoveNext, []),
                                                  Let (i,
                                                       PropertyGet (Some (enumerator),
                                                                    Current, []),
                                                       VarSet (x,
                                                               Call (None,
                                                                     op_Addition,
                                                                     [x, i])))),
                                       IfThenElse (TypeTest (IDisposable,
                                                             Coerce (enumerator,
                                                                     Object)),
                                                   Call (Some (Call (None,
                                                                     UnboxGeneric,
                                                                     [Coerce (enumerator,
                                                                              Object)])),
                                                         Dispose, []),
                                                   Value (<null>))))), x))
    *)
    [<TestMethod>]
    member __.``Evaluate for in loop with 2 skip``() = 
        assertEval 
            <@ 
            let mutable x = 0
            
            for i in 1 .. 2 .. 6 do
                x <- x + i

            x
            @> 9
    
    (*
    Let (x, Value (0),
     Sequential (Let (inputSequence,
                      Call (None, op_RangeStep,
                            [Value (20), Value (-3), Value (0)]),
                      Let (enumerator,
                           Call (Some (inputSequence), GetEnumerator, []),
                           TryFinally (WhileLoop (Call (Some (enumerator),
                                                        MoveNext, []),
                                                  Let (i,
                                                       PropertyGet (Some (enumerator),
                                                                    Current, []),
                                                       VarSet (x,
                                                               Call (None,
                                                                     op_Addition,
                                                                     [x, i])))),
                                       IfThenElse (TypeTest (IDisposable,
                                                             Coerce (enumerator,
                                                                     Object)),
                                                   Call (Some (Call (None,
                                                                     UnboxGeneric,
                                                                     [Coerce (enumerator,
                                                                              Object)])),
                                                         Dispose, []),
                                                   Value (<null>))))), x))
    *)
    [<TestMethod>]
    member __.``Evaluate for in loop with -3 skip``() = 
        assertEval 
            <@ 
            let mutable x = 0
            
            for i in 20 .. -3 .. 0 do
                x <- x + i

            x
            @> 77
    
    (*
    Let (x, Value (0),
     Sequential (WhileLoop (Call (None, op_LessThan, [x, Value (10)]),
                            VarSet (x, Call (None, op_Addition, [x, Value (1)]))),
                 x))
    *)
    [<TestMethod>]
    member __.``Evaluate while loop``() = 
        assertEval 
            <@ 
            let mutable x = 0
            
            while x < 10 do
                x <- x + 1

            x
            @> 10
    
    (*
    Call (None, Raise,
      [Coerce (NewObject (TestException, Value ("Error")), Exception)])
    *)
    [<TestMethod>]
    [<ExpectedException(typeof<TestException>)>]
    member __.``Evaluate raise exception``() = 
        evalWithTry 
            <@ raise (TestException("Error")) @>
            (fun ex -> match ex with
                       | TestException("Error") -> raise ex
                       | _ -> ignore())
                        
    (*
    Call (None, FailWith, [Value ("Error")])
    *)
    [<TestMethod>]
    [<ExpectedException(typeof<Exception>)>]
    member __.``Evaluate failwith exception``() = 
        evalWithTry 
            <@ failwith "Error" @>
            (fun ex -> match ex with
                       | ex when ex.Message = "Error" -> raise ex
                       | _ -> ignore())
    
    (*
    Application (Let (clo1,
                  Call (None, PrintFormatToStringThenFail,
                        [Coerce (NewObject (PrintfFormat`5, Value ("Error %i")),
                                 PrintfFormat`4)]),
                  Lambda (arg10, Application (clo1, arg10))), Value (1))
    *)
    [<TestMethod>]
    [<ExpectedException(typeof<Exception>)>]
    member __.``Evaluate failwithf exception``() = 
        evalWithTry 
            <@ failwithf "Error %i" 1 @>
            (fun ex -> match ex with
                       | ex when ex.Message = "Error 1" -> raise ex
                       | _ -> ignore())

    (*
    Call (None, InvalidArg, [Value ("Arg"), Value ("Error")])
    *)   
    [<TestMethod>]
    [<ExpectedException(typeof<ArgumentException>)>]
    member __.``Evaluate invalidArg exception``() = 
        evalWithTry 
            <@ invalidArg "Arg" "Error" @>
            (fun ex -> match ex with
                       | :? ArgumentException as ex 
                           when ex.ParamName = "Arg" && ex.Message = "Error\r\nParameter name: Arg" -> raise ex
                       | _ -> ignore())

    (*
    Call (None, InvalidOp, [Value ("Error")])
    *)
    [<TestMethod>]
    [<ExpectedException(typeof<InvalidOperationException>)>]
    member __.``Evaluate invalidOp exception``() = 
        evalWithTry 
            <@ invalidOp "Error" @>
            (fun ex -> match ex with
                       | :? InvalidOperationException as ex 
                           when ex.Message = "Error" -> raise ex
                       | _ -> ignore())

    (*
    Sequential (Let (x, NewObject (DisposableClass, Value ("Hello")),
        TryFinally (PropertyGet (Some (x), name, []),
                    IfThenElse (TypeTest (IDisposable,
                                          Coerce (x, Object)),
                                Call (Some (Call (None, UnboxGeneric,
                                                  [Coerce (x, Object)])),
                                      Dispose, []), Value (<null>)))),
                PropertyGet (None, IsDisposed, []))
     *)
    [<TestMethod>]
    member __.``Evaluate use statement``() = 
        assertEval 
            <@ 
            let _ = use x = new DisposableClass("Hello") in x.name
            DisposableClass.IsDisposed
            @> true

    (*
    LetRecursive ([(f,Lambda (n,
                          IfThenElse (Call (None, op_Equality, [n, Value (1)]),
                                      Value (1),
                                      Let (x, n,
                                           Call (None, op_Addition,
                                                 [x,
                                                  Application (f,
                                                               Call (None,
                                                                     op_Subtraction,
                                                                     [x,
                                                                      Value (1)]))])))))],
              Application (f, Value (6)))
    *)            
    [<TestMethod>]
    member __.``Evaluate let recursive function with 1 parameter``() = 
        assertEval 
            <@ 
            let rec f n =
                match n with
                | 1 -> 1
                | x -> x + f (x - 1)
            f 6
            @> 21

    (*
    LetRecursive ([(f,Lambda (n,
                          Lambda (m,
                                  IfThenElse (Call (None, op_Equality,
                                                    [n, Value (1)]), m,
                                              Let (x, n,
                                                   Call (None, op_Addition,
                                                         [m,
                                                          Application (Application (f,
                                                                                    Call (None,
                                                                                          op_Subtraction,
                                                                                          [x,
                                                                                           Value (1)])),
                                                                       m)]))))))],
              Application (Application (f, Value (6)), Value (3)))
    *)
    [<TestMethod>]
    member __.``Evaluate let recursive function with 2 parameters``() = 
        assertEval 
            <@ 
            let rec f n m =
                match n with
                | 1 -> m
                | x -> m + f (x - 1) m
            f 6 3
            @> 18

    (*
    Call (None, op_Subtraction, [Value (15), Value (7)])
    *)
    [<TestMethod>]
    member __.``Evaluate subtraction``() = 
        assertEval <@ 15 - 7 @> 8
    
    (*
    Let (x, Value (7), Call (None, op_UnaryNegation, [x]))
    *)
    [<TestMethod>]
    member __.``Evaluate negation``() = 
        assertEval <@ let x = 7 in -x @> -7
    
    (*
    Let (x, Value (-9), Call (None, op_UnaryPlus, [x]))
    *)
    [<TestMethod>]
    member __.``Evaluate plus``() = 
        assertEval <@ let x = -9 in +x @> -9
    
    (*
    Call (None, op_Multiply, [Value (6), Value (3)])
    *)
    [<TestMethod>]
    member __.``Evaluate multiply``() = 
        assertEval <@ 6 * 3 @> 18

    (*
    Call (None, op_Division, [Value (6), Value (3)])
    *)
    [<TestMethod>]
    member __.``Evaluate division``() = 
        assertEval <@ 6 / 3 @> 2
    
    (*
    Call (None, op_Modulus, [Value (7), Value (3)])
    *)
    [<TestMethod>]
    member __.``Evaluate modulus``() = 
        assertEval <@ 7 % 3 @> 1

    (*
    IfThenElse (Value (true), Value (true), Value (false))
    *)
    [<TestMethod>]
    member __.``Evaluate boolean and (true & true)``() = 
        assertEval <@ true && true @> true

    (*
    IfThenElse (Value (true), Value (false), Value (false))
    *)
    [<TestMethod>]
    member __.``Evaluate boolean and (true & false)``() = 
        assertEval <@ true && false @> false

    (*
    IfThenElse (Value (false), Value (true), Value (false))
    *)
    [<TestMethod>]
    member __.``Evaluate boolean and (false & true)``() = 
        assertEval <@ false && true @> false
        
    (*
    IfThenElse (Value (false), Value (false), Value (false))
    *)
    [<TestMethod>]
    member __.``Evaluate boolean and (false & false)``() = 
        assertEval <@ false && false @> false

    (*
    IfThenElse (Value (true), Value (true), Value (true))
    *)
    [<TestMethod>]
    member __.``Evaluate boolean or (true | true)``() = 
        assertEval <@ true || true @> true

    (*
    IfThenElse (Value (true), Value (true), Value (false))
    *)
    [<TestMethod>]
    member __.``Evaluate boolean or (true | false)``() = 
        assertEval <@ true || false @> true
        
    (*
    IfThenElse (Value (false), Value (true), Value (true))
    *)
    [<TestMethod>]
    member __.``Evaluate boolean or (false | true)``() = 
        assertEval <@ false || true @> true

    (*
    IfThenElse (Value (false), Value (true), Value (false))
    *)
    [<TestMethod>]
    member __.``Evaluate boolean or (false | false)``() = 
        assertEval <@ false || false @> false

    (*
    Call (None, Not, [Value (true)])
    *)
    [<TestMethod>]
    member __.``Evaluate boolean not true``() = 
        assertEval <@ not true @> false

    (*
    Call (None, Not, [Value (false)])
    *)
    [<TestMethod>]
    member __.``Evaluate boolean not false``() = 
        assertEval <@ not false @> true

    (*
    Call (None, op_BitwiseAnd, [Value (1), Value (1)])
    *)
    [<TestMethod>]
    member __.``Evaluate bitwise and (1 & 1)``() = 
        assertEval <@ 1 &&& 1 @> 1
        
    (*
    Call (None, op_BitwiseAnd, [Value (1), Value (0)])
    *)
    [<TestMethod>]
    member __.``Evaluate bitwise and (1 & 0)``() = 
        assertEval <@ 1 &&& 0 @> 0
        
    (*
    Call (None, op_BitwiseAnd, [Value (0), Value (1)])
    *)
    [<TestMethod>]
    member __.``Evaluate bitwise and (0 & 1)``() = 
        assertEval <@ 0 &&& 1 @> 0

    (*
    Call (None, op_BitwiseAnd, [Value (0), Value (0)])
    *)
    [<TestMethod>]
    member __.``Evaluate bitwise and (0 & 0)``() = 
        assertEval <@ 0 &&& 0 @> 0
        
    (*
    Call (None, op_BitwiseOr, [Value (1), Value (1)])
    *)
    [<TestMethod>]
    member __.``Evaluate bitwise or (1 | 1)``() = 
        assertEval <@ 1 ||| 1 @> 1
        
    (*
    Call (None, op_BitwiseOr, [Value (1), Value (0)])
    *)
    [<TestMethod>]
    member __.``Evaluate bitwise or (1 | 0)``() = 
        assertEval <@ 1 ||| 0 @> 1
        
    (*
    Call (None, op_BitwiseOr, [Value (0), Value (1)])
    *)
    [<TestMethod>]
    member __.``Evaluate bitwise or (0 | 1)``() = 
        assertEval <@ 0 ||| 1 @> 1

    (*
    Call (None, op_BitwiseOr, [Value (0), Value (0)])
    *)
    [<TestMethod>]
    member __.``Evaluate bitwise or (0 | 0)``() = 
        assertEval <@ 0 ||| 0 @> 0
        
    (*
    Call (None, op_ExclusiveOr, [Value (1), Value (1)])
    *)
    [<TestMethod>]
    member __.``Evaluate bitwise xor (1 ^ 1)``() = 
        assertEval <@ 1 ^^^ 1 @> 0
        
    (*
    Call (None, op_ExclusiveOr, [Value (1), Value (0)])
    *)
    [<TestMethod>]
    member __.``Evaluate bitwise xor (1 ^ 0)``() = 
        assertEval <@ 1 ^^^ 0 @> 1
        
    (*
    Call (None, op_ExclusiveOr, [Value (0), Value (1)])
    *)
    [<TestMethod>]
    member __.``Evaluate bitwise xor (0 ^ 1)``() = 
        assertEval <@ 0 ^^^ 1 @> 1

    (*
    Call (None, op_ExclusiveOr, [Value (0), Value (0)])
    *)
    [<TestMethod>]
    member __.``Evaluate bitwise xor (0 ^ 0)``() = 
        assertEval <@ 0 ^^^ 0 @> 0
        
    (*
    Call (None, op_LogicalNot, [Value (111uy)])
    *)
    [<TestMethod>]
    member __.``Evaluate bitwise negation``() = 
        assertEval <@ ~~~111uy @> 144uy

    (*
    Call (None, op_LeftShift, [Value (1), Value (3)])
    *)
    [<TestMethod>]
    member __.``Evaluate bitwise left shift``() = 
        assertEval <@ 1 <<< 3 @> 8

    (*
    Call (None, op_RightShift, [Value (64), Value (2)])
    *)
    [<TestMethod>]
    member __.``Evaluate bitwise right shift``() = 
        assertEval <@ 64 >>> 2 @> 16

    (*
    Quote (Value (4))
    *)
    [<TestMethod>]
    member __.``Evaluate typed code quotation``() = 
        assertEval <@ <@ 4 @> @> <@ 4 @>

    (*
    Quote (Value (7))
    *)
    [<TestMethod>]
    member __.``Evaluate raw code quotation``() = 
        assertEval <@ <@@ 7 @@> @> <@@ 7 @@>
    
    (*
    Let (person, NewRecord (Person, Value ("First"), Value ("Last")),
    Let (classWithReflectedDefinition, NewObject (ClassWithReflectedDefinition),
         Call (Some (classWithReflectedDefinition), GetFullName, [person])))
    *)
    (*
    Lambda (__,
        Lambda (person,
                Application (Application (Let (clo1,
                                               Call (None, PrintFormatToString,
                                                     [Coerce (NewObject (PrintfFormat`5,
                                                                         Value ("%s %s")),
                                                              PrintfFormat`4)]),
                                               Lambda (arg10,
                                                       Let (clo2,
                                                            Application (clo1,
                                                                         arg10),
                                                            Lambda (arg20,
                                                                    Application (clo2,
                                                                                 arg20))))),
                                          PropertyGet (Some (person), FirstName,
                                                       [])),
                             PropertyGet (Some (person), LastName, []))))
    *)
    [<TestMethod>]
    member __.``Evaluate reflected definition method with one parameter``() = 
        assertEval 
            <@ 
            let person = { FirstName = "First"; LastName = "Last"}
            let classWithReflectedDefinition = new ClassWithReflectedDefinition()
            classWithReflectedDefinition.GetFullName(person)
            @> 
            "First Last"

    (*
    Let (classWithReflectedDefinition, NewObject (ClassWithReflectedDefinition),
     Call (Some (classWithReflectedDefinition), GetOne, []))
    *)   
    [<TestMethod>]
    member __.``Evaluate reflected definition method with no parameters``() = 
        assertEval 
            <@ 
            let classWithReflectedDefinition = new ClassWithReflectedDefinition()
            classWithReflectedDefinition.GetOne()
            @> 
            1