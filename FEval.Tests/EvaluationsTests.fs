﻿namespace FEval.Tests

open System
open FEval.Evaluations
open FEval.Tests.TestHelpers
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type EvaluationsTest() =
    let assertEval expr expectedResult =
        Assert.AreEqual(expectedResult, eval expr)

    let collectionAssertEval expr expectedResult =
        CollectionAssert.AreEqual(expectedResult, eval expr)

    let listAssertEval expr expectedResult =
        CollectionAssert.AreEqual(expectedResult |> List.toArray, eval expr |> List.toArray)

    (*
    Value (4)
    *)
    [<TestMethod>]
    member this.``Evaluate const Int32``() = 
        assertEval <@ 4 @> 4

    (*
    Value ("Hello World")
    *)
    [<TestMethod>]
    member this.``Evaluate const string``() = 
        assertEval <@ "Hello World" @> "Hello World"

    (*
    Value (true)
    *)
    [<TestMethod>]
    member this.``Evaluate const bool``() = 
        assertEval <@ true @> true

    (*
    Value (0.87)
    *)
    [<TestMethod>]
    member this.``Evaluate const float``() = 
        assertEval <@ 0.87 @> 0.87
 
    (*
    Call (None, MakeDecimal,
      [Value (87), Value (0), Value (0), Value (false), Value (2uy)])
    *)
    [<TestMethod>]
    member this.``Evaluate const decimal``() = 
        assertEval <@ 0.87m @> 0.87m

    (*
    NewUnionCase (None)
    *)
    [<TestMethod>]
    member this.``Evaluate none``() = 
        assertEval <@ None @> None

    (*
    NewUnionCase (Some, Value (4))
    *)
    [<TestMethod>]
    member this.``Evaluate some number``() = 
        assertEval <@ Some 4 @> <| Some 4
    
    (*
    NewUnionCase (Some, Call (None, op_Addition, [Value (6), Value (9)]))
    *)
    [<TestMethod>]
    member this.``Evaluate some simple addition``() = 
        assertEval <@ Some (6 + 9) @> <| Some 15

    (*
    Call (None, Abs, [Value (-3)])
    *)
    [<TestMethod>]
    member this.``Evaluate static method call with one parameter``() = 
        assertEval <@ abs(-3) @> 3

    (*
    Call (None, Ceiling, [Call (None, Abs, [Value (-5.67)])])
    *)
    [<TestMethod>]
    member this.``Evaluate 2 static method calls with one parameter``() = 
        assertEval <@ ceil(abs(-5.67)) @> <| double 6
        
    (*
    Call (None, Max, [Value (10), Value (75)])
    *)
    [<TestMethod>]
    member this.``Evaluate static method call with 2 parameters of same type``() = 
        assertEval <@ max 10 75 @> 75

    (*
    Call (None, Round, [Value (0.1234), Value (2)])
    *)
    [<TestMethod>]
    member this.``Evaluate static method call with 2 parameters of different type``() = 
        assertEval <@ Math.Round(0.1234, 2) @> 0.12
    
    (*
    Call (None, Not, [Value (true)])
    *)
    [<TestMethod>]
    member this.``Evaluate unary operator``() = 
        assertEval <@ not true @> false

    (*
    Call (None, op_Addition, [Value (7), Value (14)])
    *)
    [<TestMethod>]
    member this.``Evaluate binary operator``() = 
        assertEval <@ 7 + 14 @> 21
    
    (*
    Call (None, op_Addition,
      [Value (7), Call (None, op_Multiply, [Value (14), Value (9)])])
    *)
    [<TestMethod>]
    member this.``Evaluate several operators chain``() = 
        assertEval <@ 7 + 14 * 9 @> 133
    
    (*
    NewRecord (Person, Value ("First"), Value ("Last"))
    *)
    [<TestMethod>]
    member this.``Evaluate a record type``() = 
        assertEval <@ { FirstName = "First" ; LastName = "Last" } @> { FirstName = "First" ; LastName = "Last" } 
        
    (*
    NewTuple (Value (16), Value ("String"), Value (true))
    *)
    [<TestMethod>]
    member this.``Evaluate a tuple``() = 
        assertEval <@ (16, "String", true) @> (16, "String", true) 

    (*
    Let (x, Value (3), x)
    *)
    [<TestMethod>]
    member this.``Evaluate a simple let statement``() = 
        assertEval <@ let x = 3 in x @> 3 

    (*
    Let (x, Value (3), Call (None, op_Addition, [x, Value (5)]))
    *)
    [<TestMethod>]
    member this.``Evaluate a let statement with addition``() = 
        assertEval <@ let x = 3 in x + 5 @> 8

    (*
    Call (None, op_PipeRight,
      [Value (3),
       Let (count, Value (5),
            Lambda (value, Call (None, Create, [count, value])))])
    *)
    [<TestMethod>]
    member this.``Evaluate a statement with right pipeline``() = 
        collectionAssertEval <@ 3 |> Array.create 5 @> [| 3 ; 3 ; 3 ; 3 ; 3 |]
        
    (*
    Call (None, op_PipeLeft,
      [Let (count, Value (5),
            Lambda (value, Call (None, Create, [count, value]))), Value (true)])
    *)
    [<TestMethod>]
    member this.``Evaluate a statement with left pipeline``() = 
        collectionAssertEval <@ Array.create 5 <| true @> [| true ; true; true ; true ; true |]
    
    (*
    NewArray (Int32, Value (1), Value (2), Value (3), Value (4), Value (5))
    *)
    [<TestMethod>]
    member this.``Evaluate new array``() = 
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
    member this.``Evaluate new list``() = 
        listAssertEval <@ [1;2;3;4;5] @> [1;2;3;4;5]

    (*
    Let (x, Value (3), Call (Some (x), ToString, []))
    *)    
    [<TestMethod>]
    member this.``Evaluate instance method call with no parameters``() = 
        assertEval <@ let x = 3 in x.ToString() @> "3"

    (*
    Let (x, Value (1738), Call (Some (x), ToString, [Value ("0,000 USD")]))
    *)
    [<TestMethod>]
    member this.``Evaluate instance method call one parameter``() = 
        assertEval <@ let x = 1738 in x.ToString("0,000 USD") @> "1,738 USD"

    (*
    Let (f, Lambda (x, Call (None, op_Addition, [x, Value (1)])),
     Application (f, Value (3)))
    *)
    [<TestMethod>]
    member this.``Evaluate function application with one parameter``() = 
        assertEval <@ let f x = x + 1 in f 3 @> 4
        
    (*
    Let (f, Lambda (x, Call (None, op_Addition, [x, Value (1)])),
     Application (f, Application (f, Value (3))))
    *)
    [<TestMethod>]
    member this.``Evaluate function application twice with one parameter``() = 
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
    member this.``Evaluate function application with two parameters``() = 
        assertEval <@ let f x y = x + y + 1 in f 3 8 @> 12
          
    (*
    Call (None, ToString, [Value (3)])
    *)
    [<TestMethod>]
    member this.``Evaluate convert int to string``() = 
        assertEval <@ string 3 @> "3"
        
    (*
    Call (None, ToByte, [Value (3)])
    *)
    [<TestMethod>]
    member this.``Evaluate convert int to byte``() = 
        assertEval <@ byte 3 @> 3uy
        
    (*
    Call (None, ToChar, [Value (3)])
    *)
    [<TestMethod>]
    member this.``Evaluate convert int to char``() = 
        assertEval <@ char 51 @> '3'
        
    (*
    Call (None, ToDecimal, [Value (13)])
    *)
    [<TestMethod>]
    member this.``Evaluate convert int to decimal``() = 
        assertEval <@ decimal 13 @> 13m
        
    (*
    Call (None, ToDouble, [Value (12)])
    *)
    [<TestMethod>]
    member this.``Evaluate convert int to float``() = 
        assertEval <@ float 12 @> 12.0
    
    (*
    Call (None, ToSingle, [Value (12)])
    *)
    [<TestMethod>]
    member this.``Evaluate convert int to float32``() = 
        assertEval <@ float32 12 @> 12.0f
        
    (*
    Call (None, ToInt, [Value ("82")])
    *)
    [<TestMethod>]
    member this.``Evaluate convert string to int``() = 
        assertEval <@ int "82" @> 82
        
    (*
    Call (None, ToInt16, [Value ("17")])
    *)
    [<TestMethod>]
    member this.``Evaluate convert string to int16``() = 
        assertEval <@ int16 "17" @> 17s

    (*
    Call (None, ToInt32, [Value ("29")])
    *)
    [<TestMethod>]
    member this.``Evaluate convert string to int32``() = 
        assertEval <@ int32 "29" @> 29
        
    (*
    Call (None, ToInt64, [Value ("18")])
    *)
    [<TestMethod>]
    member this.``Evaluate convert string to int64``() = 
        assertEval <@ int64 "18" @> 18L
         
    (*
    Call (None, ToSByte, [Value (65)])
    *)
    [<TestMethod>]
    member this.``Evaluate convert int to sbyte``() = 
        assertEval <@ sbyte 65 @> 65y
        
    (*
    Call (None, ToUInt16, [Value (28)])
    *)
    [<TestMethod>]
    member this.``Evaluate convert int to uint16``() = 
        assertEval <@ uint16 28 @> (uint16 28)
        
    (*
    Call (None, ToUInt32, [Value (34)])
    *)
    [<TestMethod>]
    member this.``Evaluate convert int to uint32``() = 
        assertEval <@ uint32 34 @> 34u
        
    (*
    Call (None, ToUInt64, [Value (91)])
    *)
    [<TestMethod>]
    member this.``Evaluate convert int to uint64``() = 
        assertEval <@ uint64 91 @> 91UL
           
    (*
    Let (x, Value ("Hello"), Coerce (x, Object))
    *)
    [<TestMethod>]
    member this.``Evaluate cast string to obj``() = 
        assertEval <@ let x = "Hello" in x :> obj @> ("Hello" :> obj)
         
    (*
    Let (child, NewObject (ChildClass, Value ("Hello")),
     Let (base1, Coerce (child, BaseClass), base1))
    *)
    [<TestMethod>]
    member this.``Evaluate upcasting child class to base class``() = 
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
    member this.``Evaluate downcasting child class to base class``() = 
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
    member this.``Evaluate cast int to enum``() = 
        assertEval <@ let x : ConsoleColor = enum 2 in x @> ConsoleColor.DarkGreen
    
    (*
    Let (child, NewObject (ChildClass, Value ("Hello")),
     PropertyGet (Some (child), NameProperty, []))
    *)
    [<TestMethod>]
    member this.``Evaluate get property``() = 
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
    member this.``Evaluate set property``() = 
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
    member this.``Evaluate get indexer property``() = 
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
    member this.``Evaluate set indexer property``() = 
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
    member this.``Evaluate struct default value``() = 
        assertEval <@ let struct1 = new Struct() in struct1 @> (new Struct())
    
    (*
    Let (field, NewObject (FieldClass, Value (54)), FieldGet (Some (field), number))
    *)
    [<TestMethod>]
    member this.``Evaluate get field``() = 
        assertEval <@ let field = new FieldClass(54) in field.number @> 54

    (*
    Let (field, NewObject (FieldClass, Value (54)),
     Sequential (FieldSet (Some (field), number, Value (37)),
                 FieldGet (Some (field), number)))
    *)
    [<TestMethod>]
    member this.``Evaluate set field``() = 
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
    member this.``Evaluate set mutable variable``() = 
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
    member this.``Evaluate for up to loop``() = 
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
    member this.``Evaluate for down to loop``() = 
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
    member this.``Evaluate for in loop``() = 
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
    member this.``Evaluate if is true``() = 
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
    member this.``Evaluate if is false``() = 
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
    member this.``Evaluate if else``() = 
        assertEval <@ if false then 5 else 7 @> 7

    (*
    Let (x, Value (4),
     IfThenElse (Call (None, op_Equality, [x, Value (6)]), Value (1),
                 IfThenElse (Call (None, op_Equality, [x, Value (4)]), Value (9),
                             Value (12))))
    *)
    [<TestMethod>]
    member this.``Evaluate if else if``() = 
        assertEval 
            <@ 
            let x = 4
            
            if x = 6 then 1 elif x = 4 then 9 else 12 
            @> 9

    (*
    Let (patternInput, NewTuple (Value (1), Value (2)),
     Let (b, TupleGet (patternInput, 1), Let (a, TupleGet (patternInput, 0), a)))
    *)
    [<TestMethod>]
    member this.``Evaluate tuple get``() = 
        assertEval <@ let (a, b) = (1, 2) in a @> 1

    (*
    Let (matchValue, Value (true), IfThenElse (matchValue, Value (1), Value (2)))
    *)
    [<TestMethod>]
    member this.``Evaluate bool pattern matching``() = 
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
    member this.``Evaluate union pattern matching``() = 
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
    member this.``Evaluate identifier pattern matching``() = 
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
    member this.``Evaluate or pattern matching``() = 
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
    member this.``Evaluate and pattern matching``() = 
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
    member this.``Evaluate cons pattern matching``() = 
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
    member this.``Evaluate list pattern matching``() = 
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
    member this.``Evaluate tuple pattern matching``() = 
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
    member this.``Evaluate record pattern matching``() = 
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
    member this.``Evaluate type test pattern matching``() = 
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
                      Let (a, TupleGet (x, 0),
                           Call (None, op_Equality, [b, Value (8)]))),
                 Let (b, TupleGet (x, 1), Let (a, TupleGet (x, 0), Value (true))),
                 Value (false)))
    *)
    [<TestMethod>]
    member this.``Evaluate pattern matching with condition``() = 
        assertEval 
            <@ 
            let x = (10, 8)
            match x with
            | (a, b) when b = 8 -> true
            | _ -> false
            @> true

    (*
    Let (x, NewRecord (Person, Value ("First"), Value ("Last")),
     Let (LastName, Value ("New Last"),
          NewRecord (Person, PropertyGet (Some (x), FirstName, []), LastName)))
    *)
    [<TestMethod>]
    member this.``Evaluate update record``() = 
        assertEval 
            <@ 
            let x = { FirstName = "First" ; LastName = "Last" }
            {
                x with LastName = "New Last"
            }
            @> { FirstName = "First" ; LastName = "New Last" }


