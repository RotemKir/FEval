namespace FEval.Tests

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
    member this.``Evaluate Const Int32``() = 
        assertEval <@ 4 @> 4

    (*
    Value ("Hello World")
    *)
    [<TestMethod>]
    member this.``Evaluate Const string``() = 
        assertEval <@ "Hello World" @> "Hello World"

    (*
    Value (true)
    *)
    [<TestMethod>]
    member this.``Evaluate Const bool``() = 
        assertEval <@ true @> true

    (*
    Value (0.87)
    *)
    [<TestMethod>]
    member this.``Evaluate Const float``() = 
        assertEval <@ 0.87 @> 0.87
 
    (*
    Call (None, MakeDecimal,
      [Value (87), Value (0), Value (0), Value (false), Value (2uy)])
    *)
    [<TestMethod>]
    member this.``Evaluate Const decimal``() = 
        assertEval <@ 0.87m @> 0.87m

    (*
    NewUnionCase (None)
    *)
    [<TestMethod>]
    member this.``Evaluate None``() = 
        assertEval <@ None @> None

    (*
    NewUnionCase (Some, Value (4))
    *)
    [<TestMethod>]
    member this.``Evaluate Some number``() = 
        assertEval <@ Some 4 @> <| Some 4
    
    (*
    NewUnionCase (Some, Call (None, op_Addition, [Value (6), Value (9)]))
    *)
    [<TestMethod>]
    member this.``Evaluate Some simple addition``() = 
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


    
