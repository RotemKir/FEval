﻿namespace FEval.Tests

open FEval.Tests.TestHelpers
open FEval.TypeFormatters
open Microsoft.VisualStudio.TestTools.UnitTesting
open System

[<TestClass>]
type TypeFormattersTests() =
    let mockTypeFormatter (t : Type) = t.Name

    [<TestMethod>]
    member this.``IsOption - type is not option - doesn't match``() = 
        match typeof<int> with
        | IsOption _ -> Assert.Fail("Shouldn't match this")
        | _          -> ignore()

    [<TestMethod>]
    member this.``IsOption - type is option - matches``() = 
        let option = Some 1

        match option.GetType() with
        | IsOption t -> Assert.AreEqual(option.GetType(), t)
        | _          -> Assert.Fail("Shouldn't match this")

    [<TestMethod>]
    member this.``IsTuple - type is not tuple - doesn't match``() = 
        match typeof<int> with
        | IsTuple _ -> Assert.Fail("Shouldn't match this")
        | _         -> ignore()

    [<TestMethod>]
    member this.``IsTuple - type is tuple - matches``() = 
        match typeof<int * bool> with
        | IsTuple t -> Assert.AreEqual(typeof<int * bool>, t)
        | _         -> Assert.Fail("Shouldn't match this")
        
    [<TestMethod>]
    member this.``IsFunction - type is not function - doesn't match``() = 
        match typeof<int> with
        | IsFunction _ -> Assert.Fail("Shouldn't match this")
        | _            -> ignore()

    [<TestMethod>]
    member this.``IsFunction - type is function - matches``() = 
        let func = (fun x -> x + 1)

        match func.GetType() with
        | IsFunction t -> Assert.AreEqual(func.GetType(), t)
        | _            -> Assert.Fail("Shouldn't match this")
    
    [<TestMethod>]
    member this.``IsObject - type is not object - doesn't match``() = 
        match typeof<int> with
        | IsObject _ -> Assert.Fail("Shouldn't match this")
        | _          -> ignore()

    [<TestMethod>]
    member this.``IsObject - type is object - matches``() = 
        match typeof<obj> with
        | IsObject t -> Assert.AreEqual(typeof<obj>, t)
        | _          -> Assert.Fail("Shouldn't match this")

    [<TestMethod>]
    member this.``HasToString - type is object - doesn't match``() = 
        match typeof<obj> with
        | HasToString t -> Assert.Fail("Shouldn't match this")
        | _             -> ignore()
    
    [<TestMethod>]
    member this.``HasToString - type is int - matches``() = 
        match typeof<int> with
        | HasToString t -> Assert.AreEqual(typeof<int>, t)
        | _             -> Assert.Fail("Shouldn't match this")

    [<TestMethod>]
    member this.``HasToString - type is tuple - matches``() = 
        match typeof<int * bool> with
        | HasToString t -> Assert.AreEqual(typeof<int * bool>, t)
        | _             -> Assert.Fail("Shouldn't match this")

    [<TestMethod>]
    member this.``HasToString - type is record - matches``() = 
        match typeof<Person> with
        | HasToString t -> Assert.AreEqual(typeof<Person>, t)
        | _             -> Assert.Fail("Shouldn't match this")
        
    [<TestMethod>]
    member this.``HasToString - type is union - matches``() = 
        match typeof<Union> with
        | HasToString t -> Assert.AreEqual(typeof<Union>, t)
        | _             -> Assert.Fail("Shouldn't match this")
          
    [<TestMethod>]
    member this.``HasToString - type is class with ToString - matches``() = 
        match typeof<ClassWithTostring> with
        | HasToString t -> Assert.AreEqual(typeof<ClassWithTostring>, t)
        | _             -> Assert.Fail("Shouldn't match this")
        
    [<TestMethod>]
    member this.``HasToString - type is class that inherits ToString from base - matches``() = 
        match typeof<InheritsClassWithTostring> with
        | HasToString t -> Assert.AreEqual(typeof<InheritsClassWithTostring>, t)
        | _             -> Assert.Fail("Shouldn't match this")
        
    [<TestMethod>]
    member this.``HasToString - type is class that doesn't override ToString - doesn't match``() = 
        match typeof<BaseClass> with
        | HasToString t -> Assert.Fail("Shouldn't match this")
        | _             -> ignore()
            
    [<TestMethod>]
    member this.``formatTypes - empty types array - returns empty string``() = 
        let result = formatTypes [||] ", " mockTypeFormatter
        Assert.AreEqual(String.Empty, result)
        
    [<TestMethod>]
    member this.``formatTypes - one type - returns type name``() = 
        let result = formatTypes [|typeof<int>|] ", " mockTypeFormatter
        Assert.AreEqual("Int32", result)
        
    [<TestMethod>]
    member this.``formatTypes - several types - returns type names with seperator``() = 
        let result = formatTypes [|typeof<int> ; typeof<string> ; typeof<bool>|] ", " mockTypeFormatter
        Assert.AreEqual("Int32, String, Boolean", result)
    
    [<TestMethod>]
    member this.``formatGenericTypeArguments - type has no generic arguments - returns empty string``() = 
        let result = formatGenericTypeArguments typeof<int>  ", " mockTypeFormatter
        Assert.AreEqual(String.Empty, result)
        
    [<TestMethod>]
    member this.``formatGenericTypeArguments - type has one generic argument - returns type name``() = 
        let result = formatGenericTypeArguments typeof<Action<int>>  ", " mockTypeFormatter
        Assert.AreEqual("Int32", result)
        
    [<TestMethod>]
    member this.``formatGenericTypeArguments - type has several generic arguments - returns type names with seperator``() = 
        let result = formatGenericTypeArguments typeof<Action<int, string, bool>>  ", " mockTypeFormatter
        Assert.AreEqual("Int32, String, Boolean", result)

    [<TestMethod>]
    member this.``formatTupleType - tuple has several types - returns type names with seperator``() = 
        let result = formatTupleType <| typeof<int * bool * string> <| mockTypeFormatter
        Assert.AreEqual("(Int32, Boolean, String)", result)
        
    [<TestMethod>]
    member this.``formatFunctionType - unit to unit - returns Unit -> Unit``() = 
        let result = formatFunctionType <| typeof<unit -> unit> <| mockTypeFormatter
        Assert.AreEqual("(Unit -> Unit)", result)
          
    [<TestMethod>]
    member this.``formatFunctionType - unit to int - returns (Unit -> Int32)``() = 
        let result = formatFunctionType <| typeof<unit -> int>  <| mockTypeFormatter
        Assert.AreEqual("(Unit -> Int32)", result)

    [<TestMethod>]
    member this.``formatFunctionType - int to unit - returns (Int32 -> Unit)``() = 
        let result = formatFunctionType <| typeof<int -> unit> <| mockTypeFormatter
        Assert.AreEqual("(Int32 -> Unit)", result)
        
    [<TestMethod>]
    member this.``formatFunctionType - int to int - returns (Int32 -> Int32)``() = 
        let result = formatFunctionType <| typeof<int -> int> <| mockTypeFormatter
        Assert.AreEqual("(Int32 -> Int32)", result)
        
    [<TestMethod>]
    member this.``formatFunctionType - int to string to bool - returns (Int32 -> String -> Boolean)``() = 
        let func = (fun (i : int32) (s : string) -> true)
        let result = formatFunctionType <| func.GetType().BaseType <| mockTypeFormatter
        Assert.AreEqual("(Int32 -> String -> Boolean)", result)
        
    [<TestMethod>]
    member this.``formatType - function int to int - returns (Int32 -> Int32)``() = 
        let result = formatType typeof<int -> int>
        Assert.AreEqual("(Int32 -> Int32)", result)
        
    [<TestMethod>]
    member this.``formatType - function int to bool to string - returns (Int32 -> (Boolean -> String))``() = 
        let result = formatType typeof<int -> bool -> string>
        Assert.AreEqual("(Int32 -> (Boolean -> String))", result)
        
    [<TestMethod>]
    member this.``formatType - tuple int, string - returns (Int32, String)``() = 
        let result = formatType typeof<int * bool>
        Assert.AreEqual("(Int32, Boolean)", result)
        
    [<TestMethod>]
    member this.``formatType - int option - returns Option<Int32>``() = 
        let result = formatType typeof<int option>
        Assert.AreEqual("Option<Int32>", result)
        
    [<TestMethod>]
    member this.``formatType - union - returns Union``() = 
        let result = formatType typeof<Union>
        Assert.AreEqual("Union", result)
        
    [<TestMethod>]
    member this.``formatType - Person record - returns Person``() = 
        let result = formatType typeof<Person>
        Assert.AreEqual("Person", result)
        
    [<TestMethod>]
    member this.``formatType - class - returns class name``() = 
        let result = formatType typeof<ChildClass>
        Assert.AreEqual("ChildClass", result)
    
    [<TestMethod>]
    member this.``formatType - tuple int, ChildClass - returns (Int32, ChildClass)``() = 
        let result = formatType typeof<int * ChildClass>
        Assert.AreEqual("(Int32, ChildClass)", result)

    [<TestMethod>]
    member this.``formatType - tuple int, bool option - returns (Int32, Option<Boolean>)``() = 
        let result = formatType typeof<int * bool option>
        Assert.AreEqual("(Int32, Option<Boolean>)", result)
        
    [<TestMethod>]
    member this.``formatType - tuple int, func string to bool - returns (Int32, String -> Boolean)``() = 
        let result = formatType typeof<int * (string -> bool)>
        Assert.AreEqual("(Int32, (String -> Boolean))", result)
        
    [<TestMethod>]
    member this.``formatType - func int to ChildClass - returns (Int32 -> ChildClass)``() = 
        let result = formatType typeof<int -> ChildClass>
        Assert.AreEqual("(Int32 -> ChildClass)", result)
        
    [<TestMethod>]
    member this.``formatType - func int to bool option - returns (Int32 -> Option<Boolean>)``() = 
        let result = formatType typeof<int -> bool option>
        Assert.AreEqual("(Int32 -> Option<Boolean>)", result)
        
    [<TestMethod>]
    member this.``formatType - func int to tuple int, string - returns (Int32 -> (Int32, String))``() = 
        let result = formatType typeof<int -> (int * string)>
        Assert.AreEqual("(Int32 -> (Int32, String))", result)
        
    [<TestMethod>]
    member this.``formatType - int array - returns Int32[]``() = 
        let result = formatType typeof<int array>
        Assert.AreEqual("Int32[]", result)
        
    [<TestMethod>]
    member this.``formatType - int list - returns List<Int32>``() = 
        let result = formatType typeof<int list>
        Assert.AreEqual("List<Int32>", result)
        
    [<TestMethod>]
    member this.``formatType - Map<string, int> - returns Map<String, Int32>``() = 
        let result = formatType typeof<Map<string, int>>
        Assert.AreEqual("Map<String, Int32>", result)
        
    [<TestMethod>]
    member this.``formatType - Map<string, int list> - returns Map<String, List<Int32>>``() = 
        let result = formatType typeof<Map<string, int list>>
        Assert.AreEqual("Map<String, List<Int32>>", result)