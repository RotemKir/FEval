namespace FEval.Tests

open FEval.Tests.TestHelpers
open FEval.TypeChecks
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type TypeChecksTests() =

    [<TestMethod>]
    member __.``IsOption - type is not option - doesn't match``() = 
        match typeof<int> with
        | IsOption _ -> Assert.Fail("Shouldn't match this")
        | _          -> ignore()

    [<TestMethod>]
    member __.``IsOption - type is option - matches``() = 
        let option = Some 1

        match option.GetType() with
        | IsOption t -> Assert.AreEqual(option.GetType(), t)
        | _          -> Assert.Fail("Shouldn't match this")

    [<TestMethod>]
    member __.``IsTuple - type is not tuple - doesn't match``() = 
        match typeof<int> with
        | IsTuple _ -> Assert.Fail("Shouldn't match this")
        | _         -> ignore()

    [<TestMethod>]
    member __.``IsTuple - type is tuple - matches``() = 
        match typeof<int * bool> with
        | IsTuple t -> Assert.AreEqual(typeof<int * bool>, t)
        | _         -> Assert.Fail("Shouldn't match this")
        
    [<TestMethod>]
    member __.``IsFunction - type is not function - doesn't match``() = 
        match typeof<int> with
        | IsFunction _ -> Assert.Fail("Shouldn't match this")
        | _            -> ignore()

    [<TestMethod>]
    member __.``IsFunction - type is function - matches``() = 
        let func = (fun x -> x + 1)

        match func.GetType() with
        | IsFunction t -> Assert.AreEqual(func.GetType(), t)
        | _            -> Assert.Fail("Shouldn't match this")
    
    [<TestMethod>]
    member __.``IsObject - type is not object - doesn't match``() = 
        match typeof<int> with
        | IsObject _ -> Assert.Fail("Shouldn't match this")
        | _          -> ignore()

    [<TestMethod>]
    member __.``IsObject - type is object - matches``() = 
        match typeof<obj> with
        | IsObject t -> Assert.AreEqual(typeof<obj>, t)
        | _          -> Assert.Fail("Shouldn't match this")

    [<TestMethod>]
    member __.``IsGenericType - type is not generic - doesn't match``() = 
        match typeof<int> with
        | IsGenericType _ -> Assert.Fail("Shouldn't match this")
        | _               -> ignore()

    [<TestMethod>]
    member __.``IsGenericType - type is generic - matches``() = 
        match typeof<int list> with
        | IsGenericType t -> Assert.AreEqual(typeof<int list>, t)
        | _               -> Assert.Fail("Shouldn't match this")

    [<TestMethod>]
    member __.``HasToString - type is object - doesn't match``() = 
        match typeof<obj> with
        | HasToString _ -> Assert.Fail("Shouldn't match this")
        | _             -> ignore()
    
    [<TestMethod>]
    member __.``HasToString - type is int - matches``() = 
        match typeof<int> with
        | HasToString t -> Assert.AreEqual(typeof<int>, t)
        | _             -> Assert.Fail("Shouldn't match this")

    [<TestMethod>]
    member __.``HasToString - type is tuple - matches``() = 
        match typeof<int * bool> with
        | HasToString t -> Assert.AreEqual(typeof<int * bool>, t)
        | _             -> Assert.Fail("Shouldn't match this")

    [<TestMethod>]
    member __.``HasToString - type is record - matches``() = 
        match typeof<Person> with
        | HasToString t -> Assert.AreEqual(typeof<Person>, t)
        | _             -> Assert.Fail("Shouldn't match this")
        
    [<TestMethod>]
    member __.``HasToString - type is union - matches``() = 
        match typeof<Union> with
        | HasToString t -> Assert.AreEqual(typeof<Union>, t)
        | _             -> Assert.Fail("Shouldn't match this")
          
    [<TestMethod>]
    member __.``HasToString - type is class with ToString - matches``() = 
        match typeof<ClassWithToString> with
        | HasToString t -> Assert.AreEqual(typeof<ClassWithToString>, t)
        | _             -> Assert.Fail("Shouldn't match this")
        
    [<TestMethod>]
    member __.``HasToString - type is class that inherits ToString from base - matches``() = 
        match typeof<InheritsClassWithTostring> with
        | HasToString t -> Assert.AreEqual(typeof<InheritsClassWithTostring>, t)
        | _             -> Assert.Fail("Shouldn't match this")
        
    [<TestMethod>]
    member __.``HasToString - type is class that doesn't override ToString - doesn't match``() = 
        match typeof<BaseClass> with
        | HasToString _ -> Assert.Fail("Shouldn't match this")
        | _             -> ignore()
        
    [<TestMethod>]
    member __.``HasToString - type is struct - doesn't match``() = 
        match typeof<Struct> with
        | HasToString _ -> Assert.Fail("Shouldn't match this")
        | _             -> ignore()

    [<TestMethod>]
    member __.``HasToString - type is struct with ToString - matches``() = 
        match typeof<StructWithToString> with
        | HasToString t -> Assert.AreEqual(typeof<StructWithToString>, t)
        | _             -> Assert.Fail("Shouldn't match this")
    
    [<TestMethod>]
    member __.``IsIEnumerable - type is sequence - matches``() = 
        let sequence = seq {1 .. 10}
        match sequence.GetType() with
        | IsIEnumerable sequence t -> Assert.AreEqual(sequence, t)
        | _                        -> Assert.Fail("Shouldn't match this")
    
    [<TestMethod>]
    member __.``IsIEnumerable - type is array - matches``() = 
        let array = [| 1 ; 2 ; 3|]
        match array.GetType() with
        | IsIEnumerable array t -> Assert.AreEqual(array, t)
        | _                     -> Assert.Fail("Shouldn't match this")
    
    [<TestMethod>]
    member __.``IsIEnumerable - type is list - matches``() = 
        let list = [1 ; 2 ; 3]
        match list.GetType() with
        | IsIEnumerable list t -> Assert.AreEqual(list, t)
        | _                    -> Assert.Fail("Shouldn't match this")
        
    [<TestMethod>]
    member __.``IsIEnumerable - type is map - matches``() = 
        let map = new Map<string, int> [||]
        match map.GetType() with
        | IsIEnumerable map t -> Assert.AreEqual(map, t)
        | _                   -> Assert.Fail("Shouldn't match this")
        
    [<TestMethod>]
    member __.``IsIEnumerable - type is int - doesn't match``() = 
        let number = 3
        match number.GetType() with
        | IsIEnumerable number _ -> Assert.Fail("Shouldn't match this")
        | _                      -> ignore()