namespace FEval.Tests

module TestHelpers =
    open System
    open System.Collections.Generic
    open Microsoft.FSharp.Quotations
    open Microsoft.VisualStudio.TestTools.UnitTesting
    open FEval.Evaluations

    let createValueExpr<'a>() =
        Some <| Expr.Value (null, typeof<'a>)

    let assertMessages (expected : string array) (actual : List<string>) =
        Assert.AreEqual(expected.Length, actual.Count)
        Array.iteri (fun i s -> StringAssert.Contains(actual.[i], s, sprintf "Item %i" i)) expected

    let assertInspectors expr createInspectors expectedMessages =
        let messageList = new List<string>()
        evalWith expr <| createInspectors messageList |> ignore
        assertMessages expectedMessages messageList

    let assertInspectorsWithException expr createInspectors expectedMessages expectedException =
        let messageList = new List<string>()
        
        try
            evalWith expr <| createInspectors messageList |> ignore
        with
        | ex -> Assert.AreEqual(expectedException, ex.Message)
        
        assertMessages expectedMessages messageList

    type Person =
        {
            FirstName : string
            LastName : string
        }

    type BaseClass(name : string) =
        member __.name = name
        member val NameProperty = name with get, set

        override __.Equals(other) =
            let otherAsBase = other :?> BaseClass
            otherAsBase.name = name

        override __.GetHashCode() =
            name.GetHashCode()

    type ChildClass(name : string) =
        inherit BaseClass(name)

    type IndexerClass() =
        let mutable ordinals = [| "one"; "two"; "three"; "four"; |]
        let mutable cardinals = [| "first"; "second"; "third"; "fourth"; |]
        
        member __.Item
            with get(index) = ordinals.[index]
            and set index value = ordinals.[index] <- value

    type Struct =
        struct
            val StructName : string
        end

    type FieldClass =
        val mutable number : int
        new (num) = {number = num}

    type DisposableClass(name : string) =
        member __.name = name
        static member val IsDisposed = false with get, set
        interface IDisposable with
            member __.Dispose() = DisposableClass.IsDisposed <- true

    type Union = UnionA | UnionB | UnionC

    exception TestException of string
    
    type ClassWithToString(name : string) =
        member __.name = name
        override __.ToString() = name

    type InheritsClassWithTostring(name : string) =
        inherit ClassWithToString(name)
        
    type StructWithToString =
        struct
            val StructName : string
            override __.ToString() = __.StructName
        end

    type ClassWithReflectedDefinition() =
        [<ReflectedDefinition>]
        member __.GetFullName person =
            sprintf "%s %s" person.FirstName person.LastName
        
        [<ReflectedDefinition>]
        member __.GetOne() = 1
