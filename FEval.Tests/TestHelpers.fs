namespace FEval.Tests

module TestHelpers =
    open System
    open Microsoft.FSharp.Quotations

    let createValueExpr<'a>() =
        Some <| Expr.Value (null, typeof<'a>)

    type Person =
        {
            FirstName : string
            LastName : string
        }

    type BaseClass(name : string) =
        member this.name = name
        member val NameProperty = name with get, set

        override this.Equals(other) =
            let otherAsBase = other :?> BaseClass
            otherAsBase.name = name

        override this.GetHashCode() =
            name.GetHashCode()

    type ChildClass(name : string) =
        inherit BaseClass(name)

    type IndexerClass() =
        let mutable ordinals = [| "one"; "two"; "three"; "four"; |]
        let mutable cardinals = [| "first"; "second"; "third"; "fourth"; |]
        
        member this.Item
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
        member this.name = name
        static member val IsDisposed = false with get, set
        interface IDisposable with
            member this.Dispose() = DisposableClass.IsDisposed <- true

    type Union = UnionA | UnionB | UnionC

    exception TestException of string
    
    type ClassWithToString(name : string) =
        member this.name = name
        override this.ToString() = name

    type InheritsClassWithTostring(name : string) =
        inherit ClassWithToString(name)
        
    type StructWithToString =
        struct
            val StructName : string
            override this.ToString() = this.StructName
        end