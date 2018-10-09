namespace FEval.Tests

module TestHelpers =

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
