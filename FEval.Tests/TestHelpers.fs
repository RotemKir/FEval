namespace FEval.Tests

module TestHelpers =

    type Person =
        {
            FirstName : string
            LastName : string
        }

    type BaseClass(name : string) =
        member this.name = name

        override this.Equals(other) =
            let otherAsBase = other :?> BaseClass
            otherAsBase.name = name

        override this.GetHashCode() =
            name.GetHashCode()

    type ChildClass(name : string) =
        inherit BaseClass(name)