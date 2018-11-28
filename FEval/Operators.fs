namespace FEval

open FEval.TypeChecks

type Operators =

    static member subtract (x : obj) (y : obj) =
        match x.GetType() with
        | IsInt16   x xTyped -> xTyped - (y :?> int16)   :> obj
        | IsInt32   x xTyped -> xTyped - (y :?> int32)   :> obj
        | IsInt64   x xTyped -> xTyped - (y :?> int64)   :> obj
        | IsUInt16  x xTyped -> xTyped - (y :?> uint16)  :> obj
        | IsUInt32  x xTyped -> xTyped - (y :?> uint32)  :> obj
        | IsUInt64  x xTyped -> xTyped - (y :?> uint64)  :> obj
        | IsByte    x xTyped -> xTyped - (y :?> byte)    :> obj
        | IsSByte   x xTyped -> xTyped - (y :?> sbyte)   :> obj
        | IsFloat   x xTyped -> xTyped - (y :?> float)   :> obj
        | IsFloat32 x xTyped -> xTyped - (y :?> float32) :> obj
        | IsDecimal x xTyped -> xTyped - (y :?> decimal) :> obj
        | t                  -> invalidOp ("Cannot perform subtract on " + t.Name)

    static member unaryNegate (x : obj) =
        match x.GetType() with
        | IsInt16   x xTyped -> -xTyped :> obj
        | IsInt32   x xTyped -> -xTyped :> obj
        | IsInt64   x xTyped -> -xTyped :> obj
        | IsSByte   x xTyped -> -xTyped :> obj
        | IsFloat   x xTyped -> -xTyped :> obj
        | IsFloat32 x xTyped -> -xTyped :> obj
        | IsDecimal x xTyped -> -xTyped :> obj
        | t                  -> invalidOp ("Cannot perform unary negate on " + t.Name)

    static member unaryPlus (x : obj) = id x

    static member division (x : obj) (y : obj) =
        match x.GetType() with
        | IsInt16   x xTyped -> xTyped / (y :?> int16)   :> obj
        | IsInt32   x xTyped -> xTyped / (y :?> int32)   :> obj
        | IsInt64   x xTyped -> xTyped / (y :?> int64)   :> obj
        | IsUInt16  x xTyped -> xTyped / (y :?> uint16)  :> obj
        | IsUInt32  x xTyped -> xTyped / (y :?> uint32)  :> obj
        | IsUInt64  x xTyped -> xTyped / (y :?> uint64)  :> obj
        | IsByte    x xTyped -> xTyped / (y :?> byte)    :> obj
        | IsSByte   x xTyped -> xTyped / (y :?> sbyte)   :> obj
        | IsFloat   x xTyped -> xTyped / (y :?> float)   :> obj
        | IsFloat32 x xTyped -> xTyped / (y :?> float32) :> obj
        | IsDecimal x xTyped -> xTyped / (y :?> decimal) :> obj
        | t                  -> invalidOp ("Cannot perform division on " + t.Name)
    
    static member modulus (x : obj) (y : obj) =
        match x.GetType() with
        | IsInt16   x xTyped -> xTyped % (y :?> int16)   :> obj
        | IsInt32   x xTyped -> xTyped % (y :?> int32)   :> obj
        | IsInt64   x xTyped -> xTyped % (y :?> int64)   :> obj
        | IsUInt16  x xTyped -> xTyped % (y :?> uint16)  :> obj
        | IsUInt32  x xTyped -> xTyped % (y :?> uint32)  :> obj
        | IsUInt64  x xTyped -> xTyped % (y :?> uint64)  :> obj
        | IsByte    x xTyped -> xTyped % (y :?> byte)    :> obj
        | IsSByte   x xTyped -> xTyped % (y :?> sbyte)   :> obj
        | IsFloat   x xTyped -> xTyped % (y :?> float)   :> obj
        | IsFloat32 x xTyped -> xTyped % (y :?> float32) :> obj
        | IsDecimal x xTyped -> xTyped % (y :?> decimal) :> obj
        | t                  -> invalidOp ("Cannot perform modulus on " + t.Name)

    static member bitwiseAnd (x : obj) (y : obj) =
        match x.GetType() with
        | IsInt16   x xTyped -> xTyped &&& (y :?> int16)   :> obj
        | IsInt32   x xTyped -> xTyped &&& (y :?> int32)   :> obj
        | IsInt64   x xTyped -> xTyped &&& (y :?> int64)   :> obj
        | IsUInt16  x xTyped -> xTyped &&& (y :?> uint16)  :> obj
        | IsUInt32  x xTyped -> xTyped &&& (y :?> uint32)  :> obj
        | IsUInt64  x xTyped -> xTyped &&& (y :?> uint64)  :> obj
        | IsByte    x xTyped -> xTyped &&& (y :?> byte)    :> obj
        | IsSByte   x xTyped -> xTyped &&& (y :?> sbyte)   :> obj
        | t                  -> invalidOp ("Cannot perform bitwise and on " + t.Name)
        
    static member bitwiseOr (x : obj) (y : obj) =
        match x.GetType() with
        | IsInt16   x xTyped -> xTyped ||| (y :?> int16)   :> obj
        | IsInt32   x xTyped -> xTyped ||| (y :?> int32)   :> obj
        | IsInt64   x xTyped -> xTyped ||| (y :?> int64)   :> obj
        | IsUInt16  x xTyped -> xTyped ||| (y :?> uint16)  :> obj
        | IsUInt32  x xTyped -> xTyped ||| (y :?> uint32)  :> obj
        | IsUInt64  x xTyped -> xTyped ||| (y :?> uint64)  :> obj
        | IsByte    x xTyped -> xTyped ||| (y :?> byte)    :> obj
        | IsSByte   x xTyped -> xTyped ||| (y :?> sbyte)   :> obj
        | t                  -> invalidOp ("Cannot perform bitwise or on " + t.Name)
        
    static member exclusiveOr (x : obj) (y : obj) =
        match x.GetType() with
        | IsInt16   x xTyped -> xTyped ^^^ (y :?> int16)   :> obj
        | IsInt32   x xTyped -> xTyped ^^^ (y :?> int32)   :> obj
        | IsInt64   x xTyped -> xTyped ^^^ (y :?> int64)   :> obj
        | IsUInt16  x xTyped -> xTyped ^^^ (y :?> uint16)  :> obj
        | IsUInt32  x xTyped -> xTyped ^^^ (y :?> uint32)  :> obj
        | IsUInt64  x xTyped -> xTyped ^^^ (y :?> uint64)  :> obj
        | IsByte    x xTyped -> xTyped ^^^ (y :?> byte)    :> obj
        | IsSByte   x xTyped -> xTyped ^^^ (y :?> sbyte)   :> obj
        | t                  -> invalidOp ("Cannot perform exclusive or on " + t.Name)

    static member logicalNot (x : obj) =
        match x.GetType() with
        | IsInt16   x xTyped -> ~~~ xTyped :> obj
        | IsInt32   x xTyped -> ~~~ xTyped :> obj
        | IsInt64   x xTyped -> ~~~ xTyped :> obj
        | IsUInt16  x xTyped -> ~~~ xTyped :> obj
        | IsUInt32  x xTyped -> ~~~ xTyped :> obj
        | IsUInt64  x xTyped -> ~~~ xTyped :> obj
        | IsByte    x xTyped -> ~~~ xTyped :> obj
        | IsSByte   x xTyped -> ~~~ xTyped :> obj
        | t                  -> invalidOp ("Cannot perform logical not on " + t.Name)

    static member leftShift (x : obj) (y : int) =
        match x.GetType() with
        | IsInt16   x xTyped -> xTyped <<< y :> obj
        | IsInt32   x xTyped -> xTyped <<< y :> obj
        | IsInt64   x xTyped -> xTyped <<< y :> obj
        | IsUInt16  x xTyped -> xTyped <<< y :> obj
        | IsUInt32  x xTyped -> xTyped <<< y :> obj
        | IsUInt64  x xTyped -> xTyped <<< y :> obj
        | IsByte    x xTyped -> xTyped <<< y :> obj
        | IsSByte   x xTyped -> xTyped <<< y :> obj
        | t                  -> invalidOp ("Cannot perform left shift on " + t.Name)
        
    static member rightShift (x : obj) (y : int) =
        match x.GetType() with
        | IsInt16   x xTyped -> xTyped >>> y :> obj
        | IsInt32   x xTyped -> xTyped >>> y :> obj
        | IsInt64   x xTyped -> xTyped >>> y :> obj
        | IsUInt16  x xTyped -> xTyped >>> y :> obj
        | IsUInt32  x xTyped -> xTyped >>> y :> obj
        | IsUInt64  x xTyped -> xTyped >>> y :> obj
        | IsByte    x xTyped -> xTyped >>> y :> obj
        | IsSByte   x xTyped -> xTyped >>> y :> obj
        | t                  -> invalidOp ("Cannot perform right shift on " + t.Name)