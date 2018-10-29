namespace FEval

type Operators =
    static member subtract (x : obj) (y : obj) =
        match x.GetType() with
        | t when t = typeof<int16>   -> (x :?> int16)   - (y :?> int16)   :> obj
        | t when t = typeof<int32>   -> (x :?> int32)   - (y :?> int32)   :> obj
        | t when t = typeof<int64>   -> (x :?> int64)   - (y :?> int64)   :> obj
        | t when t = typeof<uint16>  -> (x :?> uint16)  - (y :?> uint16)  :> obj
        | t when t = typeof<uint32>  -> (x :?> uint32)  - (y :?> uint32)  :> obj
        | t when t = typeof<uint64>  -> (x :?> uint64)  - (y :?> uint64)  :> obj
        | t when t = typeof<byte>    -> (x :?> byte)    - (y :?> byte)    :> obj
        | t when t = typeof<sbyte>   -> (x :?> sbyte)   - (y :?> sbyte)   :> obj
        | t when t = typeof<float>   -> (x :?> float)   - (y :?> float)   :> obj
        | t when t = typeof<float32> -> (x :?> float32) - (y :?> float32) :> obj
        | t when t = typeof<decimal> -> (x :?> decimal) - (y :?> decimal) :> obj
        | t                          -> invalidOp ("Cannot perform subtract on " + t.Name)

    static member unaryNegate (x : obj) =
        match x.GetType() with
        | t when t = typeof<int16>   -> - (x :?> int16)   :> obj
        | t when t = typeof<int32>   -> - (x :?> int32)   :> obj
        | t when t = typeof<int64>   -> - (x :?> int64)   :> obj
        | t when t = typeof<sbyte>   -> - (x :?> sbyte)   :> obj
        | t when t = typeof<float>   -> - (x :?> float)   :> obj
        | t when t = typeof<float32> -> - (x :?> float32) :> obj
        | t when t = typeof<decimal> -> - (x :?> decimal) :> obj
        | t                          -> invalidOp ("Cannot perform unary negate on " + t.Name)

    static member unaryPlus (x : obj) = id x

    static member division (x : obj) (y : obj) =
        match x.GetType() with
        | t when t = typeof<int16>   -> (x :?> int16)   / (y :?> int16)   :> obj
        | t when t = typeof<int32>   -> (x :?> int32)   / (y :?> int32)   :> obj
        | t when t = typeof<int64>   -> (x :?> int64)   / (y :?> int64)   :> obj
        | t when t = typeof<uint16>  -> (x :?> uint16)  / (y :?> uint16)  :> obj
        | t when t = typeof<uint32>  -> (x :?> uint32)  / (y :?> uint32)  :> obj
        | t when t = typeof<uint64>  -> (x :?> uint64)  / (y :?> uint64)  :> obj
        | t when t = typeof<byte>    -> (x :?> byte)    / (y :?> byte)    :> obj
        | t when t = typeof<sbyte>   -> (x :?> sbyte)   / (y :?> sbyte)   :> obj
        | t when t = typeof<float>   -> (x :?> float)   / (y :?> float)   :> obj
        | t when t = typeof<float32> -> (x :?> float32) / (y :?> float32) :> obj
        | t when t = typeof<decimal> -> (x :?> decimal) / (y :?> decimal) :> obj
        | t                          -> invalidOp ("Cannot perform division on " + t.Name)
    
    static member modulus (x : obj) (y : obj) =
        match x.GetType() with
        | t when t = typeof<int16>   -> (x :?> int16)   % (y :?> int16)   :> obj
        | t when t = typeof<int32>   -> (x :?> int32)   % (y :?> int32)   :> obj
        | t when t = typeof<int64>   -> (x :?> int64)   % (y :?> int64)   :> obj
        | t when t = typeof<uint16>  -> (x :?> uint16)  % (y :?> uint16)  :> obj
        | t when t = typeof<uint32>  -> (x :?> uint32)  % (y :?> uint32)  :> obj
        | t when t = typeof<uint64>  -> (x :?> uint64)  % (y :?> uint64)  :> obj
        | t when t = typeof<byte>    -> (x :?> byte)    % (y :?> byte)    :> obj
        | t when t = typeof<sbyte>   -> (x :?> sbyte)   % (y :?> sbyte)   :> obj
        | t when t = typeof<float>   -> (x :?> float)   % (y :?> float)   :> obj
        | t when t = typeof<float32> -> (x :?> float32) % (y :?> float32) :> obj
        | t when t = typeof<decimal> -> (x :?> decimal) % (y :?> decimal) :> obj
        | t                          -> invalidOp ("Cannot perform modulus on " + t.Name)

    static member bitwiseAnd (x : obj) (y : obj) =
        match x.GetType() with
        | t when t = typeof<int16>   -> (x :?> int16)  &&& (y :?> int16)   :> obj
        | t when t = typeof<int32>   -> (x :?> int32)  &&& (y :?> int32)   :> obj
        | t when t = typeof<int64>   -> (x :?> int64)  &&& (y :?> int64)   :> obj
        | t when t = typeof<uint16>  -> (x :?> uint16) &&& (y :?> uint16)  :> obj
        | t when t = typeof<uint32>  -> (x :?> uint32) &&& (y :?> uint32)  :> obj
        | t when t = typeof<uint64>  -> (x :?> uint64) &&& (y :?> uint64)  :> obj
        | t when t = typeof<byte>    -> (x :?> byte)   &&& (y :?> byte)    :> obj
        | t when t = typeof<sbyte>   -> (x :?> sbyte)  &&& (y :?> sbyte)   :> obj
        | t                          -> invalidOp ("Cannot perform bitwise and on " + t.Name)
        
    static member bitwiseOr (x : obj) (y : obj) =
        match x.GetType() with
        | t when t = typeof<int16>   -> (x :?> int16)  ||| (y :?> int16)   :> obj
        | t when t = typeof<int32>   -> (x :?> int32)  ||| (y :?> int32)   :> obj
        | t when t = typeof<int64>   -> (x :?> int64)  ||| (y :?> int64)   :> obj
        | t when t = typeof<uint16>  -> (x :?> uint16) ||| (y :?> uint16)  :> obj
        | t when t = typeof<uint32>  -> (x :?> uint32) ||| (y :?> uint32)  :> obj
        | t when t = typeof<uint64>  -> (x :?> uint64) ||| (y :?> uint64)  :> obj
        | t when t = typeof<byte>    -> (x :?> byte)   ||| (y :?> byte)    :> obj
        | t when t = typeof<sbyte>   -> (x :?> sbyte)  ||| (y :?> sbyte)   :> obj
        | t                          -> invalidOp ("Cannot perform bitwise or on " + t.Name)
        
    static member exclusiveOr (x : obj) (y : obj) =
        match x.GetType() with
        | t when t = typeof<int16>   -> (x :?> int16)  ^^^ (y :?> int16)   :> obj
        | t when t = typeof<int32>   -> (x :?> int32)  ^^^ (y :?> int32)   :> obj
        | t when t = typeof<int64>   -> (x :?> int64)  ^^^ (y :?> int64)   :> obj
        | t when t = typeof<uint16>  -> (x :?> uint16) ^^^ (y :?> uint16)  :> obj
        | t when t = typeof<uint32>  -> (x :?> uint32) ^^^ (y :?> uint32)  :> obj
        | t when t = typeof<uint64>  -> (x :?> uint64) ^^^ (y :?> uint64)  :> obj
        | t when t = typeof<byte>    -> (x :?> byte)   ^^^ (y :?> byte)    :> obj
        | t when t = typeof<sbyte>   -> (x :?> sbyte)  ^^^ (y :?> sbyte)   :> obj
        | t                          -> invalidOp ("Cannot perform exclusive or on " + t.Name)

    static member logicalNot (x : obj) =
        match x.GetType() with
        | t when t = typeof<int16>   -> ~~~ (x :?> int16)  :> obj
        | t when t = typeof<int32>   -> ~~~ (x :?> int32)  :> obj
        | t when t = typeof<int64>   -> ~~~ (x :?> int64)  :> obj
        | t when t = typeof<uint16>  -> ~~~ (x :?> uint16) :> obj
        | t when t = typeof<uint32>  -> ~~~ (x :?> uint32) :> obj
        | t when t = typeof<uint64>  -> ~~~ (x :?> uint64) :> obj
        | t when t = typeof<byte>    -> ~~~ (x :?> byte)   :> obj
        | t when t = typeof<sbyte>   -> ~~~ (x :?> sbyte)  :> obj
        | t                          -> invalidOp ("Cannot perform logical not on " + t.Name)

    static member leftShift (x : obj) (y : int) =
        match x.GetType() with
        | t when t = typeof<int16>   -> (x :?> int16)  <<< y :> obj
        | t when t = typeof<int32>   -> (x :?> int32)  <<< y :> obj
        | t when t = typeof<int64>   -> (x :?> int64)  <<< y :> obj
        | t when t = typeof<uint16>  -> (x :?> uint16) <<< y :> obj
        | t when t = typeof<uint32>  -> (x :?> uint32) <<< y :> obj
        | t when t = typeof<uint64>  -> (x :?> uint64) <<< y :> obj
        | t when t = typeof<byte>    -> (x :?> byte)   <<< y :> obj
        | t when t = typeof<sbyte>   -> (x :?> sbyte)  <<< y :> obj
        | t                          -> invalidOp ("Cannot perform left shift on " + t.Name)
        
    static member rightShift (x : obj) (y : int) =
        match x.GetType() with
        | t when t = typeof<int16>   -> (x :?> int16)  >>> y :> obj
        | t when t = typeof<int32>   -> (x :?> int32)  >>> y :> obj
        | t when t = typeof<int64>   -> (x :?> int64)  >>> y :> obj
        | t when t = typeof<uint16>  -> (x :?> uint16) >>> y :> obj
        | t when t = typeof<uint32>  -> (x :?> uint32) >>> y :> obj
        | t when t = typeof<uint64>  -> (x :?> uint64) >>> y :> obj
        | t when t = typeof<byte>    -> (x :?> byte)   >>> y :> obj
        | t when t = typeof<sbyte>   -> (x :?> sbyte)  >>> y :> obj
        | t                          -> invalidOp ("Cannot perform right shift on " + t.Name)