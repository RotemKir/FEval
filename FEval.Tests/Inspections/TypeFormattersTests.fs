namespace FEval.Tests

open FEval.Tests.TestHelpers
open FEval.Inspections.TypeFormatters
open Microsoft.VisualStudio.TestTools.UnitTesting
open System
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Quotations

[<TestClass>]
type TypeFormattersTests() =
    let mockTypeFormatter (t : Type) = t.Name

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

    [<TestMethod>]
    member this.``formatGenericType - type is int list - returns List<Int32>``() = 
        let result = formatGenericType typeof<int list> mockTypeFormatter
        Assert.AreEqual("List<Int32>", result)
        
    [<TestMethod>]
    member this.``formatGenericType - type is int option - returns Option<Int32>``() = 
        let result = formatGenericType typeof<int option> mockTypeFormatter
        Assert.AreEqual("Option<Int32>", result)
        
    [<TestMethod>]
    member this.``formatGenericType - type is Map<int, string> - returns Map<Int32, String>``() = 
        let result = formatGenericType typeof<Map<int, string>> mockTypeFormatter
        Assert.AreEqual("Map<Int32, String>", result)
        
    [<TestMethod>]
    member this.``formatGenericType - type is Action<int, string> - returns Action<Int32, String>``() = 
        let result = formatGenericType typeof<Action<int, string>> mockTypeFormatter
        Assert.AreEqual("Action<Int32, String>", result)

    [<TestMethod>]
    member this.``formatValue - value is null object - returns null : Object``() = 
        let result = formatValue null typeof<obj>
        Assert.AreEqual("null : Object", result)
        
    [<TestMethod>]
    member this.``formatValue - value is object - returns Object``() = 
        let result = formatValue <| new obj() <| typeof<obj>
        Assert.AreEqual("Object", result)
        
    [<TestMethod>]
    member this.``formatValue - value is boxed int - returns Int (Object)``() = 
        let result = formatValue <| (3 :> obj) <| typeof<obj>
        Assert.AreEqual("3 : Int32 (Object)", result)
        
    [<TestMethod>]
    member this.``formatValue - type is function int to string - returns Int32 -> String``() = 
        let funcType = FSharpType.MakeFunctionType (typeof<int>, typeof<string>)
        let func = FSharpValue.MakeFunction (funcType, id)
        let result = formatValue func typeof<int -> string>
        Assert.AreEqual("(Int32 -> String)", result)
        
    [<TestMethod>]
    member this.``formatValue - value is null option - returns None : Option``() = 
        let result = formatValue null typeof<int option>
        Assert.AreEqual("None : Option<Int32>", result)
      
    [<TestMethod>]
    member this.``formatValue - value is some option - returns Some : Option``() = 
        let result = formatValue <| (Some 4 :> obj) <| typeof<int option>
        Assert.AreEqual("Some 4 : Option<Int32>", result)
    
    [<TestMethod>]
    member this.``formatValue - value is 17 int - returns 17 : Int32``() = 
        let result = formatValue <| (17 :> obj) <| typeof<int>
        Assert.AreEqual("17 : Int32", result)

    [<TestMethod>]
    member this.``formatValue - value is Person record - returns Person``() = 
        let result = 
            formatValue 
            <| ({FirstName = "First" ; LastName = "Last"} :> obj) 
            <| typeof<Person>
        Assert.AreEqual("{FirstName = \"First\";\n LastName = \"Last\";} : Person", result)
        
    [<TestMethod>]
    member this.``formatValue - value is UnionA - returns UnionA : Union``() = 
        let result = formatValue <| (UnionA :> obj) <| typeof<Union>
        Assert.AreEqual("UnionA : Union", result)
        
    [<TestMethod>]
    member this.``formatValue - value is class with no ToString - returns class name``() = 
        let result = formatValue <| (new ChildClass("") :> obj )<| typeof<ChildClass>
        Assert.AreEqual("ChildClass", result)
        
    [<TestMethod>]
    member this.``formatValue - value is class with ToString - returns ToString : class name``() = 
        let result = formatValue <| (new ClassWithToString("Hello") :> obj )<| typeof<ClassWithToString>
        Assert.AreEqual("Hello : ClassWithToString", result)
 
    [<TestMethod>]
    member this.``formatVariable - variable type is int - returns name : Int32``() = 
        let result = formatVariable <| new Var("number", typeof<int>)
        Assert.AreEqual("number : Int32", result)
        
    [<TestMethod>]
    member this.``formatVariable - variable type is tuple - returns name : tuple type``() = 
        let result = formatVariable <| new Var("tuple", typeof<int * string>)
        Assert.AreEqual("tuple : (Int32, String)", result)
        
    [<TestMethod>]
    member this.``formatVariable - variable type is function - returns name : function type``() = 
        let result = formatVariable <| new Var("func", typeof<int -> string>)
        Assert.AreEqual("func : (Int32 -> String)", result)
        
    [<TestMethod>]
    member this.``formatVariable - variable type is list - returns name : list type``() = 
        let result = formatVariable <| new Var("list", typeof<int list>)
        Assert.AreEqual("list : List<Int32>", result)
        
    [<TestMethod>]
    member this.``formatVariable - variable type is record - returns name : record type``() = 
        let result = formatVariable <| new Var("person", typeof<Person>)
        Assert.AreEqual("person : Person", result)
        
    [<TestMethod>]
    member this.``formatVariable - variable type is union - returns name : union type``() = 
        let result = formatVariable <| new Var("some union", typeof<Union>)
        Assert.AreEqual("some union : Union", result)

    [<TestMethod>]
    member this.``formatVariable - variable type is class - returns name : class type``() = 
        let result = formatVariable <| new Var("some class", typeof<ChildClass>)
        Assert.AreEqual("some class : ChildClass", result)
            
    [<TestMethod>]
    member this.``formatVariables - no variables - returns empty list``() = 
        let result = formatVariables [||]
        Assert.AreEqual(String.Empty, result)
    
    [<TestMethod>]
    member this.``formatVariables - one variable - returns formatted variable``() = 
        let result = formatVariables [| new Var("number", typeof<int>) |]
        Assert.AreEqual("number : Int32", result)

    [<TestMethod>]
    member this.``formatVariables - several variables - returns formatted variables separated by ,``() = 
        let result = formatVariables 
                        [| 
                            new Var("number", typeof<int>) 
                            new Var("text", typeof<string>) 
                            new Var("isTrue", typeof<bool>) 
                        |]
        Assert.AreEqual("number : Int32, text : String, isTrue : Boolean", result)

    [<TestMethod>]
    member this.``formatMethodName - returns formatted type and method name``() = 
        let result = formatMethodName <| typeof<int>.GetMethod("GetHashCode") <| typeof<int>
        Assert.AreEqual("Int32.GetHashCode", result)

    [<TestMethod>]
    member this.``formatMethod - has no instance - returns method declaring type and method name``() = 
        let result = formatMethod <| typeof<int>.GetMethod("GetHashCode") <| None
        Assert.AreEqual("Int32.GetHashCode()", result)
        
    [<TestMethod>]
    member this.``formatMethod - has no instance, method has parameters - returns method declaring type, method name and parameters``() = 
        let result = formatMethod <| typeof<string>.GetMethod("Insert") <| None
        Assert.AreEqual("String.Insert(Int32, String)", result)
        
    [<TestMethod>]
    member this.``formatMethod - has instance - returns instance type and member name``() = 
        let result = formatMethod
                        <| typeof<int>.GetMethod("GetHashCode")
                        <| createValueExpr<string>()
        Assert.AreEqual("String.GetHashCode()", result)
        
    [<TestMethod>]
    member this.``formatMethod - has instance, method has parameters - returns instance type, method name and parameters``() = 
        let result = formatMethod 
                        <| typeof<string>.GetMethod("Insert")
                        <| createValueExpr<int>()
        Assert.AreEqual("Int32.Insert(Int32, String)", result)
        
    [<TestMethod>]
    member this.``formatCtor - has no parameters - returns type name with ()``() = 
        let result = formatCtor (typeof<Exception>.GetConstructor([||]))
        Assert.AreEqual("Exception()", result)
        
    [<TestMethod>]
    member this.``formatCtor - has parameters - returns type name with parameters ()``() = 
        let result = 
            formatCtor 
                (typeof<Exception>.GetConstructor([|typeof<string> ; typeof<Exception> |]))
        Assert.AreEqual("Exception(String, Exception)", result)
    
    [<TestMethod>]
    member this.``formatPropertyName - returns declaring type name and property name()``() = 
        let result = formatPropertyName
                        <| typeof<Exception>.GetProperty("Message")
                        <| typeof<string>
        Assert.AreEqual("String.Message", result)

    [<TestMethod>]
    member this.``formatProperty - has instance - returns instance type name and property name()``() = 
        let result = formatProperty 
                        <| typeof<Exception>.GetProperty("Message")
                        <| createValueExpr<string>()
        Assert.AreEqual("String.Message", result)
    
    [<TestMethod>]
    member this.``formatProperty - has no instance - returns declaring type name and property name()``() = 
        let result = formatProperty <| typeof<Exception>.GetProperty("Message") <| None
        Assert.AreEqual("Exception.Message", result)
        
    [<TestMethod>]
    member this.``formatParameterTypes - has no parameters - returns empty string``() = 
        let result = formatParameterTypes [||]
        Assert.AreEqual(String.Empty, result)
        
    [<TestMethod>]
    member this.``formatParameterTypes - has parameters - returns types separated by ,``() = 
        let result = formatParameterTypes <| typeof<string>.GetMethod("Insert").GetParameters()
        Assert.AreEqual("Int32, String", result)
        
    [<TestMethod>]
    member this.``formatParameterValues - has no parameters - returns empty string``() = 
        let result = formatParameterValues [||] [||] 
        Assert.AreEqual(String.Empty, result)
        
    [<TestMethod>]
    member this.``formatParameterValues - has parameters - returns types and values separated by ,``() = 
        let result = formatParameterValues 
                         <| typeof<string>.GetMethod("Insert").GetParameters()
                         <| [| 2 :> obj ; "Hello" :> obj |] 
        Assert.AreEqual("2 : Int32, \"Hello\" : String", result)

    [<TestMethod>]
    member this.``formatIndexerParameterTypes - has no parameters - returns empty string``() = 
        let result = formatIndexerParameterTypes [||]
        Assert.AreEqual(String.Empty, result)
        
    [<TestMethod>]
    member this.``formatIndexerParameterTypes - has parameters - returns [types separated by ,]``() = 
        let result = formatIndexerParameterTypes 
                        <| typeof<IndexerClass>.GetProperty("Item").GetIndexParameters()
        Assert.AreEqual("[Int32]", result)
        
    [<TestMethod>]
    member this.``formatIndexerParameterValues - has no parameters - returns empty string``() = 
        let result = formatIndexerParameterValues [||] [||]
        Assert.AreEqual(String.Empty, result)
        
    [<TestMethod>]
    member this.``formatIndexerParameterValues - has parameters - returns [types and values separated by ,]``() = 
        let result = formatIndexerParameterValues 
                         <| typeof<IndexerClass>.GetProperty("Item").GetIndexParameters()
                         <| [| 2 :> obj |]
        Assert.AreEqual("[2 : Int32]", result)
    
    [<TestMethod>]
    member this.``formatFieldName - returns declaring type name and field name``() = 
        let result = formatFieldName
                        <| typeof<FieldClass>.GetField("number")
                        <| typeof<string>
        Assert.AreEqual("String.number", result)

    [<TestMethod>]
    member this.``formatField - has instance - returns instance type name and field name``() = 
        let result = formatField
                        <| typeof<FieldClass>.GetField("number")
                        <| createValueExpr<string>()
        Assert.AreEqual("String.number", result)
    
    [<TestMethod>]
    member this.``formatField - has no instance - returns declaring type name and field name``() = 
        let result = formatField <| typeof<FieldClass>.GetField("number") <| None
        Assert.AreEqual("FieldClass.number", result)
                
    [<TestMethod>]
    member this.``formatUnionCaseInfo - returns union case name and type name``() = 
        let result = formatUnionCaseInfo <| (Array.item 0 <| FSharpType.GetUnionCases typeof<Union>)
        Assert.AreEqual("UnionA : Union", result)