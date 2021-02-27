// Tests for C#'s is pattern matching. 
// Consider writing some property based test for this, as there is considerable code transforms going on
namespace Tests

open NUnit.Framework
open FSharper.Core
open FsUnit
open CodeFormatter

[<TestFixture>]
type ConversionCsharpIsPatternMatching () =

    [<Test>]
    member this.``convert C# is statement with name binding`` () = 
        let csharp = 
             """
                public CodeFixMenu Foo(object item)
                {
                    if (item is CodeFixMenu itemAsMenu)
                    {
                        return itemAsMenu;
                    }
                    return null;
                }"""

        let fsharp = 
             """
                member this.Foo(item: obj): CodeFixMenu =
                    match item with
                    | :? CodeFixMenu as itemAsMenu -> itemAsMenu
                    | _ -> null"""

        csharp
        |> reduceIndent
        |> Converter.run 
        |> logConverted
        |> should equal (formatFsharpWithClass fsharp)

    [<Test>]
    member this.``convert C# is statement with name binding nested if statement using logical OR`` () = 
        let csharp = 
             """
                if (!(item is CodeFixMenu itemAsMenu) || itemAsMenu.Items.Count <= 0) {
                    _menuItem.Sensitive = false;
                }"""
                
        let fsharp = 
             """
                match item with
                | :? CodeFixMenu as itemAsMenu when itemAsMenu.Items.Count > 0 -> ()
                | _ -> _menuItem.Sensitive <- false"""

        csharp
        |> reduceIndent
        |> Converter.run 
        |> logConverted
        |> should equal (formatFsharp fsharp)

    [<Test>]
    member this.``convert C# is statement with name binding nested if statement using logical AND`` () = 
        let csharp = 
             """
                public void Foo(object item)
                {
                    if ((item is CodeFixMenu itemAsMenu) && itemAsMenu.Items.Count > 0) {
                        _menuItem.Sensitive = true;
                    }
                }"""

        let fsharp = 
             """
                member this.Foo(item: obj) =
                    match item with
                    | :? CodeFixMenu as itemAsMenu when itemAsMenu.Items.Count > 0 -> _menuItem.Sensitive <- true
                    | _ -> ()"""

        csharp
        |> reduceIndent
        |> Converter.run 
        |> logConverted
        |> should equal (formatFsharpWithClass fsharp)

    [<Test>]
    member this.``convert C# is statement with name binding nested if statement using logical AND twice`` () = 
        let csharp = 
             """
                public void Foo(object item)
                {
                    if ((item is CodeFixMenu itemAsMenu) && itemAsMenu.Items != null && itemAsMenu.Items.Count > 0) {
                        _menuItem.Sensitive = true;
                    }
                }"""

        let fsharp = 
             """
                member this.Foo(item: obj) =
                    match item with
                    | :? CodeFixMenu as itemAsMenu when itemAsMenu.Items <> null && itemAsMenu.Items.Count > 0 ->
                        _menuItem.Sensitive <- true
                    | _ -> ()"""

        csharp
        |> reduceIndent
        |> Converter.run 
        |> logConverted
        |> should equal (formatFsharpWithClass fsharp)
    [<Test>]
    member this.``convert C# is statement with name binding nested if statement using logical OR twice`` () = 
        let csharp = 
             """
                public object Foo(object item)
                {
                    if (!(item is CodeFixMenu itemAsMenu) || itemAsMenu.Items == null || itemAsMenu.Items.Count <= 0) {
                        _menuItem.Sensitive = false;
                    }

                    return _menuItem;
                }"""

        let fsharp = 
             """
                member this.Foo(item: obj): obj =
                    match item with
                    | :? CodeFixMenu as itemAsMenu when itemAsMenu.Items <> null && itemAsMenu.Items.Count > 0 -> ()
                    | _ -> _menuItem.Sensitive <- false
                    _menuItem"""

        csharp
        |> reduceIndent
        |> Converter.run 
        |> logConverted
        |> should equal (formatFsharpWithClass fsharp)

    [<Test>]
    member this.``convert C# is statement with name binding nested if statement using logical OR and logical AND`` () = 
        let csharp = 
             """
                public void Foo(object item)
                {
                    if (!(item is CodeFixMenu itemAsMenu) || itemAsMenu.Foo == null && itemAsMenu.Bar <= 0) 
                    {
                        _menuItem.Sensitive = false;
                    }
                }"""

        let fsharp = 
             """
                member this.Foo(item: obj) =
                    match item with
                    | :? CodeFixMenu as itemAsMenu when not (itemAsMenu.Foo = null && itemAsMenu.Bar <= 0) -> ()
                    | _ -> _menuItem.Sensitive <- false"""

        csharp
        |> reduceIndent
        |> Converter.run 
        |> logConverted
        |> should equal (formatFsharpWithClass fsharp)

    [<Test>]
    member this.``convert C# is statement with two name bindings`` () = 
        let csharp = 
             """
                public void Foo(object item)
                {
                    if ((item is CodeFixMenu itemAsMenu) || (item is CodeFixSubMenu itemAsSubMenu) && itemAsSubMenu.Items.Count <= 0) 
                    {
                        _menuItem.Sensitive = true;
                    }
                }"""

        let fsharp = 
             """
                member this.Foo(item: obj) =
                    match item with
                    | :? CodeFixMenu as itemAsMenu -> ()
                    | :? CodeFixSubMenu as itemAsSubMenu when itemAsSubMenu.Items.Count <= 0 -> ()
                    | _ -> _menuItem.Sensitive <- true"""

        csharp
        |> reduceIndent
        |> Converter.run 
        |> logConverted
        |> should equal (formatFsharpWithClass fsharp)

    [<Test>]
    member this.``convert C# is statement with two name bindings of different type is not supported`` () = 
        let csharp = 
             """
                public string Foo(object foo, object bar)
                {
                    if ((foo is string fooString) && (bar is string barString)) 
                    {
                        return fooString + barString;
                    }
                    return null;
                }"""

        let fsharp = 
             """
                member this.Foo(foo: obj, bar: obj): string =
                    match foo, bar with
                    | :? string as fooString, :? string as barString -> fooString + barString
                    | _ -> null"""

        csharp
        |> reduceIndent
        |> Converter.run 
        |> logConverted
        |> should equal (formatFsharpWithClass fsharp)

    [<Test>]
    member this.``convert C# is statement with two name bindings of different type is not supported with when clause`` () = 
        let csharp = 
             """
                public string Foo(object foo, object bar)
                {
                    if ((foo is string fooString) && (bar is string barString) && barString.Length > 5 && fooString.Length < 10) 
                    {
                        return fooString + barString;
                    }
                    return null;
                }"""

        let fsharp = 
             """
                member this.Foo(foo: obj, bar: obj): string =
                    match foo, bar with
                    | :? string as fooString, :? string as barString when barString.Length > 5 && fooString.Length < 10 ->
                        fooString + barString
                    | _ -> null"""

        csharp
        |> reduceIndent
        |> Converter.run 
        |> logConverted
        |> should equal (formatFsharpWithClass fsharp)

    [<Test>]
    member this.``convert C# is constant type statement `` () = 
        let csharp = 
             """
                if (item is null)
                    return false;
                else 
                    return true;"""

        let fsharp = 
             """
                match item with
                | null -> false
                | _ -> true"""

        csharp
        |> reduceIndent
        |> Converter.run 
        |> logConverted
        |> should equal (formatFsharp fsharp)
        
    [<Test>]
    member this.``convert C# is constant const statement `` () = 
        let csharp = 
             """
                if (item is "o")
                    return "a";
                else 
                    return "b";"""

        let fsharp = 
             """
                match item with
                | "o" -> "a"
                | _ -> "b" """

        csharp
        |> reduceIndent
        |> Converter.run 
        |> logConverted
        |> should equal (formatFsharp fsharp)

    [<Test>]
    member this.``convert C# is constant value statement `` () = 
        let csharp = 
             """
                if (item is var x)
                    return "a";
                else 
                    return "b";"""
                  // C# should warn that the else block is impossible to reach. 

        let fsharp = 
             """
                match item with
                | x -> "a"
                | _ -> "b" """
                 // FSharp will warn this code is impossible to reach.

        csharp
        |> reduceIndent
        |> Converter.run 
        |> logConverted
        |> should equal (formatFsharp fsharp) 

    [<Test>]
    member this.``convert C# precondition and is pattern match`` () = 
        let csharp = 
             """
                public void Foo(object item)
                {
                    if (item != null && (item is CodeFixMenu itemAsMenu))
                    {
                        return itemAsMenu;
                    }
                    return null;
                }"""

        let fsharp = 
             """
                member this.Foo(item: obj) =
                    match item with
                    | :? CodeFixMenu as itemAsMenu when item <> null -> itemAsMenu
                    | _ -> null"""

        csharp
        |> reduceIndent
        |> Converter.run 
        |> logConverted
        |> should equal (formatFsharpWithClass fsharp)
        
    [<Test>]
    member this.``convert C# precondition and is pattern match NOT`` () = 
        let csharp = 
             """
                public void Foo(object item)
                {
                    if (!item.Foo && (item is CodeFixMenu itemAsMenu))
                    {
                        return itemAsMenu;
                    }
                    return null;
                }"""

        let fsharp = 
             """
                member this.Foo(item: obj) =
                    if item.Foo then itemAsMenu
                    else
                        match item with
                        | :? CodeFixMenu as itemAsMenu -> itemAsMenu
                        | _ -> null"""

        csharp
        |> reduceIndent
        |> Converter.run 
        |> logConverted
        |> should equal (formatFsharpWithClass fsharp) 

    [<Test>]
    member this.``convert C# precondition and is pattern match OR`` () = 
        let csharp = 
             """
                public void Foo(object item)
                {
                    if (item.Bar || (item is CodeFixMenu itemAsMenu))
                    {
                        return item.Bar;
                    }
                    return null;
                }"""
               
        let fsharp = 
             """
                member this.Foo(item: obj) =
                    if item.Bar then item.Bar
                    else
                        match item with
                        | :? CodeFixMenu as itemAsMenu -> item.Bar
                        | _ -> null"""

        csharp
        |> reduceIndent
        |> Converter.run 
        |> logConverted
        |> should equal (formatFsharpWithClass fsharp) 


    [<Test>]
    member this.``convert C# precondition and is pattern match OR with AND`` () = 
        let csharp = 
             """
                public void Foo(object item)
                {
                    if (item.Bar || (item is CodeFixMenu itemAsMenu) && itemAsMenu.Items.Count <= 0 )
                    {
                        return item.Bar;
                    }
                    return null;
                }"""

        let fsharp = 
             """
                member this.Foo(item: obj) =
                    if item.Bar then item.Bar
                    else
                        match item with
                        | :? CodeFixMenu as itemAsMenu when itemAsMenu.Items.Count <= 0 -> item.Bar
                        | _ -> null"""

        csharp
        |> reduceIndent
        |> Converter.run 
        |> logConverted
        |> should equal (formatFsharpWithClass fsharp)
