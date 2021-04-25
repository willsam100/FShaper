namespace Tests
open CodeFormatter
open NUnit.Framework
open FShaper.Core
open FsUnit
open Swensen.Unquote.Assertions

[<TestFixture>]
type SingleLineMethodsTests () =        

    [<Test>]
    member this.``parse chars`` () = 
        // https://devblogs.microsoft.com/csharpfaq/what-character-escape-sequences-are-available/
        let csharp = 
             """
                void Foo() 
                { 
                    Console.WriteLine('a'); 
                    Console.WriteLine('\n'); 
                    Console.WriteLine('\t'); 
                    Console.WriteLine('\r'); 
                    Console.WriteLine('\b'); 
                    Console.WriteLine('\''); 
                    Console.WriteLine('\\');
                }"""

        let fsharp = 
             """
                member this.Foo() =
                    Console.WriteLine('a')
                    Console.WriteLine('\n')
                    Console.WriteLine('\t')
                    Console.WriteLine('\r')
                    Console.WriteLine('\b')
                    Console.WriteLine('\'')
                    Console.WriteLine('\\')"""
                   
        csharp
        |> reduceIndent
        |> Converter.run 
        |> logConverted
        |> should equal (formatFsharpWithClass fsharp)


    [<Test>]
    member this.``modulo operation`` () = 
        let csharp = 
             """
                int Foo() { c[i] % n; }"""

        let fsharp = 
             """
                member this.Foo(): int = c.[i] % n"""
                   
        csharp
        |> reduceIndent
        |> Converter.run 
        |> logConverted
        |> should equal (formatFsharpWithClass fsharp)

    [<Test>]
    member this.``yield statement`` () = 
        let csharp = 
             """
                IEnumerable<int> Foo() { yield return 10; }"""

        let fsharp = 
             """
                member this.Foo(): seq<int> = seq { yield 10 }"""
                   
        csharp
        |> reduceIndent
        |> Converter.run 
        |> logConverted
        |> should equal (formatFsharpWithClass fsharp)

    [<Test>]
    member this.``conditional statement`` () = 
        let csharp = 
             """
                string Foo() { return (c == 1 ? "" : "s"); }"""

        let fsharp = 
             """
                member this.Foo(): string = if c = 1 then "" else "s" """
                   
        csharp
        |> reduceIndent
        |> Converter.run 
        |> logConverted
        |> should equal (formatFsharpWithClass fsharp)

    [<Test>]
    member this.``adding to string is escaped`` () = 
        let csharp = 
             """
                void Foo() { Console.Write("Hello, " + n); }"""

        let fsharp = 
             """
                member this.Foo() = Console.Write("Hello, " + (n.ToString()))"""
                   
        csharp
        |> reduceIndent
        |> Converter.run 
        |> logConverted
        |> should equal (formatFsharpWithClass fsharp)


    [<Test>]
    member this.``global prefix in namespace converts to period for statement`` () = 
        let csharp = 
             """
                global::Xamarin.Forms.Forms.Init(this, bundle);"""
    
        let fsharp = 
             """
                global.Xamarin.Forms.Forms.Init(this, bundle)"""
        csharp |> Converter.runWithConfig false 
        |> logConverted
        |> should equal (formatFsharp fsharp)     


    [<Test>]
    member this.``Can convert class method`` () = 
        let csharp = 
            """
                public void Foo(Class a, Hud fifty) {
                    Console.WriteLine("Hello, world");
                } """

        let fsharp = 
            """
                member this.Foo(a: Class, fifty: Hud) = Console.WriteLine("Hello, world")"""
                   
        csharp
        |> reduceIndent
        |> Converter.run 
        |> logConverted
        |> should equal (formatFsharpWithClass fsharp)


    [<Test>]
    member this.``convert const`` () = 
        let csharp = 
             """
                class CodeActionEditorExtension
                {
                    const int menuTimeout = 150;
                }"""

        let fsharp = 
            """
                type CodeActionEditorExtension() =
                    let menuTimeout = 150"""
                   
        csharp
        |> reduceIndent
        |> Converter.run 
        |> logConverted
        |> should equal (formatFsharp fsharp)

    [<Test>]
    member this.``convert on clicked handler`` () = 
        let csharp = 
             """
                ContextMenu CreateContextMenu (CodeFixMenu entrySet)
                {
                    _menuItem.Clicked += (sender, e) => sender.Foo();
                }"""

        let fsharp = 
             """
                member this.CreateContextMenu(entrySet: CodeFixMenu): ContextMenu =
                    _menuItem.Clicked
                        .AddHandler<_>(fun (sender, e) -> sender.Foo())"""
                   
        csharp
        |> reduceIndent
        |> Converter.run 
        |> logConverted
        |> should equal (formatFsharpWithClass fsharp)

    [<Ignore("This turns out to be really hard to solve consistently without adding other bugs")>]
    [<Test>]
    member this.``preprocessor for invalid C# adds semicolon to end of lines`` () = 
        let csharp = 
             """
                public async Task StartMyTask()
                {
                    await Foo()
                    // code to execute once foo is done
                }"""

        let fsharp = 
             """
                member this.StartMyTask(): Task =
                    async { do! Foo() |> Async.AwaitTask }
                    |> Async.StartAsTask"""
                       
        csharp
        |> reduceIndent
        |> Converter.run 
        |> logConverted
        |> should equal (formatFsharpWithClass fsharp)

    [<Test>]
    member this.``convert convert lambda`` () = 
        let csharp = 
             """
                public void Include(INotifyCollectionChanged changed)
                {
                    changed.CollectionChanged += (s, e) => { var test = $"Args: {e.Action}{e.NewItems}{e.OldItems}, Index: {e.OldStartingIndex}"; };
                }"""

        let fsharp = 
             """
                member this.Include(changed: INotifyCollectionChanged) =
                    changed.CollectionChanged
                        .AddHandler<_>(fun (s, e) ->
                            let mutable test = sprintf "Args: %O%O%O, Index: %O" (e.Action) (e.NewItems) (e.OldItems) (e.OldStartingIndex)
                            ())"""
                       
        csharp
        |> reduceIndent
        |> Converter.run 
        |> logConverted
        |> should equal (formatFsharpWithClass fsharp)

    [<Test>]
    member this.``convert private method`` () = 
        let csharp = 
             """
                public class Foo
                {
                    private void Foo() 
                    {
                        return;
                    }
                }"""

        let fsharp = 
             """
                type Foo() =
                    member private this.Foo() = ()"""

        csharp
        |> reduceIndent
        |> Converter.run 
        |> logConverted
        |> should equal (formatFsharp fsharp)

    [<Test>]
    member this.``convert object type to obj`` () = 
        let csharp = 
             """
                public class Foo
                {
                    public Foo(object oo) { }
                    
                    public void Foo(object o)
                    {
                        var x = (object) o;
                        return;
                    }
                }"""

        let fsharp = 
             """
                type Foo(oo: obj) =
                    member this.Foo(o: obj) =
                        let mutable x = (o :?> obj)
                        ()"""

        csharp
        |> reduceIndent
        |> Converter.run  
        |> logConverted
        |> should equal (formatFsharp fsharp)

    [<Test>]
    member this.``convert invocation on cast to Action to Invoke()`` () = 
        let csharp = 
             """
                public void CreateContextMenu (object entrySet)
                {
                    ((Action)entrySet) ();
                }"""
    
        let fsharp = 
             """
                member this.CreateContextMenu(entrySet: obj) = (entrySet :?> Action).Invoke()"""
                   
        csharp
        |> reduceIndent
        |> Converter.run
        |> logConverted
        |> should equal (formatFsharpWithClass fsharp) 

    [<Test>]
    member this.``convert invocation on cast to Action args to invoke args`` () = 
        let csharp = 
             """
                public void CreateContextMenu (object entrySet)
                {
                    ((Action<int>)entrySet)(42);
                }"""
    
        let fsharp = 
             """
                member this.CreateContextMenu(entrySet: obj) = (entrySet :?> Action<int>).Invoke(42)"""
                   
        csharp
        |> reduceIndent
        |> Converter.run 
        |> logConverted
        |> should equal (formatFsharpWithClass fsharp) 

    [<Test>]
    member this.``convert invocation on cast to Func to Invoke()`` () = 
        let csharp = 
             """
                public int CreateContextMenu (object entrySet)
                {
                    ((Func<int>)entrySet) ();
                }"""
    
        let fsharp = 
             """
                member this.CreateContextMenu(entrySet: obj): int = (entrySet :?> Func<int>).Invoke()"""
                   
        csharp
        |> reduceIndent
        |> Converter.run 
        |> logConverted
        |> should equal (formatFsharpWithClass fsharp)   

    [<Test>]
    member this.``convert invocation on cast to Func args to Invoke args`` () = 
        let csharp = 
             """
                public int CreateContextMenu (object entrySet)
                {
                    return ((Func<int,int>)entrySet)(42);
                }"""
    
        let fsharp = 
             """
                member this.CreateContextMenu(entrySet: obj): int = (entrySet :?> Func<int, int>).Invoke(42)"""
                   
        csharp
        |> reduceIndent
        |> Converter.run 
        |> logConverted
        |> should equal (formatFsharpWithClass fsharp)



    [<Test>]
    member this.``incomplete recursive static method`` () = 
        let csharp = 
             """
                static int fib(int n)
                {
                    if (n == 0 || n == 1)
                        return n;

                    return fib(n - 1) + fib(n - 2);
                }"""
    
        let fsharp = 
             """
                static member fib (n: int): int = if n = 0 || n = 1 then n else fib (n - 1) + fib (n - 2)"""

        csharp
        |> reduceIndent
        |> Converter.runWithConfig false
        |> logConverted
        |> should equal (formatFsharpWithClass fsharp)

        
    [<Test>]
    member this.``method with params keyword`` () = 
        let csharp = 
             """
                public static void UseParams(params int[] myList)
                {
                    Console.WriteLine();
                }"""
    
        let fsharp = 
             """
                static member UseParams(([<ParamArray>] myList: int [])) = Console.WriteLine()"""

        csharp
        |> reduceIndent
        |> Converter.runWithConfig false
        |> logConverted
        |> should equal (formatFsharpWithClass fsharp)


    [<Test>]
    member this.``convert expression body with CoalesceExpression`` () = 
        let csharp = 
             """
                public class HeadsMealDownloads
                {
                    public string FileName => 
                        ValidFrom.ToString("yyyy-MM-dd_") +
                        (Href.Split('/').LastOrDefault() ?? Href.Replace("/", "_").Replace(":", "_")).TrimEnd();
                }"""

        let fsharp = 
             """
                type HeadsMealDownloads() =
                    member this.FileName =
                        ValidFrom.ToString("yyyy-MM-dd_") +
                            (Href.Split('/').LastOrDefault() |> Option.ofObj |> Option.defaultValue (Href.Replace("/", "_").Replace(":", "_"))).TrimEnd()"""

        csharp
        |> reduceIndent
        |> Converter.runWithConfig false
        |> logConverted
        |> should equal (formatFsharp fsharp)  

    [<Test>]
    member this.``convert generic types`` () = 
        let csharp = 
             """
                static ISet<HeelsMedsDownloadDto> ParseMedsLiElements(IEnumerable<IElement> elems) =>
                  elems.Aggregate(new HashSet<HeelsMedsDownloadDto>(), (medsList, li) => medsList);"""

        let fsharp = 
             """
                static member ParseMedsLiElements(elems: seq<IElement>): ISet<HeelsMedsDownloadDto> =
                    elems.Aggregate(new HashSet<HeelsMedsDownloadDto>(), (fun (medsList, li) -> medsList))"""

        csharp
        |> reduceIndent
        |> Converter.runWithConfig false
        |> logConverted
        |> should equal (formatFsharpWithClass fsharp)

    [<Test>]
    member this.``convert throw exception`` () = 
        let csharp = 
             """
                public void ThrowException()
                {
                    throw new NullReferenceException();                    
                }"""

        let fsharp = 
             """
                member this.ThrowException() = raise(new NullReferenceException())"""

        csharp
        |> reduceIndent
        |> Converter.runWithConfig false
        |> logConverted
        |> should equal (formatFsharpWithClass fsharp)
        
    [<Test>]
    member this.``method with override is converted to override method`` () = 
        let csharp = 
             """
                public override void Convert()
                {
                    FooBar();
                }
                """

        let fsharp = 
             """
                override this.Convert() = FooBar()"""
                
        csharp
        |> reduceIndent
        |> Converter.runWithConfig false
        |> logConverted
        |> should equal (formatFsharpWithClass fsharp)

    [<Test>]
    member this.``coalescing operator handles assignment`` () = 
        let csharp = 
             """
                public Foo GetFoobar ()
        		{
        			return fooBar ?? (fooBar = CreateFoo());
        		}"""
    
        let fsharp = 
             """
                member this.GetFoobar(): Foo =
                    if fooBar = null then fooBar <- CreateFoo()
                    fooBar"""
        csharp
        |> reduceIndent
        |> Converter.runWithConfig false
        |> logConverted
        |> should equal (formatFsharpWithClass fsharp)   

    [<Test>]
    member this.``double question mark operator`` () = 
        let csharp = 
             """
                public Foo GetFoobar ()
        		{
        			return fooBar ?? (fooBar = CreateFoo());
        		}"""
    
        let fsharp = 
             """
                member this.GetFoobar(): Foo =
                    if fooBar = null then fooBar <- CreateFoo()
                    fooBar"""
        csharp
        |> reduceIndent
        |> Converter.runWithConfig false
        |> logConverted
        |> should equal (formatFsharpWithClass fsharp)



    