namespace Tests

open NUnit.Framework
open FSharper.Core
open FsUnit
open System
open System.IO
open System.Linq
open Swensen.Unquote.Assertions

[<TestFixture>]
type ClassTests () =

    let formatFsharp (s:string) = 

        let indent = "                "
        s.Split ("\n") |> Array.map (fun x -> if x.StartsWith indent then x.Substring indent.Length else x) |> String.concat "\n"
        |> (fun s -> 
            s
                .Replace("\n    \n", "\n\n")
                .Replace("\n            \n", "\n\n")
                .Trim() )


    [<Test>]
    member this.``class with static main method`` () = 
        let csharp = 
             """using System;

                public class Program
                {
                    static void Main(string[] args)
                    {
                        Console.WriteLine("Hello, World");
                    } 
                }"""

        let fsharp = 
             """open System
                
                type Program() =
                    static member Main(args: string []) = Console.WriteLine("Hello, World")"""
                   
        csharp |> Converter.run 
        |> (fun x -> printfn "%s" x; x)
        |> should equal (formatFsharp fsharp)

    [<Test>]
    member this.``class with static field`` () = 
        let csharp = 
             """public class Program
                {
                    static foo c = "hello, world";
                }"""

        let fsharp = 
             """type Program() =
                    static let mutable c = "hello, world" """

        csharp |> Converter.run 
        |> (fun x -> printfn "%s" x; x)
        |> should equal (formatFsharp fsharp)

    [<Test>]
    member this.``class with static and instance methods - correct prefix for method calls`` () = 
        let csharp = 
             """public class Program
                {
                    public static void Main()
                    {
                        var p = new Program();
                        p.FooInstance();
                    }
                    
                    public static void FooStatic() 
                    {
                            Console.WriteLine($"Foo Static");   
                    }
                    
                    public void FooInstance()
                    {
                        FooStatic();
                        BarInstance();
                    }
        
                    private void BarInstance()
                    {
                        Console.WriteLine($"Bar instance private"); 
                    }
                }"""

        let fsharp = 
             """type Program() =

                    static member Main() =
                        let mutable p = new Program()
                        p.FooInstance()

                    static member FooStatic() = Console.WriteLine(sprintf "Foo Static")

                    member this.FooInstance() =
                        Program.FooStatic()
                        this.BarInstance()

                    member private this.BarInstance() = Console.WriteLine(sprintf "Bar instance private") """
            
        csharp |> Converter.run 
        |> (fun x -> printfn "%s" x; x)
        |> should equal (formatFsharp fsharp)

    [<Test>]
    member this.``Can convert Android application`` () = 
        let csharp = 
            """[Activity(Label = "Activity A", MainLauncher = true)]
                public class MainApplication : MvxAndroidApplication
                {
                    public MainApplication(IntPtr javaReference, JniHandleOwnership transfer) : base(javaReference, transfer)
                    {
                    }
                }"""

        let fsharp = 
            """[<Activity(Label = "Activity A", MainLauncher = true)>]
                type MainApplication(javaReference: IntPtr, transfer: JniHandleOwnership) =
                    inherit MvxAndroidApplication(javaReference, transfer)"""
                   
        csharp |> Converter.run 
        |> (fun x -> printfn "%s" x; x)
        |> should equal (formatFsharp fsharp)

    [<Test>]
    member this.``can convert class constructor with subclass args`` () = 
        let csharp = 
             """[Activity(Label = "Activity A", MainLauncher = true)]
                public class MainApplication : MvxAndroidApplication
                {
                    public MainApplication(IntPtr javaReference, JniHandleOwnership transfer) : base(javaReference, transfer)
                    {
                    }
                } """

        let fsharp = 
             """[<Activity(Label = "Activity A", MainLauncher = true)>]
                type MainApplication(javaReference: IntPtr, transfer: JniHandleOwnership) =
                    inherit MvxAndroidApplication(javaReference, transfer)"""
                       
        csharp |> Converter.run 
        |> (fun x -> printfn "%s" x; x)
        |> should equal (formatFsharp fsharp)

    [<Test>]
    member this.``convert class with generic types`` () = 
        let csharp = 
             """public partial class TipView<T, Z> : MvxContentPage<TipViewModel>
                {
                    public TipView(string s, int i) : base(message)
                    {
                        InitializeComponent();
                    }
                }"""

        let fsharp = 
             """type TipView<'T, 'Z>(s: string, i: int) =
                    inherit MvxContentPage<TipViewModel>(message)
                    do InitializeComponent()"""

        csharp |> Converter.run 
        |> (fun x -> printfn "%s" x; x)
        |> should equal (formatFsharp fsharp)

    [<Test>]
    member this.``convert class with interface beginning with I as interface`` () = 
        let csharp = 
             """public class Foo : IDisp
                {
                    public void Dispose()
                    {
                        FooBar();
                    }
                }"""

        let fsharp = 
             """type Foo() =
                    member this.Dispose() = FooBar()
                    interface IDisp with
                        member this.Dispose() = this.Dispose()"""

        csharp |> Converter.run 
        |> (fun x -> printfn "%s" x; x)
        |> should equal (formatFsharp fsharp)

    [<Test>]
    member this.``convert class with override method and class arg should assume inheritance`` () = 
        let csharp = 
             """public class Foo : IsoDate
                {
                    public override void Convert()
                    {
                        FooBar();
                    }
                }"""

        let fsharp = 
             """type Foo() =
                    inherit IsoDate()
                    override this.Convert() = FooBar()"""

        csharp |> Converter.run 
        |> (fun x -> printfn "%s" x; x)
        |> should equal (formatFsharp fsharp)

    [<Test>]
    member this.``convert class with call to base method`` () = 
        let csharp = 
             """public class Foo : IsoDate
                {
                    public override void Convert()
                    {
                        base.ConvertBase();
                    }
                }"""

        let fsharp = 
             """type Foo() =
                    inherit IsoDate()
                    override this.Convert() = base.ConvertBase()"""

        csharp |> Converter.run 
        |> (fun x -> printfn "%s" x; x)
        |> should equal (formatFsharp fsharp)

    [<Test>]
    member this.``convert class with subclass and interface`` () = 
        let csharp = 
             """public class Foo : IsoDate, IDisposable
                {
                    public override void Convert()
                    {
                        FooBar();
                    }

                    public void Dispose() 
                    {
                        Clear();
                    }
                }"""

        let fsharp = 
             """type Foo() =
                    inherit IsoDate()
                    override this.Convert() = FooBar()
                    member this.Dispose() = Clear()
                    interface IDisposable with
                        member this.Dispose() = this.Dispose()"""

        csharp |> Converter.run 
        |> (fun x -> printfn "%s" x; x)
        |> should equal (formatFsharp fsharp)

    [<Test>]
    member this.``convert class with multiple interfaces`` () = 
        let csharp = 
             """public class Foo : IDisposable, IFoo
                {
                    public void Dispose()
                    {
                        FooBar();
                    }

                    public void Clear() 
                    {
                        Baz();
                    }
                }"""

        // Two methods on the class means that there is not enough information to know
        // which method belongs to which interface. 
        let fsharp = 
             """type Foo() =
                    member this.Dispose() = FooBar()
                    member this.Clear() = Baz()

                    interface IDisposable with
                        member this.Todo() = ()

                    interface IFoo with
                        member this.Todo() = ()"""

        csharp |> Converter.run 
        |> (fun x -> printfn "%s" x; x)
        |> should equal (formatFsharp fsharp)


    [<Test>]
    member this.``convert constructor and interface`` () = 
        let csharp = 
             """public class AppBootstrapper : ReactiveObject
                {
                    public RoutingState Router { get; protected set; }

                    public AppBootstrapper()
                    {
                        Router = new RoutingState();
                        Locator.CurrentMutable.RegisterConstant(this, typeof(IScreen));
                        Locator.CurrentMutable.Register(() => new MainView(), typeof(IViewFor<MainViewModel>));
                        Locator.CurrentMutable.Register(() => new SecondView(), typeof(IViewFor<SecondViewModel>));

                        this
                            .Router
                            .NavigateAndReset
                            .Execute(new MainViewModel())
                            .Subscribe();
                    }
                }"""

        let fsharp = 
             """type AppBootstrapper() =
                    inherit ReactiveObject()

                    do
                        Router <- new RoutingState()
                        Locator.CurrentMutable.RegisterConstant(this, typeof<IScreen>)
                        Locator.CurrentMutable.Register(fun () -> new MainView(), typeof<IViewFor<MainViewModel>>)
                        Locator.CurrentMutable.Register(fun () -> new SecondView(), typeof<IViewFor<SecondViewModel>>)
                        this.Router.NavigateAndReset.Execute(new MainViewModel()).Subscribe()

                    member val Router: RoutingState = Unchecked.defaultof<RoutingState> with get, set"""

        csharp |> Converter.runWithConfig false 
        |> (fun x -> printfn "%s" x; x)
        |> should equal (formatFsharp fsharp)  


    [<Test>]
    member this.``convert static class with constants`` () = 
        let csharp = 
             """namespace MedsProcessor.Common
                {
                    public static class Constants
                    {
                        public const string CURRENT_LISTS_URL = "http://www.hzzo.hr/zdravstveni-sustav-rh/trazilica-za-lijekove-s-vazecih-lista/";
                        public const string ARCHIVE_LISTS_URL = "http://www.hzzo.hr/zdravstveni-sustav-rh/trazilica-za-lijekove-s-vazecih-lista/arhiva-liste-lijekova/";
                        public const string DOWNLOAD_DIR = "";
                    }
                }"""

        let fsharp = 
             """namespace MedsProcessor.Common

                type Constants() =
                    member this.CURRENT_LISTS_URL = "http://www.hzzo.hr/zdravstveni-sustav-rh/trazilica-za-lijekove-s-vazecih-lista/"
                    member this.ARCHIVE_LISTS_URL =
                        "http://www.hzzo.hr/zdravstveni-sustav-rh/trazilica-za-lijekove-s-vazecih-lista/arhiva-liste-lijekova/"
                    member this.DOWNLOAD_DIR = "" """

        csharp |> Converter.runWithConfig false 
        |> (fun x -> printfn "%s" x; x)
        |> should equal (formatFsharp fsharp)  

    [<Test>]
    member this.``constructor is optimised to F# idomatic ie no fields`` () = 
        let csharp = 
             """public class HzzoHtmlScraper
            	{
            		readonly IBrowsingContext _browsingContext;

            		public HzzoHtmlScraper(IBrowsingContext browsingContext)
            		{
            			this._browsingContext = browsingContext;
            		}
              	}"""

        let fsharp = 
             """type HzzoHtmlScraper(_browsingContext: IBrowsingContext) ="""

        test <@ csharp |> Converter.runWithConfig false = formatFsharp fsharp @>

    [<Test>] 
    member this.``constructor is optimised to F# idomatic ie no fields when names the same`` () = 
        let csharp = 
             """public class HzzoHtmlScraper
            	{
            		readonly IBrowsingContext browsingContext;

            		public HzzoHtmlScraper(IBrowsingContext browsingContext)
            		{
            			this.browsingContext = browsingContext;
            		}
              	}"""

        let fsharp = 
             """type HzzoHtmlScraper(browsingContext: IBrowsingContext) ="""

        test <@ csharp |> Converter.runWithConfig false = formatFsharp fsharp @>    

    [<Test>] 
    member this.``constructor init is placed after fields`` () = 
        let csharp = 
             """public class HzzoHtmlScraper
            	{
            		readonly IBrowsingContext browsingContext;
                    string _bar;

            		public HzzoHtmlScraper(IBrowsingContext browsingContext)
            		{
            			this.browsingContext = browsingContext;
                        Foo();
                        _bar = "42";
            		}
              	}"""

        let fsharp = 
             """type HzzoHtmlScraper(browsingContext: IBrowsingContext) =
                    let mutable _bar = Unchecked.defaultof<string>
                    do 
                        Foo()
                        _bar <- "42" """

        test <@ 
                    csharp |> Converter.runWithConfig false
                    |> (fun x -> x.Split '\n' |> Array.toList)
                    |> List.map (fun x -> x.Trim())
                        = 
                        (fsharp 
                            |> formatFsharp 
                            |> (fun x -> x.Split '\n' |> Array.toList) 
                            |> List.map (fun x -> x.Trim())) @> 

    [<Test>] 
    member this.``convert inner class`` () = 
        let csharp = 
             """public class FooBar
            	{
            		public void Foo()
            		{
            			var x = new InternalBar();
                        x.Baz();
            		}

                    class InternalBar
                    {
                        public void Baz() 
                        {

                        }
                    }
              	}"""

        let fsharp = 
             """type InternalBar() = 
                    member this.Baz() = ()
                    
                type FooBar() = 
                    member this.Foo() = 
                        let mutable x = new InternalBar()
                        x.Baz()"""

        test <@ 
                    csharp |> Converter.runWithConfig false
                    |> (fun x -> x.Split '\n' |> Array.toList)
                    |> List.map (fun x -> x.Trim())
                        = 
                        (fsharp 
                            |> formatFsharp 
                            |> (fun x -> x.Split '\n' |> Array.toList) 
                            |> List.map (fun x -> x.Trim())) @> 

    [<Test>] 
    member this.``convert public field to public property`` () = 
        let csharp = 
             """public class FooBar
            	{
                    public Object bar;
            		public FooBar(Object o)
            		{
            			this.bar = o;
            		}
              	}"""

        let fsharp = 
             """type FooBar(o: obj) = 
                    do this.bar <- o
                    member val bar: obj = Unchecked.defaultof<obj> with get, set"""

        test <@ 
                    csharp |> Converter.runWithConfig false
                    |> (fun x -> x.Split '\n' |> Array.toList)
                    |> List.map (fun x -> x.Trim())
                        = 
                        (fsharp 
                            |> formatFsharp 
                            |> (fun x -> x.Split '\n' |> Array.toList) 
                            |> List.map (fun x -> x.Trim())) @> 

    [<Test>] 
    member this.``convert public fields to F# properties`` () = 
        let csharp = 
             """class Entry
                {
                    public object data;
                    public Entry next;
                    public Entry(Entry next, object data)
                    {
                        this.next = next;
                        this.data = data;
                    }
                }"""

        let fsharp = 
             """type Entry(next: Entry, data: obj) =
                    member this.data = data
                    member this.next = next"""

        test <@ 
                    csharp |> Converter.runWithConfig false
                    |> (fun x -> x.Split '\n' |> Array.toList)
                    |> List.map (fun x -> x.Trim())
                        = 
                        (fsharp 
                            |> formatFsharp 
                            |> (fun x -> x.Split '\n' |> Array.toList) 
                            |> List.map (fun x -> x.Trim())) @> 