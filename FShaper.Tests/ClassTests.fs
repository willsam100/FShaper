namespace Tests

open NUnit.Framework
open FShaper.Core
open FsUnit
open Swensen.Unquote.Assertions
open CodeFormatter

[<TestFixture>]
type ClassTests () =

    [<Test>]
    member this.``class with static main method`` () = 
        let csharp = 
             """
                using System;

                public class Program
                {
                    static void Main(string[] args)
                    {
                        Console.WriteLine("Hello, World");
                    } 
                }"""

        let fsharp = 
             """
                open System
                
                type Program() =
                    static member Main(args: string []) = Console.WriteLine("Hello, World")"""
                   
        csharp
        |> reduceIndent
        |> Converter.run 
        |> logConverted
        |> should equal (formatFsharp fsharp)

    [<Test>]
    member this.``class with static field`` () = 
        let csharp = 
             """
                public class Program
                {
                    static foo c = "hello, world";
                }"""

        let fsharp = 
             """
                type Program() =
                    static let mutable c = "hello, world" """

        csharp
        |> reduceIndent
        |> Converter.run 
        |> logConverted
        |> should equal (formatFsharp fsharp)

    [<Test>]
    member this.``class with static and instance methods - correct prefix for method calls`` () = 
        let csharp = 
             """
                public class Program
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
             """
                type Program() =

                    static member Main() =
                        let mutable p = new Program()
                        p.FooInstance()

                    static member FooStatic() = Console.WriteLine(sprintf "Foo Static")

                    member this.FooInstance() =
                        Program.FooStatic()
                        this.BarInstance()

                    member private this.BarInstance() = Console.WriteLine(sprintf "Bar instance private") """

        csharp
        |> reduceIndent
        |> Converter.run 
        |> logConverted
        |> should equal (formatFsharp fsharp)

    [<Test>]
    member this.``Can convert Android application`` () = 
        let csharp = 
            """
                [Activity(Label = "Activity A", MainLauncher = true)]
                public class MainApplication : MvxAndroidApplication
                {
                    public MainApplication(IntPtr javaReference, JniHandleOwnership transfer) : base(javaReference, transfer)
                    {
                    }
                }"""

        let fsharp = 
            """
                [<Activity(Label = "Activity A", MainLauncher = true)>]
                type MainApplication(javaReference: IntPtr, transfer: JniHandleOwnership) =
                    inherit MvxAndroidApplication(javaReference, transfer)"""
                   
        csharp
        |> reduceIndent
        |> Converter.run 
        |> logConverted
        |> should equal (formatFsharp fsharp)

    [<Test>]
    member this.``can convert class constructor with subclass args`` () = 
        let csharp = 
             """
                [Activity(Label = "Activity A", MainLauncher = true)]
                public class MainApplication : MvxAndroidApplication
                {
                    public MainApplication(IntPtr javaReference, JniHandleOwnership transfer) : base(javaReference, transfer)
                    {
                    }
                } """

        let fsharp = 
             """
                [<Activity(Label = "Activity A", MainLauncher = true)>]
                type MainApplication(javaReference: IntPtr, transfer: JniHandleOwnership) =
                    inherit MvxAndroidApplication(javaReference, transfer)"""
                       
        csharp
        |> reduceIndent
        |> Converter.run 
        |> logConverted
        |> should equal (formatFsharp fsharp)

    [<Test>]
    member this.``convert class with generic types`` () = 
        let csharp = 
             """
                public partial class TipView<T, Z> : MvxContentPage<TipViewModel>
                {
                    public TipView(string s, int i) : base(message)
                    {
                        InitializeComponent();
                    }
                }"""

        let fsharp = 
             """
                type TipView<'T, 'Z>(s: string, i: int) =
                    inherit MvxContentPage<TipViewModel>(message)
                    do InitializeComponent()"""

        csharp
        |> reduceIndent
        |> Converter.run 
        |> logConverted
        |> should equal (formatFsharp fsharp)

    [<Test>]
    member this.``convert class with interface beginning with I as interface`` () = 
        let csharp = 
             """
                public class Foo : IDisposable
                {
                    public void Dispose()
                    {
                        FooBar();
                    }
                }"""

        let fsharp = 
             """
                type Foo() =
                    member this.Dispose() = FooBar()
                    interface IDisposable with
                        member this.Dispose() = this.Dispose()"""

        csharp
        |> reduceIndent
        |> Converter.run 
        |> logConverted
        |> should equal (formatFsharp fsharp)

    [<Test>]
    member this.``convert class with override method and class arg should assume inheritance`` () = 
        let csharp = 
             """
                public class Foo : IsoDate
                {
                    public override void Convert()
                    {
                        FooBar();
                    }
                }"""

        let fsharp = 
             """
                type Foo() =
                    inherit IsoDate()
                    override this.Convert() = FooBar()"""

        csharp
        |> reduceIndent
        |> Converter.run 
        |> logConverted
        |> should equal (formatFsharp fsharp)

    [<Test>]
    member this.``convert class with call to base method`` () = 
        let csharp = 
             """
                public class Foo : IsoDate
                {
                    public override void Convert()
                    {
                        base.ConvertBase();
                    }
                }"""

        let fsharp = 
             """
                type Foo() =
                    inherit IsoDate()
                    override this.Convert() = base.ConvertBase()"""

        csharp
        |> reduceIndent
        |> Converter.run 
        |> logConverted
        |> should equal (formatFsharp fsharp)

    [<Test>]
    member this.``convert class with subclass and interface`` () = 
        let csharp = 
             """
                public class Foo : IsoDate, IDisposable
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
             """
                type Foo() =
                    inherit IsoDate()
                    override this.Convert() = FooBar()
                    member this.Dispose() = Clear()
                    interface IDisposable with
                        member this.Dispose() = this.Dispose()"""

        csharp
        |> reduceIndent
        |> Converter.run 
        |> logConverted
        |> should equal (formatFsharp fsharp)

    [<Test>]
    member this.``convert class with multiple interfaces`` () = 
        let csharp = 
             """
                public class Foo : IDisposable, IFoo
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
             """
                type Foo() =
                    member this.Dispose() = FooBar()
                    member this.Clear() = Baz()

                    interface IDisposable with
                        member this.Todo() = ()

                    interface IFoo with
                        member this.Todo() = ()"""

        csharp
        |> reduceIndent
        |> Converter.run 
        |> logConverted
        |> should equal (formatFsharp fsharp)


    [<Test>]
    member this.``convert constructor and interface`` () = 
        let csharp = 
             """
                public class AppBootstrapper : ReactiveObject
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
             """
                type AppBootstrapper() =
                    inherit ReactiveObject()

                    do
                        Router <- new RoutingState()
                        Locator.CurrentMutable.RegisterConstant(this, typeof<IScreen>)
                        Locator.CurrentMutable.Register((fun () -> new MainView()), typeof<IViewFor<MainViewModel>>)
                        Locator.CurrentMutable.Register((fun () -> new SecondView()), typeof<IViewFor<SecondViewModel>>)
                        this.Router.NavigateAndReset.Execute(new MainViewModel()).Subscribe()

                    member val Router: RoutingState = Unchecked.defaultof<RoutingState> with get, set"""

        csharp
        |> reduceIndent
        |> Converter.run 
        |> logConverted
        |> should equal (formatFsharp fsharp)


    [<Test>]
    member this.``convert static class with constants`` () = 
        let csharp = 
             """
                namespace MedsProcessor.Common
                {
                    public static class Constants
                    {
                        public const string CURRENT_LISTS_URL = "http://www.Heels.hr/zdravstveni-sustav-rh/trazilica-za-lijekove-s-vazecih-lista/";
                        public const string ARCHIVE_LISTS_URL = "http://www.Heels.hr/zdravstveni-sustav-rh/trazilica-za-lijekove-s-vazecih-lista/arhiva-liste-lijekova/";
                        public const string DOWNLOAD_DIR = "";
                    }
                }"""

        let fsharp = 
             """
                namespace MedsProcessor.Common

                type Constants() =
                    member this.CURRENT_LISTS_URL = "http://www.Heels.hr/zdravstveni-sustav-rh/trazilica-za-lijekove-s-vazecih-lista/"
                    member this.ARCHIVE_LISTS_URL =
                        "http://www.Heels.hr/zdravstveni-sustav-rh/trazilica-za-lijekove-s-vazecih-lista/arhiva-liste-lijekova/"
                    member this.DOWNLOAD_DIR = "" """

        csharp
        |> reduceIndent
        |> Converter.run 
        |> logConverted
        |> should equal (formatFsharp fsharp)

    [<Test>]
    member this.``constructor is optimised to F# idiomatic ie no fields`` () = 
        let csharp = 
             """
                public class HeelsHtmlScraper
            	{
            		readonly IBrowsingContext _browsingContext;

            		public HeelsHtmlScraper(IBrowsingContext browsingContext)
            		{
            			this._browsingContext = browsingContext;
            		}
              	}"""

        let fsharp = 
             """
                type HeelsHtmlScraper(_browsingContext: IBrowsingContext) ="""

        csharp
        |> reduceIndent
        |> Converter.run 
        |> logConverted
        |> should equal (simpleFormat fsharp)

    [<Test>] 
    member this.``constructor is optimised to F# idiomatic ie no fields when names the same`` () = 
        let csharp = 
             """
                public class HeelsHtmlScraper
            	{
            		readonly IBrowsingContext browsingContext;

            		public HeelsHtmlScraper(IBrowsingContext browsingContext)
            		{
            			this.browsingContext = browsingContext;
            		}
              	}"""

        let fsharp = 
             """
                type HeelsHtmlScraper(browsingContext: IBrowsingContext) ="""

        csharp
        |> reduceIndent
        |> Converter.run 
        |> logConverted
        |> should equal (simpleFormat fsharp)

    [<Test>] 
    member this.``constructor init is placed after fields`` () = 
        let csharp = 
             """
                public class HeelsHtmlScraper
            	{
            		readonly IBrowsingContext browsingContext;
                    string _bar;

            		public HeelsHtmlScraper(IBrowsingContext browsingContext)
            		{
            			this.browsingContext = browsingContext;
                        Foo();
                        _bar = "42";
            		}
              	}"""

        let fsharp = 
             """
                type HeelsHtmlScraper(browsingContext: IBrowsingContext) =
                    let mutable _bar = Unchecked.defaultof<string>
                    do 
                        Foo()
                        _bar <- "42" """

        csharp
        |> reduceIndent
        |> Converter.run 
        |> logConverted
        |> should equal (formatFsharp fsharp)

    [<Test>] 
    member this.``convert inner class`` () = 
        let csharp = 
             """
                public class FooBar
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
             """
                type InternalBar() = 
                    member this.Baz() = ()
                    
                type FooBar() = 
                    member this.Foo() = 
                        let mutable x = new InternalBar()
                        x.Baz()"""

        csharp
        |> reduceIndent
        |> Converter.run 
        |> logConverted
        |> should equal (formatFsharp fsharp)

    [<Test>] 
    member this.``convert public field to public property`` () = 
        let csharp = 
             """
                public class FooBar
            	{
                    public Object bar;
            		public FooBar(Object o)
            		{
            			this.bar = o;
            		}
              	}"""

        let fsharp = 
             """
                type FooBar(o: obj) = 
                    do this.bar <- o
                    member val bar: obj = Unchecked.defaultof<obj> with get, set"""

        csharp
        |> reduceIndent
        |> Converter.run 
        |> logConverted
        |> should equal (formatFsharp fsharp)

    [<Test>] 
    member this.``convert public fields to F# properties`` () = 
        let csharp = 
             """
                class Entry
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
             """
                type Entry(next: Entry, data: obj) =
                    member this.data = data
                    member this.next = next"""

        csharp
        |> reduceIndent
        |> Converter.run 
        |> logConverted
        |> should equal (formatFsharp fsharp)


    [<Test>]
    member this.``Convert float to float32`` () = 
        let csharp = 
             """
                class TransformedData
                {
                    public float Education { get; set; }

                    public float ZipCode { get; set; }
                }"""
    
        let fsharp = 
             """
                type TransformedData() =
                    member val Education: float32 = Unchecked.defaultof<float32> with get, set
                    member val ZipCode: float32 = Unchecked.defaultof<float32> with get, set"""

        csharp
        |> reduceIndent
        |> Converter.run 
        |> logConverted
        |> should equal (formatFsharp fsharp)

    [<Test>]
    member this.``Convert Android Effects class`` () = 
        let csharp = 
             """
                using System;

                [assembly :ResolutionGroupName ("MyCompany")]
                [assembly :ExportEffect (typeof(BackgroundColorEffect), "BackgroundColorEffect")]
                namespace EffectsDemo.Droid
                {
                	public class Foo
                	{
                		protected void OnAttached ()
                		{
                			foo();
                		}
                	}
                }"""
    
        let fsharp = 
             """
                namespace EffectsDemo.Droid

                open System

                [<assembly:ResolutionGroupName("MyCompany")>]
                do ()
                [<assembly:ExportEffect(typeof<BackgroundColorEffect>, "BackgroundColorEffect")>]
                do ()

                type Foo() =
                    member this.OnAttached() = foo()"""

        csharp
        |> reduceIndent
        |> Converter.run 
        |> logConverted
        |> should equal (formatFsharp fsharp)


    [<Test>]
    member this.``Convert Xamarin Forms page to work with F#`` () = 
        let csharp = 
             """
                using Xamarin.Forms;

                namespace AwesomeApp
                {
                    public partial class MainPage : ContentPage
                    {
                        public MainPage()
                        {
                            InitializeComponent();
                        }
                    }
                }"""
    
        let fsharp = 
             """
                namespace AwesomeApp

                open Xamarin.Forms

                type MainPage() =
                    inherit ContentPage()
                    let _ = base.LoadFromXaml typeof<MainPage>"""

        csharp
        |> reduceIndent
        |> Converter.run 
        |> logConverted
        |> should equal (formatFsharp fsharp)