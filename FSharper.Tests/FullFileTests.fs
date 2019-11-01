namespace Tests

open NUnit.Framework
open FSharper.Core
open FsUnit
open System
open System.IO
open System.Linq
open Swensen.Unquote.Assertions


[<TestFixture>]
type FullFileTests () =

    let formatFsharp (s:string) = 

        let indent = "                "
        s.Split ("\n") |> Array.map (fun x -> if x.StartsWith indent then x.Substring indent.Length else x) |> String.concat "\n"
        |> (fun s -> 
            s
                .Replace("\n    \n", "\n\n")
                .Replace("\n            \n", "\n\n")
                .Trim() )

    [<Test>]
    member this.``mulitple attributes`` () = 
        let csharp = 
             """using System;
                using Android.App;
                using Firebase.Iid;
                using Android.Util;

                namespace FCMClient
                {
                    [Service]
                    [IntentFilter(new[] { "com.google.firebase.INSTANCE_ID_EVENT" })]
                    public class MyFirebaseIIDService : FirebaseInstanceIdService
                    {
                        const string TAG = "MyFirebaseIIDService";
                        public override void OnTokenRefresh()
                        {
                            var refreshedToken = FirebaseInstanceId.Instance.Token;
                            Log.Debug(TAG, "Refreshed token: " + refreshedToken);
                            SendRegistrationToServer(refreshedToken);
                        }
                        void SendRegistrationToServer(string token)
                        {
                            // Add custom implementation, as needed.
                        }
                    }
                }"""

        let fsharp = 
            """namespace FCMClient

                open System
                open Android.App
                open Firebase.Iid
                open Android.Util

                [<Service; IntentFilter([| "com.google.firebase.INSTANCE_ID_EVENT" |])>]
                type MyFirebaseIIDService() =
                    inherit FirebaseInstanceIdService()
                    let TAG = "MyFirebaseIIDService"

                    override this.OnTokenRefresh() =
                        let mutable refreshedToken = FirebaseInstanceId.Instance.Token
                        Log.Debug(TAG, "Refreshed token: " + (refreshedToken.ToString()))
                        this.SendRegistrationToServer(refreshedToken)
                
                    member this.SendRegistrationToServer(token: string) = ()"""
                   
        csharp |> Converter.run 
        |> (fun x -> printfn "%s" x; x)
        |> should equal (formatFsharp fsharp)

    [<Test>]
    member this.``convert full file - namespace with class`` () = 
        let csharp = 
         """using System;
            using System.Collections.Specialized;
            using System.Windows.Input;
            using Android.App;
            using Android.Views;
            using Android.Widget;
            using MvvmCross.Binding.BindingContext;
            using MvvmCross.Navigation;
            using MvvmCross.ViewModels;

            namespace StarWarsSample.Forms.Droid
            {
                // This class is never actually executed, but when Xamarin linking is enabled it does how to ensure types and properties
                // are preserved in the deployed app
                [Android.Runtime.Preserve(AllMembers = true)]
                public class LinkerPleaseInclude
                {
                    public void Include(Button button)
                    {
                        button.Click += (s, e) => button.Text = button.Text + "";
                    }

                public void Include(CheckBox checkBox)
                {
                    checkBox.CheckedChange += (sender, args) => checkBox.Checked = !checkBox.Checked;
                }

                public void Include(View view)
                {
                    view.Click += (s, e) => view.ContentDescription = view.ContentDescription + "";
                }

                public void Include(TextView text)
                {
                    text.AfterTextChanged += (sender, args) => text.Text = "" + text.Text;
                    text.Hint = "" + text.Hint;
                }

                public void Include(CheckedTextView text)
                {
                    text.AfterTextChanged += (sender, args) => text.Text = "" + text.Text;
                    text.Hint = "" + text.Hint;
                }

                public void Include(CompoundButton cb)
                {
                    cb.CheckedChange += (sender, args) => cb.Checked = !cb.Checked;
                }

                public void Include(SeekBar sb)
                {
                    sb.ProgressChanged += (sender, args) => sb.Progress = sb.Progress + 1;
                }

                public void Include(RadioGroup radioGroup)
                {
                    radioGroup.CheckedChange += (sender, args) => radioGroup.Check(args.CheckedId);
                }

                public void Include(RadioButton radioButton)
                {
                    radioButton.CheckedChange += (sender, args) => radioButton.Checked = args.IsChecked;
                }

                public void Include(RatingBar ratingBar)
                {
                    ratingBar.RatingBarChange += (sender, args) => ratingBar.Rating = 0 + ratingBar.Rating;
                }

                public void Include(Activity act)
                {
                    act.Title = act.Title + "";
                }

                public void Include(INotifyCollectionChanged changed)
                {
                    changed.CollectionChanged += (s, e) => { var test = $"{e.Action}{e.NewItems}{e.NewStartingIndex}{e.OldItems}"; };
                }

                public void Include(ICommand command)
                {
                    command.CanExecuteChanged += (s, e) => { if (command.CanExecute(null)) command.Execute(null); };
                }

                public void Include(MvvmCross.IoC.MvxPropertyInjector injector)
                {
                    injector = new MvvmCross.IoC.MvxPropertyInjector();
                }

                public void Include(System.ComponentModel.INotifyPropertyChanged changed)
                {
                    changed.PropertyChanged += (sender, e) =>
                    {
                        var test = e.PropertyName;
                    };
                }

                public void Include(MvxTaskBasedBindingContext context)
                {
                    context.Dispose();
                    var context2 = new MvxTaskBasedBindingContext();
                    context2.Dispose();
                }

                public void Include(MvxNavigationService service, IMvxViewModelLoader loader)
                {
                    service = new MvxNavigationService(null, loader);
                }

                public void Include(ConsoleColor color)
                {
                    Console.Write("");
                    Console.WriteLine("");
                    color = Console.ForegroundColor;
                    Console.ForegroundColor = ConsoleColor.Red;
                    Console.ForegroundColor = ConsoleColor.Yellow;
                    Console.ForegroundColor = ConsoleColor.Magenta;
                    Console.ForegroundColor = ConsoleColor.White;
                    Console.ForegroundColor = ConsoleColor.Gray;
                    Console.ForegroundColor = ConsoleColor.DarkGray;
                }

                public void Include(MvvmCross.Plugin.Json.Plugin plugin)
                {
                    plugin.Load();
                }
            }
        }"""

        let fsharp = 
             """namespace StarWarsSample.Forms.Droid

                open System
                open System.Collections.Specialized
                open System.Windows.Input
                open Android.App
                open Android.Views
                open Android.Widget
                open MvvmCross.Binding.BindingContext
                open MvvmCross.Navigation
                open MvvmCross.ViewModels

                [<Android.Runtime.Preserve(AllMembers = true)>]
                type LinkerPleaseInclude() =
                    member this.Include(button: Button) = button.Click.AddHandler<_>(fun (s, e) -> button.Text <- button.Text + "")
                    member this.Include(checkBox: CheckBox) =
                        checkBox.CheckedChange.AddHandler<_>(fun (sender, args) -> checkBox.Checked <- not checkBox.Checked)
                    member this.Include(view: View) =
                        view.Click.AddHandler<_>(fun (s, e) -> view.ContentDescription <- view.ContentDescription + "")

                    member this.Include(text: TextView) =
                        text.AfterTextChanged.AddHandler<_>(fun (sender, args) -> text.Text <- "" + text.Text)
                        text.Hint <- "" + text.Hint

                    member this.Include(text: CheckedTextView) =
                        text.AfterTextChanged.AddHandler<_>(fun (sender, args) -> text.Text <- "" + text.Text)
                        text.Hint <- "" + text.Hint

                    member this.Include(cb: CompoundButton) =
                        cb.CheckedChange.AddHandler<_>(fun (sender, args) -> cb.Checked <- not cb.Checked)
                    member this.Include(sb: SeekBar) =
                        sb.ProgressChanged.AddHandler<_>(fun (sender, args) -> sb.Progress <- sb.Progress + 1)
                    member this.Include(radioGroup: RadioGroup) =
                        radioGroup.CheckedChange.AddHandler<_>(fun (sender, args) -> radioGroup.Check(args.CheckedId))
                    member this.Include(radioButton: RadioButton) =
                        radioButton.CheckedChange.AddHandler<_>(fun (sender, args) -> radioButton.Checked <- args.IsChecked)
                    member this.Include(ratingBar: RatingBar) =
                        ratingBar.RatingBarChange.AddHandler<_>(fun (sender, args) -> ratingBar.Rating <- 0 + ratingBar.Rating)
                    member this.Include(act: Activity) = act.Title <- act.Title + ""

                    member this.Include(changed: INotifyCollectionChanged) =
                        changed.CollectionChanged.AddHandler<_>(fun (s, e) ->
                            let mutable test = sprintf "%O%O%O%O" (e.Action) (e.NewItems) (e.NewStartingIndex) (e.OldItems)
                            ())

                    member this.Include(command: ICommand) =
                        command.CanExecuteChanged.AddHandler<_>(fun (s, e) ->
                            if command.CanExecute(null) then command.Execute(null))

                    member this.Include(injector: MvvmCross.IoC.MvxPropertyInjector) =
                        injector <- new MvvmCross.IoC.MvxPropertyInjector()

                    member this.Include(changed: System.ComponentModel.INotifyPropertyChanged) =
                        changed.PropertyChanged.AddHandler<_>(fun (sender, e) ->
                            let mutable test = e.PropertyName
                            ())

                    member this.Include(context: MvxTaskBasedBindingContext) =
                        context.Dispose()
                        let mutable context2 = new MvxTaskBasedBindingContext()
                        context2.Dispose()

                    member this.Include(service: MvxNavigationService, loader: IMvxViewModelLoader) =
                        service <- new MvxNavigationService(null, loader)

                    member this.Include(color: ConsoleColor) =
                        Console.Write("")
                        Console.WriteLine("")
                        color <- Console.ForegroundColor
                        Console.ForegroundColor <- ConsoleColor.Red
                        Console.ForegroundColor <- ConsoleColor.Yellow
                        Console.ForegroundColor <- ConsoleColor.Magenta
                        Console.ForegroundColor <- ConsoleColor.White
                        Console.ForegroundColor <- ConsoleColor.Gray
                        Console.ForegroundColor <- ConsoleColor.DarkGray

                    member this.Include(plugin: MvvmCross.Plugin.Json.Plugin) = plugin.Load()""" 

        csharp |> Converter.run 
        |> (fun x -> printfn "%s" x; x)
        |> should equal (formatFsharp fsharp)


    [<Test>]
    member this.``parse mutliple classes`` () = 
        let csharp = 
             """public class QuestStarted
                {
                    public string Name { get; set; }
                    public Guid Id { get; set; }

                    public override string ToString()
                    {
                        return $"Quest {Name} started";
                    }
                }

                public class QuestEnded
                {
                    public string Name { get; set; }
                    public Guid Id { get; set; }

                    public override string ToString()
                    {
                        return $"Quest {Name} ended";
                    }
                }"""
    
        let fsharp = 
             """type QuestStarted() =
                    member val Name: string = Unchecked.defaultof<string> with get, set
                    member val Id: Guid = Unchecked.defaultof<Guid> with get, set
                    override this.ToString(): string = sprintf "Quest %O started" (Name)

                type QuestEnded() =
                    member val Name: string = Unchecked.defaultof<string> with get, set
                    member val Id: Guid = Unchecked.defaultof<Guid> with get, set
                    override this.ToString(): string = sprintf "Quest %O ended" (Name)"""

        csharp |> Converter.runWithConfig false 
        |> (fun x -> printfn "%s" x; x)
        |> should equal (formatFsharp fsharp)

    [<Test>]
    member this.``parse multiple classes with odd formatting`` () = 
        let csharp = 
             """public interface IService {
                    void Serve();
                }
                public class Service1 : IService {
                    public void Serve() { Console.WriteLine("Service1 Called"); }
                }
                public class Service2 : IService {
                    public void Serve() { Console.WriteLine("Service2 Called"); }
                }
                public class Client {
                    private IService _service;
                    public Client(IService service) {
                        this._service = service;
                    }
                    public void ServeMethod() { this._service.Serve(); }
                }"""
    
        let fsharp = 
             """type IService =
                    abstract Serve: unit -> unit
 
                type Service1() =
                    member this.Serve() = Console.WriteLine("Service1 Called")
                    interface IService with
                        member this.Serve() = this.Serve()

                type Service2() =
                    member this.Serve() = Console.WriteLine("Service2 Called")
                    interface IService with
                        member this.Serve() = this.Serve()
                        
                type Client(_service: IService) =
                    member this.ServeMethod() = this._service.Serve()"""

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
    member this.``Correct order of classes`` () = 
        let csharp = 
             """public class Bar
                {
                    public Foo GetFoo()
                    {
                        return new Foo().MagicNumber;
                    }
                }

                public class Foo
                {
                    public int MagicNumber()
                    {
                        return 42;
                    }
                }
                 """
    
        let fsharp = 
             """type Foo() =
                    member this.MagicNumber(): int = 42

                type Bar() =
                    member this.GetFoo(): Foo = (new Foo()).MagicNumber"""

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
    member this.``fixed complex class ordering`` () = 
        let csharp = 
             """public class D : C
                {
                }

                public class C : B
                {
                }

                public class B : A
                {
                }

                public class A
                {
                }"""
    
        let fsharp = 
             """type A() =

                type B() =
                    inherit A()
                
                type C() =
                    inherit B()

                type D() =
                    inherit C()"""

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
    member this.``fixed two sets of related classes`` () = 
        let csharp = 
             """public class D : C
                {
                }

                public class C : B
                {
                }

                public class B
                {
                }

                public class A
                {
                }

                public class X : Z
                {
                }
                
                public class Z
                {
                }"""
    
        let fsharp = 
             """type B() =

                type A() =

                type Z() =
                
                type C() =
                    inherit B()

                type X() =
                    inherit Z()

                type D() =
                    inherit C()"""

        csharp |> Converter.runWithConfig false
        |> (fun x -> printfn "%s" x; x)
        |> should equal (formatFsharp fsharp)  

    [<Test>]
    member this.``reoder main method and claseses`` () = 
        let csharp = 
             """using System;
                using System.Collections.Generic;
                using System.Linq;
                using System.Text;

                namespace Inheritance {
                   class Test {
                      static void Main(string[] args) {
                         Father f = new Father();
                         f.display();

                         Son s = new Son();
                         s.display();
                         s.displayOne();

                         Daughter d = new Daughter();
                         d.displayTwo();

                         Console.ReadKey();
                      }

                      class Father {
                         public void display() {
                            Console.WriteLine("Display...");
                         }
                      }

                      class Son : Father {
                         public void displayOne() {
                            Console.WriteLine("Display One");
                         }
                      }

                      class Daughter : Father {
                         public void displayTwo() {
                            Console.WriteLine("Display Two");
                         }
                      }
                   }
                }"""
    
        let fsharp = 
             """namespace Inheritance

                open System
                open System.Collections.Generic
                open System.Linq
                open System.Text
                
                type Father() =
                    member this.display() = Console.WriteLine("Display...")
                
                type Son() =
                    inherit Father()
                    member this.displayOne() = Console.WriteLine("Display One")
                
                type Daughter() =
                    inherit Father()
                    member this.displayTwo() = Console.WriteLine("Display Two")
                
                type Test() =
                    static member Main(args: string []) =
                        let mutable f = new Father()
                        f.display()
                        let mutable s = new Son()
                        s.display()
                        s.displayOne()
                        let mutable d = new Daughter()
                        d.displayTwo()
                        Console.ReadKey()"""

        csharp |> Converter.run 
        |> (fun x -> printfn "%s" x; x)
        |> should equal (formatFsharp fsharp) 