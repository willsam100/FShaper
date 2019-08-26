namespace Tests

open NUnit.Framework
open FSharper.Core
open FsUnit
open System
open System.IO
open System.Linq
open Swensen.Unquote.Assertions

[<TestFixture>]
type ConversionTests () =

    let formatFsharp (s:string) = 

        let indent = "                "
        s.Split ("\n") |> Array.map (fun x -> if x.StartsWith indent then x.Substring indent.Length else x) |> String.concat "\n"
        |> (fun s -> 
            s
                .Replace("\n    \n", "\n\n")
                .Replace("\n            \n", "\n\n")
                .Trim() )

    [<Test>]
    member this.``using statements convert to open statements`` () = 
        """using MvvmCross.Forms.Views;
           using TipCalc.Core.ViewModels; """ 
        |> Converter.run |> should equal (
            formatFsharp 
               "open MvvmCross.Forms.Views
                open TipCalc.Core.ViewModels")


    [<Test>]
    member this.``array initlization`` () = 
        let csharp = 
             """long[] c = new long[100];"""

        let fsharp = 
            """let mutable c = Array.zeroCreate<int64> (100)
                ()"""
                   
        csharp |> Converter.run 
        |> (fun x -> printfn "%s" x; x)
        |> should equal (formatFsharp fsharp)

    [<Test>]
    member this.``array initlization with values`` () = 
        let csharp = 
             """string[] values = { "12", "31.", "5.8:32:16", "12:12:15.95", ".12"};"""

        let fsharp = 
             """let mutable values = [| "12"; "31."; "5.8:32:16"; "12:12:15.95"; ".12" |]
                ()"""
                   
        csharp |> Converter.run 
        |> (fun x -> printfn "%s" x; x)
        |> should equal (formatFsharp fsharp)        

    [<Test>]
    member this.``parse chars`` () = 
        // https://devblogs.microsoft.com/csharpfaq/what-character-escape-sequences-are-available/
        let csharp = 
             """void Foo() 
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
             """member this.Foo() =
                    Console.WriteLine('a')
                    Console.WriteLine('\n')
                    Console.WriteLine('\t')
                    Console.WriteLine('\r')
                    Console.WriteLine('\b')
                    Console.WriteLine('\'')
                    Console.WriteLine('\\')"""
                   
        csharp |> Converter.run 
        |> (fun x -> printfn "%s" x; x)
        |> should equal (formatFsharp fsharp)

    [<Test>]
    member this.``array update with nested assignment`` () = 
        let csharp = 
             """c[i = n] -= 1;"""

        let fsharp = 
             """i <- n
                c.[i] <- c.[i] - 1"""
                   
        csharp |> Converter.run 
        |> (fun x -> printfn "%s" x; x)
        |> should equal (formatFsharp fsharp)

    [<Test>]
    member this.``modulo operation`` () = 
        let csharp = 
             """int Foo() { c[i] % n; }"""

        let fsharp = 
             """member this.Foo(): int = c.[i] % n"""
                   
        csharp |> Converter.run 
        |> (fun x -> printfn "%s" x; x)
        |> should equal (formatFsharp fsharp)

    [<Test>]
    member this.``yeild statment`` () = 
        let csharp = 
             """IEnumerable<int> Foo() { yield return 10; }"""

        let fsharp = 
             """member this.Foo(): seq<int> = seq { yield 10 }"""
                   
        csharp |> Converter.run 
        |> (fun x -> printfn "%s" x; x)
        |> should equal (formatFsharp fsharp)

    [<Test>]
    member this.``conditional statment`` () = 
        let csharp = 
             """string Foo() { return (c == 1 ? "" : "s"); }"""

        let fsharp = 
             """member this.Foo(): string =
                    if c = 1 then ""
                    else "s" """
                   
        csharp |> Converter.run 
        |> (fun x -> printfn "%s" x; x)
        |> should equal (formatFsharp fsharp)

    [<Test>]
    member this.``adding to string is escaped`` () = 
        let csharp = 
             """void Foo() { Console.Write("Hello, " + n); }"""

        let fsharp = 
             """member this.Foo() = Console.Write("Hello, " + (n.ToString()))"""
                   
        csharp |> Converter.run 
        |> (fun x -> printfn "%s" x; x)
        |> should equal (formatFsharp fsharp)
        

    [<Test>]
    member this.``Can convert property and auto property`` () = 
        let csharp = 
             """using MvvmCross.Forms.Views;
                using TipCalc.Core.ViewModels;

                namespace TipCalc.Forms.UI.Pages
                {
                    public class TipView
                    {
                        private int _foo = 34;
                        public int Foo { 
                            get { return _foo; } 
                            set {
                                _foo = value;
                                RaisePropertyChanged("Foo");
                            } }

                        public Baz Bar { get; set; }
                        public Baz FooBar { get { return _foo; }}                        
                    }
                } """
            
        let fsharp = 
            """namespace TipCalc.Forms.UI.Pages

                open MvvmCross.Forms.Views
                open TipCalc.Core.ViewModels

                type TipView() =
                    let mutable _foo = 34

                    member this.Foo
                        with get () = _foo
                        and set value =
                            _foo <- value
                            RaisePropertyChanged("Foo")

                    member val Bar: Baz = Unchecked.defaultof<Baz> with get, set
                    member this.FooBar = _foo"""

        csharp |> Converter.run 
        |> (fun x -> printfn "%s" x; x)
        |> should equal (formatFsharp fsharp)


    [<Test>]
    member this.``Can convert class method`` () = 
        let csharp = 
            """public void Foo(Class a, HudidIt fifity) {
                    Console.WriteLine("Hello, world");
                } """

        let fsharp = 
            """member this.Foo(a: Class, fifity: HudidIt) = Console.WriteLine("Hello, world")"""
                   
        csharp |> Converter.run 
        |> (fun x -> printfn "%s" x; x)
        |> should equal (formatFsharp fsharp)

    [<Test>]
    member this.``can convert if statements`` () = 
        let csharp = 
            """[CommandHandler (RefactoryCommands.QuickFix)]
                void OnQuickFixCommand ()
                {
                    if (!AnalysisOptions.EnableFancyFeatures || smartTagMarginMarker == null) 
                    {
                        //Fixes = RefactoringService.GetValidActions (Editor, DocumentContext, Editor.CaretLocation).Result;
                        PopupQuickFixMenu (null, null);
                        return;
                    }

                    CancelSmartTagPopupTimeout ();
                    PopupQuickFixMenu (null, menu => { });
                
                }"""

        let fsharp = 
            """[<CommandHandler(RefactoryCommands.QuickFix)>]
                member this.OnQuickFixCommand() =
                    if not AnalysisOptions.EnableFancyFeatures || smartTagMarginMarker = null then
                        PopupQuickFixMenu(null, null)
                        ()
                    else
                        CancelSmartTagPopupTimeout()
                        PopupQuickFixMenu(null, fun menu -> ())"""
                   
        csharp |> Converter.run 
        |> (fun x -> printfn "%s" x; x)
        |> should equal (formatFsharp fsharp)

    [<Test>]
    member this.``single line`` () = 
        let csharp = 
            """AppCenter.Start("cf32a57f-60ce-4a42-aac1-a84148c29b0d", typeof(Push));"""

        let fsharp = 
            """AppCenter.Start("cf32a57f-60ce-4a42-aac1-a84148c29b0d", typeof<Push>)"""
                   
        csharp |> Converter.run 
        |> (fun x -> printfn "%s" x; x)
        |> should equal (formatFsharp fsharp)
        
    [<Test>]
    member this.``create class instance`` () = 
        let csharp = 
             """void SendNotification(string messageBody)
                {
                    var intent = new Intent(this, typeof(MainActivity));
                    intent.AddFlags(ActivityFlags.ClearTop);
                    var pendingIntent = PendingIntent.GetActivity(this, 0, intent, PendingIntentFlags.OneShot);

                    var notificationBuilder = new Notification.Builder(this)
                                .SetContentTitle("FCM Message")
                                .SetSmallIcon(Resource.Drawable.ic_launcher)
                                .SetContentText(messageBody)
                                .SetAutoCancel(true)
                                .SetContentIntent(pendingIntent);

                    var notificationManager = NotificationManager.FromContext(this);
                    notificationManager.Notify(0, notificationBuilder.Build());
                }"""

        let fsharp = 
             """member this.SendNotification(messageBody: string) =
                    let mutable intent = new Intent(this, typeof<MainActivity>)
                    intent.AddFlags(ActivityFlags.ClearTop)
                    let mutable pendingIntent = PendingIntent.GetActivity(this, 0, intent, PendingIntentFlags.OneShot)
                    let mutable notificationBuilder =
                        (new Notification.Builder(this)).SetContentTitle("FCM Message").SetSmallIcon(Resource.Drawable.ic_launcher)
                            .SetContentText(messageBody)
                            .SetAutoCancel(true)
                            .SetContentIntent(pendingIntent)
                    let mutable notificationManager = NotificationManager.FromContext(this)
                    notificationManager.Notify(0, notificationBuilder.Build())"""
                   
        csharp |> Converter.run 
        |> (fun x -> printfn "%s" x; x)
        |> should equal (formatFsharp fsharp)


    [<Test>]
    member this.``convert const`` () = 
        let csharp = 
             """class CodeActionEditorExtension : TextEditorExtension
                {
                    const int menuTimeout = 150;
                }"""

        let fsharp = 
            """type CodeActionEditorExtension() =
                    inherit TextEditorExtension()
                    let menuTimeout = 150"""
                   
        csharp |> Converter.run 
        |> (fun x -> printfn "%s" x; x)
        |> should equal (formatFsharp fsharp)

    [<Test>]
    member this.``convert on clicked handler`` () = 
        let csharp = 
             """ContextMenu CreateContextMenu (CodeFixMenu entrySet)
                {
                    _menuItem.Clicked += (sender, e) => sender.Foo();
                }"""

        let fsharp = 
             """member this.CreateContextMenu(entrySet: CodeFixMenu): ContextMenu =
                    _menuItem.Clicked.AddHandler<_>(fun (sender, e) -> sender.Foo())"""
                   
        csharp |> Converter.run 
        |> (fun x -> printfn "%s" x; x)
        |> should equal (formatFsharp fsharp)

    [<Test>]
    member this.``convert if else statement with an early return in else`` () = 
        let csharp = 
             """public int Foo()
                {
                    if (x)
                    {
                        SomeAction();
                    } 
                    else {
                        return f;
                    }
    
                    return 0;
                }"""

        let fsharp = 
             """member this.Foo(): int =
                    if x then
                        SomeAction()
                        0
                    else f"""
                       
        csharp |> Converter.run 
        |> (fun x -> printfn "%s" x; x)
        |> should equal (formatFsharp fsharp)

    [<Test>]
    member this.``convert if else statement with early return if first if`` () = 
        let csharp = 
             """public int Foo()
                {
                    if (x)
                        return 0;

                    Bar();
                    return 1;
                }"""

        let fsharp = 
             """member this.Foo(): int =
                    if x then 0
                    else
                        Bar()
                        1"""
                       
        csharp |> Converter.run 
        |> (fun x -> printfn "%s" x; x)
        |> should equal (formatFsharp fsharp)

    [<Test>]
    member this.``convert method attributes`` () = 
        let csharp = 
             """[CommandHandler (RefactoryCommands.QuickFix)]
                void OnQuickFixCommand ()
                {
                    if (!AnalysisOptions.EnableFancyFeatures || smartTagMarginMarker == null) {
                        //Fixes = RefactoringService.GetValidActions (Editor, DocumentContext, Editor.CaretLocation).Result;

                        PopupQuickFixMenu (null, null);
                        return;
                    }

                    CancelSmartTagPopupTimeout ();
                    PopupQuickFixMenu (null, menu => { });
                }"""

        let fsharp = 
             """[<CommandHandler(RefactoryCommands.QuickFix)>]
                member this.OnQuickFixCommand() =
                    if not AnalysisOptions.EnableFancyFeatures || smartTagMarginMarker = null then
                        PopupQuickFixMenu(null, null)
                        ()
                    else
                        CancelSmartTagPopupTimeout()
                        PopupQuickFixMenu(null, fun menu -> ())"""
                       
        csharp |> Converter.run 
        |> (fun x -> printfn "%s" x; x)
        |> should equal (formatFsharp fsharp)

    [<Test>]
    member this.``convert multiple if statements with early returns`` () = 
        let csharp = 
             """public int Foo()
                {
                    if (x)
                        return 0;

                    Baz();
                    if (y)
                    {
                        return 42;
                    }
                    else 
                    {
                        Bar();
                    }
                                        
                    return 1;
                }"""

        let fsharp = 
             """member this.Foo(): int =
                    if x then 0
                    else
                        Baz()
                        if y then 42
                        else Bar()
                        1"""
                       
        csharp |> Converter.run 
        |> (fun x -> printfn "%s" x; x)
        |> should equal (formatFsharp fsharp)

    [<Test>]
    member this.``convert convert not statement`` () = 
        let csharp = 
            """
                public bool IsPlayServicesAvailable ()
                {
                    int resultCode = GoogleApiAvailability.Instance.IsGooglePlayServicesAvailable (this);
                    if (resultCode != ConnectionResult.Success)
                    {
                        if (GoogleApiAvailability.Instance.IsUserResolvableError (resultCode))
                            msgText.Text = GoogleApiAvailability.Instance.GetErrorString (resultCode);
                        else
                        {
                            msgText.Text = "This device is not supported";
                            Finish ();
                        }
                        return false;
                    }
                    else
                    {
                        msgText.Text = "Google Play Services is available.";
                        return true;
                    }
                }"""

        let fsharp = 
             """member this.IsPlayServicesAvailable(): bool =
                    let mutable resultCode = GoogleApiAvailability.Instance.IsGooglePlayServicesAvailable(this)
                    if resultCode <> ConnectionResult.Success then
                        if GoogleApiAvailability.Instance.IsUserResolvableError(resultCode) then
                            msgText.Text <- GoogleApiAvailability.Instance.GetErrorString(resultCode)
                        else
                            msgText.Text <- "This device is not supported"
                            Finish()
                        false
                    else
                        msgText.Text <- "Google Play Services is available."
                        true"""
                       
        csharp |> Converter.run 
        |> (fun x -> printfn "%s" x; x)
        |> should equal (formatFsharp fsharp)

    [<Test>]
    member this.``convert convert lambda`` () = 
        let csharp = 
             """public void Include(INotifyCollectionChanged changed)
                {
                    changed.CollectionChanged += (s, e) => { var test = $"Args: {e.Action}{e.NewItems}{e.OldItems}, Index: {e.OldStartingIndex}"; };
                }"""

        let fsharp = 
             """member this.Include(changed: INotifyCollectionChanged) =
                    changed.CollectionChanged.AddHandler<_>(fun (s, e) ->
                        let mutable test =
                            sprintf "Args: %O%O%O, Index: %O" (e.Action) (e.NewItems) (e.OldItems) (e.OldStartingIndex)
                        ())"""
                       
        csharp |> Converter.run 
        |> (fun x -> printfn "%s" x; x)
        |> should equal (formatFsharp fsharp)

    [<Test>]
    member this.``convert foreach with break`` () = 
        let csharp = 
             """public int Foo(IEnumerable<int> myList)
                {
                    var x = 0;
                    foreach (var i in myList)
                    {
                        if (x >= 10) 
                            break;

                        x += i;
                    }
                    return x;
                }"""

        let fsharp = 
             """member this.Foo(myList: seq<int>): int =
                    let mutable x = 0
                    for i in myList do
                        if x >= 10 then ()
                        else x <- x + i
                    x"""

        csharp |> Converter.run 
        |> (fun x -> printfn "%s" x; x)
        |> should equal (formatFsharp fsharp)

    [<Test>]
    member this.``convert foreach with continue`` () = 
        let csharp = 
             """public int Foo(IEnumerable<int> myList)
                {
                    var x = 0;
                    foreach (var i in myList)
                    {
                        if (x == 10) 
                            continue;

                        x += i;
                    }
                    return x;
                }"""

        let fsharp = 
             """member this.Foo(myList: seq<int>): int =
                    let mutable x = 0
                    for i in myList do
                        if x = 10 then ()
                        else x <- x + i
                    x"""

        csharp |> Converter.run 
        |> (fun x -> printfn "%s" x; x)
        |> should equal (formatFsharp fsharp)

    [<Test>]
    member this.``convert private method`` () = 
        let csharp = 
             """public class Foo
                {
                    private void Foo() 
                    {
                        return;
                    }
                }"""

        let fsharp = 
             """type Foo() =
                    member private this.Foo() = ()"""

        csharp |> Converter.run 
        |> (fun x -> printfn "%s" x; x)
        |> should equal (formatFsharp fsharp)

    [<Test>]
    member this.``convert object type to obj`` () = 
        let csharp = 
             """public class Foo
                {
                    public Foo(object oo) { }
                    
                    public void Foo(object o)
                    {
                        var x = (object) o;
                        return;
                    }
                }"""

        let fsharp = 
             """type Foo(oo: obj) =
                    member this.Foo(o: obj) =
                        let mutable x = (o :?> obj)
                        ()"""

        csharp |> Converter.run 
        |> (fun x -> printfn "%s" x; x)
        |> should equal (formatFsharp fsharp)

    [<Test>]
    member this.``convert continue and foreach loop`` () = 
        let csharp = 
             """ContextMenu CreateContextMenu (CodeFixMenu entrySet)
                {
                    var menu = new ContextMenu ();
                    foreach (var item in entrySet.Items) {
                        if (item == CodeFixMenuEntry.Separator) {
                            menu.Items.Add (new SeparatorContextMenuItem ());
                            continue;
                        }

                        var _menuItem = new ContextMenuItem (item.Label);
                        _menuItem.Context = item.Action;
                        if (item.Action == null) {
                            if (!(item is CodeFixMenu itemAsMenu) || itemAsMenu.Items.Count <= 0) {
                                _menuItem.Sensitive = false;
                            }
                        }
                        var subMenu = item as CodeFixMenu;
                        if (subMenu != null) {
                            _menuItem.SubMenu = CreateContextMenu (subMenu);
                            _menuItem.Selected += delegate {
                                RefactoringPreviewTooltipWindow.HidePreviewTooltip ();
                            };
                            _menuItem.Deselected += delegate { RefactoringPreviewTooltipWindow.HidePreviewTooltip (); };
                        } else {
                            _menuItem.Clicked += (sender, e) => ((System.Action)((ContextMenuItem)sender).Context) ();
                            _menuItem.Selected += (sender, e) => {
                                RefactoringPreviewTooltipWindow.HidePreviewTooltip ();
                                if (item.ShowPreviewTooltip != null) {
                                    item.ShowPreviewTooltip (e);
                                }
                            };
                            _menuItem.Deselected += delegate { RefactoringPreviewTooltipWindow.HidePreviewTooltip (); };
                        }
                        menu.Items.Add (_menuItem);
                    }
                    menu.Closed += delegate { RefactoringPreviewTooltipWindow.HidePreviewTooltip (); };
                    return menu;
                }"""
    
        let fsharp = 
             """member this.CreateContextMenu(entrySet: CodeFixMenu): ContextMenu =
                    let mutable menu = new ContextMenu()
                    for item in entrySet.Items do
                        if item = CodeFixMenuEntry.Separator then
                            menu.Items.Add(new SeparatorContextMenuItem())
                            ()
                        else
                            let mutable _menuItem = new ContextMenuItem(item.Label)
                            _menuItem.Context <- item.Action
                            if item.Action = null then
                                match item with
                                | :? CodeFixMenu as itemAsMenu when itemAsMenu.Items.Count > 0 -> ()
                                | _ -> _menuItem.Sensitive <- false
                            else
                                let mutable subMenu = item :?> CodeFixMenu
                                if subMenu <> null then
                                    _menuItem.SubMenu <- this.CreateContextMenu(subMenu)
                                    _menuItem.Selected.AddHandler<_>(fun () -> RefactoringPreviewTooltipWindow.HidePreviewTooltip())
                                    _menuItem.Deselected.AddHandler<_>
                                        (fun () -> RefactoringPreviewTooltipWindow.HidePreviewTooltip())
                                else
                                    _menuItem.Clicked.AddHandler<_>
                                        (fun (sender, e) -> ((sender :?> ContextMenuItem).Context :?> System.Action).Invoke())
                                    _menuItem.Selected.AddHandler<_>(fun (sender, e) ->
                                        RefactoringPreviewTooltipWindow.HidePreviewTooltip()
                                        if item.ShowPreviewTooltip <> null then item.ShowPreviewTooltip(e))
                                    _menuItem.Deselected.AddHandler<_>
                                        (fun () -> RefactoringPreviewTooltipWindow.HidePreviewTooltip())
                                menu.Items.Add(_menuItem)
                    menu.Closed.AddHandler<_>(fun () -> RefactoringPreviewTooltipWindow.HidePreviewTooltip())
                    menu"""
                   
        csharp |> Converter.run 
        |> (fun x -> printfn "%s" x; x)
        |> should equal (formatFsharp fsharp)

    [<Test>]
    member this.``convert invocation on cast to Action to Invoke()`` () = 
        let csharp = 
             """public void CreateContextMenu (object entrySet)
                {
                    ((Action)entrySet) ();
                }"""
    
        let fsharp = 
             """member this.CreateContextMenu(entrySet: obj) = (entrySet :?> Action).Invoke()"""
                   
        csharp |> Converter.run 
        |> (fun x -> printfn "%s" x; x)
        |> should equal (formatFsharp fsharp) 

    [<Test>]
    member this.``convert invocation on cast to Action args to invoke args`` () = 
        let csharp = 
             """public void CreateContextMenu (object entrySet)
                {
                    ((Action<int>)entrySet)(42);
                }"""
    
        let fsharp = 
             """member this.CreateContextMenu(entrySet: obj) = (entrySet :?> Action<int>).Invoke(42)"""
                   
        csharp |> Converter.run 
        |> (fun x -> printfn "%s" x; x)
        |> should equal (formatFsharp fsharp) 

    [<Test>]
    member this.``convert invocation on cast to Func to Invoke()`` () = 
        let csharp = 
             """public int CreateContextMenu (object entrySet)
                {
                    ((Func<int>)entrySet) ();
                }"""
    
        let fsharp = 
             """member this.CreateContextMenu(entrySet: obj): int = (entrySet :?> Func<int>).Invoke()"""
                   
        csharp |> Converter.run 
        |> (fun x -> printfn "%s" x; x)
        |> should equal (formatFsharp fsharp)   

    [<Test>]
    member this.``convert invocation on cast to Func args to Invoke args`` () = 
        let csharp = 
             """public int CreateContextMenu (object entrySet)
                {
                    return ((Func<int,int>)entrySet)(42);
                }"""
    
        let fsharp = 
             """member this.CreateContextMenu(entrySet: obj): int = (entrySet :?> Func<int, int>).Invoke(42)"""
                   
        csharp |> Converter.run 
        |> (fun x -> printfn "%s" x; x)
        |> should equal (formatFsharp fsharp)

    [<Test>]
    member this.``convert try catch`` () = 
        let csharp = 
             """public class Sample
                {
                    public static void Main() {
                        String myString = "abc";
                        bool test1 = myString.Substring(2, 1).Equals("c"); // This is true.
                        Console.WriteLine(test1);
                        bool test2 = String.IsNullOrEmpty(myString.Substring(3, 0)); // This is true.
                        Console.WriteLine(test2);
                        try {
                           string str3 = myString.Substring(3, 1); // This throws ArgumentOutOfRangeException.
                           Console.WriteLine(str3);
                        }
                        catch (ArgumentOutOfRangeException e) {
                           Console.WriteLine(e.Message);
                        }         
                    }
                }"""
    
        let fsharp = 
             """type Sample() =
                    static member Main() =
                        let mutable myString = "abc"
                        let mutable test1 = myString.Substring(2, 1).Equals("c")
                        Console.WriteLine(test1)
                        let mutable test2 = String.IsNullOrEmpty(myString.Substring(3, 0))
                        Console.WriteLine(test2)
                        try
                            let mutable str3 = myString.Substring(3, 1)
                            Console.WriteLine(str3)
                        with :? ArgumentOutOfRangeException as e -> Console.WriteLine(e.Message)"""

        csharp |> Converter.runWithConfig false 
        |> (fun x -> printfn "%s" x; x)
        |> should equal (formatFsharp fsharp)

    [<Test>]
    member this.``convert try catch with multiple exception matchs`` () = 
        let csharp = 
             """public void Main() {
                    try {
                       string str3 = myString.Substring(3, 1); // This throws ArgumentOutOfRangeException.
                       Console.WriteLine(str3);
                    }
                    catch (ArgumentOutOfRangeException e) {
                       Console.WriteLine(e.Message);
                    }  
                    catch (Exception e) {
                       Console.WriteLine(e.Message);
                    }         
                }"""
    
        let fsharp = 
             """member this.Main() =
                    try
                        let mutable str3 = myString.Substring(3, 1)
                        Console.WriteLine(str3)
                    with
                    | :? ArgumentOutOfRangeException as e -> Console.WriteLine(e.Message)
                    | :? Exception as e -> Console.WriteLine(e.Message)"""

        csharp |> Converter.runWithConfig false 
        |> (fun x -> printfn "%s" x; x)
        |> should equal (formatFsharp fsharp)

    [<Test>]
    member this.``convert try catch without named exception`` () = 
        let csharp = 
             """public void Main() {
                    try {
                       string str3 = myString.Substring(3, 1); // This throws ArgumentOutOfRangeException.
                       Console.WriteLine(str3);
                    }
                    catch (ArgumentOutOfRangeException) {
                       Console.WriteLine("Oops");
                    }         
                }"""
    
        let fsharp = 
             """member this.Main() =
                    try
                        let mutable str3 = myString.Substring(3, 1)
                        Console.WriteLine(str3)
                    with :? ArgumentOutOfRangeException -> Console.WriteLine("Oops")"""

        csharp |> Converter.runWithConfig false 
        |> (fun x -> printfn "%s" x; x)
        |> should equal (formatFsharp fsharp)

    [<Test>]
    member this.``convert using block`` () = 
        let csharp = 
             """public static byte[] ReadFully(Stream input)
                {
                    byte[] buffer = new byte[16*1024];
                    using (MemoryStream ms = new MemoryStream())
                    {
                        int read;
                        while ((read = input.Read(buffer, 0, buffer.Length)) > 0)
                        {
                            ms.Write(buffer, 0, read);
                        }
                        return ms.ToArray();
                    }
                }"""
    
        let fsharp = 
             """static member ReadFully(input: Stream): byte [] =
                    let mutable buffer = Array.zeroCreate<byte> (16 * 1024)
                    use ms = new MemoryStream()
                    let mutable read = Unchecked.defaultof<int>
                    read <- input.Read(buffer, 0, buffer.Length)
                    while read > 0 do
                        ms.Write(buffer, 0, read)
                        read <- input.Read(buffer, 0, buffer.Length)
                    ms.ToArray()"""

        csharp |> Converter.runWithConfig false 
        |> (fun x -> printfn "%s" x; x)
        |> should equal (formatFsharp fsharp)

    [<Test>]
    member this.``single statements`` () = 
        let csharp = 
             """var credentials = new StoredProfileAWSCredentials(profileName);
                var s3Client = new AmazonS3Client(credentials, RegionEndpoint.USWest2);"""
    
        let fsharp = 
             """let mutable credentials = new StoredProfileAWSCredentials(profileName)
                let mutable s3Client = new AmazonS3Client(credentials, RegionEndpoint.USWest2)
                ()"""

        csharp |> Converter.runWithConfig false 
        |> (fun x -> printfn "%s" x; x)
        |> should equal (formatFsharp fsharp)


    [<Test>]
    member this.``parse params keyword`` () = 
        let csharp = 
             """public class MembersJoined
                {

                    public MembersJoined()
                    {
                    }

                    public MembersJoined(int day, string location, params string[] members)
                    {
                        Day = day;
                        Location = location;
                        Members = members;
                    }

                    public Guid QuestId { get; set; }
                    public int Day { get; set; }
                    public string Location { get; set; }
                    public string[] Members { get; set; }
                    public override string ToString()
                    {
                        return $"Members {Members.Join(", ")} joined at {Location} on Day {Day}";
                    }
                }"""

        let fsharp = 
             """type MembersJoined(day: int, location: string, [<ParamArray>] members: string []) =

                    do
                        Day <- day
                        Location <- location
                        Members <- members

                    member val QuestId: Guid = Unchecked.defaultof<Guid> with get, set
                    member val Day: int = Unchecked.defaultof<int> with get, set
                    member val Location: string = Unchecked.defaultof<string> with get, set
                    member val Members: string [] = Unchecked.defaultof<string []> with get, set
                    override this.ToString(): string = sprintf "Members %O joined at %O on Day %O" (Members.Join(", ")) (Location) (Day)"""

        csharp |> Converter.runWithConfig false 
        |> (fun x -> printfn "%s" x; x)
        |> should equal (formatFsharp fsharp)



    [<Test>]
    member this.``incomplete recursive static method`` () = 
        let csharp = 
             """static int fib(int n)
                {
                    if (n == 0 || n == 1)
                        return n;

                    return fib(n - 1) + fib(n - 2);
                }"""
    
        let fsharp = 
             """static member fib (n: int): int =
                    if n = 0 || n = 1 then n
                    else fib (n - 1) + fib (n - 2)"""

        csharp |> Converter.runWithConfig false 
        |> (fun x -> printfn "%s" x; x)
        |> should equal (formatFsharp fsharp)

        
    [<Test>]
    member this.``method with params keyword`` () = 
        let csharp = 
             """public static void UseParams(params int[] myList)
                {
                    Console.WriteLine();
                }"""
    
        let fsharp = 
             """static member UseParams(([<ParamArray>] myList: int [])) = Console.WriteLine()"""

        csharp |> Converter.runWithConfig false 
        |> (fun x -> printfn "%s" x; x)
        |> should equal (formatFsharp fsharp)  


    [<Test>]
    member this.``convert expression body with CoalesceExpression`` () = 
        let csharp = 
             """public class HzzoMedsDownloadDto
                {
                    public string FileName => 
                        ValidFrom.ToString("yyyy-MM-dd_") +
                        (Href.Split('/').LastOrDefault() ?? Href.Replace("/", "_").Replace(":", "_")).TrimEnd();
                }"""

        let fsharp = 
             """type HzzoMedsDownloadDto() =
                    member this.FileName =
                        ValidFrom.ToString("yyyy-MM-dd_") + (Href.Split('/').LastOrDefault()
                                                             |> Option.ofObj
                                                             |> Option.defaultValue Href.Replace("/", "_").Replace(":", "_")).TrimEnd()"""

        csharp |> Converter.runWithConfig false 
        |> (fun x -> printfn "%s" x; x)
        |> should equal (formatFsharp fsharp)  

    [<Test>]
    member this.``convert generic types`` () = 
        let csharp = 
             """static ISet<HzzoMedsDownloadDto> ParseMedsLiElements(IEnumerable<IElement> elems) =>
                  elems.Aggregate(new HashSet<HzzoMedsDownloadDto>(), (medsList, li) => medsList);"""

        let fsharp = 
             """static member ParseMedsLiElements(elems: seq<IElement>): ISet<HzzoMedsDownloadDto> =
                    elems.Aggregate(new HashSet<HzzoMedsDownloadDto>(), fun (medsList, li) -> medsList)"""

        csharp |> Converter.runWithConfig false 
        |> (fun x -> printfn "%s" x; x)
        |> should equal (formatFsharp fsharp)

    [<Test>]
    member this.``convert throw exception`` () = 
        let csharp = 
             """public void ThrowException()
                {
                    throw new NullReferenceException();                    
                }"""

        let fsharp = 
             """member this.ThrowException() = raise new NullReferenceException()"""

        csharp |> Converter.runWithConfig false 
        |> (fun x -> printfn "%s" x; x)
        |> should equal (formatFsharp fsharp)

    [<Test>]
    member this.``convert generic types in method`` () = 
        let csharp = 
             """public void ConfigureServices(IServiceCollection services)
        		{
        			services.AddHttpClient();
        			services.AddAngleSharp();
        			services.AddSingleton<HzzoHtmlScraper>();
        			services.AddMvc().SetCompatibilityVersion(CompatibilityVersion.Version_2_1);
        		}"""

        let fsharp = 
             """member this.ConfigureServices(services: IServiceCollection) =
                    services.AddHttpClient()
                    services.AddAngleSharp()
                    services.AddSingleton<HzzoHtmlScraper>()
                    services.AddMvc().SetCompatibilityVersion(CompatibilityVersion.Version_2_1)"""

        csharp |> Converter.runWithConfig false 
        |> (fun x -> printfn "%s" x; x)
        |> should equal (formatFsharp fsharp)  

    [<Test>]
    member this.``match is converts to pattern match`` () = 
        let csharp = 
             """if (value is DateTime)
                    writer.WriteValue(((DateTime)value)); 
                else
                    base.WriteJson(writer, value, serializer);
                """

        let fsharp = 
             """if value :? DateTime then writer.WriteValue((value :?> DateTime))
                else base.WriteJson(writer, value, serializer)"""

        test <@ csharp |> Converter.run = formatFsharp fsharp @>

    [<Test>]
    member this.``method with override is converted to override method`` () = 
        let csharp = 
             """public override void Convert()
                {
                    FooBar();
                }
                """

        let fsharp = 
             """override this.Convert() = FooBar()"""

        test <@ csharp |> Converter.run = formatFsharp fsharp @>

    [<Test>]
    member this.``parse class object initlization`` () = 
        let csharp = 
             """public static void SetupJsonSettings()
                {
                    var jsonSerializerSettings = new JsonSerializerSettings
                    {
                        ContractResolver = new CamelCasePropertyNamesContractResolver(),
                        DateFormatHandling = DateFormatHandling.IsoDateFormat,
                        DateTimeZoneHandling = DateTimeZoneHandling.Utc
                    };

                    jsonSerializerSettings.Converters.Add(new JsonDateConverter());

                    JsonConvert.DefaultSettings = () => jsonSerializerSettings;
                }"""
    
        let fsharp = 
             """static member SetupJsonSettings() =
                    let mutable jsonSerializerSettings =
                        new JsonSerializerSettings(ContractResolver = new CamelCasePropertyNamesContractResolver(),
                                                   DateFormatHandling = DateFormatHandling.IsoDateFormat,
                                                   DateTimeZoneHandling = DateTimeZoneHandling.Utc)
                    jsonSerializerSettings.Converters.Add(new JsonDateConverter())
                    JsonConvert.DefaultSettings <- fun () -> jsonSerializerSettings"""

        csharp |> Converter.runWithConfig false 
        |> (fun x -> printfn "%s" x; x)
        |> should equal (formatFsharp fsharp)  

    [<Test>]
    member this.``list initlization`` () = 
        let csharp = 
             """List<int> ListGeneric = new List<int> { 5, 9, 1, 4 };"""
    
        let fsharp = 
             """let mutable ListGeneric = new List<int>([| 5; 9; 1; 4 |])
                ()"""
        csharp |> Converter.runWithConfig false 
        |> (fun x -> printfn "%s" x; x)
        |> should equal (formatFsharp fsharp)      