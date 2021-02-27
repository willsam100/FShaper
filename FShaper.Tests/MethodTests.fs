namespace Tests
open CodeFormatter
open NUnit.Framework
open FSharper.Core
open FsUnit
open Swensen.Unquote.Assertions

[<TestFixture>]
type MethodTests () =        

    [<Test>]
    member this.``Can convert property and auto property`` () = 
        let csharp = 
             """
                using MvvmCross.Forms.Views;
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
             """
                namespace TipCalc.Forms.UI.Pages

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

        csharp
        |> reduceIndent
        |> Converter.run 
        |> logConverted
        |> should equal (formatFsharp fsharp)



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
    member this.``enum flags are converted correctly`` () = 
        let csharp = 
             """[Activity (ConfigurationChanges = ConfigChanges.ScreenSize | ConfigChanges.Orientation)]
                public class MainActivity
                {
                }"""
    
        let fsharp = 
             """
                [<Activity(ConfigurationChanges = (ConfigChanges.ScreenSize
                                                   ||| ConfigChanges.Orientation))>]
                type MainActivity() ="""

        csharp |> Converter.runWithConfig false 
        |> logConverted
        |> should equal (simpleFormat fsharp) // This code cannot be formatted by the compiler. It's not legal F#        

    [<Test>]
    member this.``can convert if statements`` () = 
        let csharp = 
            """[CommandHandler (RefectoryCommands.QuickFix)]
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
            """
                [<CommandHandler(RefectoryCommands.QuickFix)>]
                member this.OnQuickFixCommand() =
                    if not AnalysisOptions.EnableFancyFeatures
                       || smartTagMarginMarker = null then
                        PopupQuickFixMenu(null, null)
                        ()
                    else
                        CancelSmartTagPopupTimeout()
                        PopupQuickFixMenu(null, (fun menu -> ()))"""
                   
        csharp
        |> reduceIndent
        |> Converter.run 
        |> logConverted
        |> should equal (formatFsharpWithClass fsharp)
        
    [<Test>]
    member this.``create class instance`` () = 
        let csharp = 
             """
                void SendNotification(string messageBody)
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
             """
                member this.SendNotification(messageBody: string) =
                    let mutable intent = new Intent(this, typeof<MainActivity>)
                    intent.AddFlags(ActivityFlags.ClearTop)
                    let mutable pendingIntent = PendingIntent.GetActivity(this, 0, intent, PendingIntentFlags.OneShot)
                    let mutable notificationBuilder =
                        (new Notification.Builder(this))
                            .SetContentTitle("FCM Message")
                            .SetSmallIcon(Resource.Drawable.ic_launcher)
                            .SetContentText(messageBody)
                            .SetAutoCancel(true)
                            .SetContentIntent(pendingIntent)

                    let mutable notificationManager = NotificationManager.FromContext(this)
                    notificationManager.Notify(0, notificationBuilder.Build())"""
                   
        csharp
        |> reduceIndent
        |> Converter.run 
        |> logConverted
        |> should equal (formatFsharpWithClass fsharp)

    [<Test>]
    member this.``convert if else statement with an early return in else`` () = 
        let csharp = 
             """
                public int Foo()
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
             """
                member this.Foo(): int =
                    if x then
                        SomeAction()
                        0
                    else
                        f"""
                       
        csharp
        |> reduceIndent
        |> Converter.run 
        |> logConverted
        |> should equal (formatFsharpWithClass fsharp)

    [<Test>]
    member this.``convert if else statement with early return if first if`` () = 
        let csharp = 
             """
                public int Foo()
                {
                    if (x)
                        return 0;

                    Bar();
                    return 1;
                }"""

        let fsharp = 
             """
                member this.Foo(): int =
                    if x then
                        0
                    else
                        Bar()
                        1"""
                       
        csharp
        |> reduceIndent
        |> Converter.run 
        |> logConverted
        |> should equal (formatFsharpWithClass fsharp)

    [<Test>]
    member this.``convert method attributes`` () = 
        let csharp = 
             """[CommandHandler (RefectoryCommands.QuickFix)]
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
             """[<CommandHandler(RefectoryCommands.QuickFix)>]
                member this.OnQuickFixCommand() =
                    if not AnalysisOptions.EnableFancyFeatures
                       || smartTagMarginMarker = null then
                        PopupQuickFixMenu(null, null)
                        ()
                    else
                        CancelSmartTagPopupTimeout()
                        PopupQuickFixMenu(null, (fun menu -> ()))"""
                       
        csharp
        |> reduceIndent
        |> Converter.run 
        |> (fun x -> printfn "\n------------------------CONVERTED------------------------\n%s\n" x; x)
        |> should equal (formatFsharpWithClass fsharp)

    [<Test>]
    member this.``convert multiple if statements with early returns`` () = 
        let csharp = 
             """
                public int Foo(int x, int y)
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
             """
                member this.Foo(x:int, y:int): int =
                    if x then
                        0
                    else
                        Baz()
                        if y then 42
                        else 
                            Bar()
                            1"""
                       
        csharp
        |> reduceIndent
        |> Converter.run 
        |> logConverted
        |> should equal (formatFsharpWithClass fsharp)

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
             """
                member this.IsPlayServicesAvailable(): bool =
                    let mutable resultCode =
                        GoogleApiAvailability.Instance
                            .IsGooglePlayServicesAvailable(this)
                            
                    if resultCode <> ConnectionResult.Success then
                        if GoogleApiAvailability.Instance
                                .IsUserResolvableError(resultCode) then
                            msgText.Text <-
                                GoogleApiAvailability.Instance
                                    .GetErrorString(resultCode)
                        else
                            msgText.Text <- "This device is not supported"
                            Finish()
                        false
                    else
                        msgText.Text <- "Google Play Services is available."
                        true"""
                       
        csharp
        |> reduceIndent
        |> Converter.run 
        |> logConverted
        |> should equal (formatFsharpWithClass fsharp)

    [<Test>]
    member this.``convert foreach with break`` () = 
        let csharp = 
             """
                public int Foo(IEnumerable<int> myList)
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
             """
                member this.Foo(myList: seq<int>): int =
                    let mutable x = 0
                    for i in myList do
                        if x >= 10 then () else x <- x + i

                    x"""

        csharp
        |> reduceIndent
        |> Converter.run 
        |> logConverted
        |> should equal (formatFsharpWithClass fsharp)

    [<Test>]
    member this.``convert foreach with continue`` () = 
        let csharp = 
             """
                public int Foo(IEnumerable<int> myList)
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
             """
                member this.Foo(myList: seq<int>): int =
                    let mutable x = 0
                    for i in myList do
                        if x = 10 then () else x <- x + i

                    x"""

        csharp
        |> reduceIndent
        |> Converter.run 
        |> logConverted
        |> should equal (formatFsharpWithClass fsharp)

    [<Test>]
    member this.``convert continue and foreach loop`` () = 
        let csharp = 
             """
                ContextMenu CreateContextMenu (CodeFixMenu entrySet)
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
             """
                member this.CreateContextMenu(entrySet: CodeFixMenu): ContextMenu =
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
                                    _menuItem.Selected
                                        .AddHandler<_>(fun () ->
                                            RefactoringPreviewTooltipWindow
                                                .HidePreviewTooltip())

                                    _menuItem.Deselected
                                        .AddHandler<_>(fun () ->
                                            RefactoringPreviewTooltipWindow
                                                .HidePreviewTooltip())
                                else
                                    _menuItem.Clicked
                                        .AddHandler<_>(fun (sender, e) ->
                                            ((sender :?> ContextMenuItem).Context :?> System.Action)
                                                .Invoke())

                                    _menuItem.Selected
                                        .AddHandler<_>(fun (sender, e) ->
                                            RefactoringPreviewTooltipWindow
                                                .HidePreviewTooltip()

                                            if item.ShowPreviewTooltip <> null then item.ShowPreviewTooltip(e))

                                    _menuItem.Deselected
                                        .AddHandler<_>(fun () ->
                                            RefactoringPreviewTooltipWindow
                                                .HidePreviewTooltip())

                                menu.Items.Add(_menuItem)

                    menu.Closed
                        .AddHandler<_>(fun () -> RefactoringPreviewTooltipWindow.HidePreviewTooltip())
                    menu"""
                   
        csharp
        |> reduceIndent
        |> Converter.run 
        |> logConverted
        |> should equal (formatFsharpWithClass fsharp)

    [<Test>]
    member this.``convert try catch`` () = 
        let csharp = 
             """
                public class Sample
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
             """
                type Sample() =
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

        csharp
        |> reduceIndent
        |> Converter.runWithConfig false
        |> logConverted
        |> should equal (formatFsharp fsharp)

    [<Test>]
    member this.``convert try catch with multiple exception matches`` () = 
        let csharp = 
             """
                public void Main() {
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
             """
                member this.Main() =
                    try
                        let mutable str3 = myString.Substring(3, 1)
                        Console.WriteLine(str3)
                    with
                    | :? ArgumentOutOfRangeException as e -> Console.WriteLine(e.Message)
                    | :? Exception as e -> Console.WriteLine(e.Message)"""

        csharp
        |> reduceIndent
        |> Converter.runWithConfig false
        |> logConverted
        |> should equal (formatFsharpWithClass fsharp)

    [<Test>]
    member this.``convert try catch without named exception`` () = 
        let csharp = 
             """
                public void Main() {
                    try {
                       string str3 = myString.Substring(3, 1); // This throws ArgumentOutOfRangeException.
                       Console.WriteLine(str3);
                    }
                    catch (ArgumentOutOfRangeException) {
                       Console.WriteLine("Oops");
                    }         
                }"""
    
        let fsharp = 
             """
                member this.Main() =
                    try
                        let mutable str3 = myString.Substring(3, 1)
                        Console.WriteLine(str3)
                    with :? ArgumentOutOfRangeException -> Console.WriteLine("Oops")"""

        csharp
        |> reduceIndent
        |> Converter.runWithConfig false
        |> logConverted
        |> should equal (formatFsharpWithClass fsharp)

    [<Test>]
    member this.``convert using block`` () = 
        let csharp = 
             """
                public static byte[] ReadFully(Stream input)
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
             """
                static member ReadFully(input: Stream): byte [] =
                    let mutable buffer = Array.zeroCreate<byte> (16 * 1024)
                    use ms = new MemoryStream()
                    let mutable read = Unchecked.defaultof<int>
                    read <- input.Read(buffer, 0, buffer.Length)
                    while read > 0 do
                        ms.Write(buffer, 0, read)
                        read <- input.Read(buffer, 0, buffer.Length)

                    ms.ToArray()"""

        csharp
        |> reduceIndent
        |> Converter.runWithConfig false
        |> logConverted
        |> should equal (formatFsharpWithClass fsharp)


    [<Test>]
    member this.``parse params keyword`` () = 
        let csharp = 
             """
                public class MembersJoined
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
             """
                type MembersJoined(day: int, location: string, [<ParamArray>] members: string []) =

                    do
                        Day <- day
                        Location <- location
                        Members <- members

                    member val QuestId: Guid = Unchecked.defaultof<Guid> with get, set
                    member val Day: int = Unchecked.defaultof<int> with get, set
                    member val Location: string = Unchecked.defaultof<string> with get, set
                    member val Members: string [] = Unchecked.defaultof<string []> with get, set
                    override this.ToString(): string = sprintf "Members %O joined at %O on Day %O" (Members.Join(", ")) (Location) (Day)"""

        csharp
        |> reduceIndent
        |> Converter.runWithConfig false
        |> logConverted
        |> should equal (formatFsharp fsharp)

    [<Test>]
    member this.``convert generic types in method`` () = 
        let csharp = 
             """
                public void ConfigureServices(IServiceCollection services)
        		{
        			services.AddHttpClient();
        			services.AddAngleSharp();
        			services.AddSingleton<HeelsHtmlScraper>();
        			services.AddMvc().SetCompatibilityVersion(CompatibilityVersion.Version_2_1);
        		}"""

        let fsharp = 
             """
                member this.ConfigureServices(services: IServiceCollection) =
                    services.AddHttpClient()
                    services.AddAngleSharp()
                    services.AddSingleton<HeelsHtmlScraper>()
                    services
                        .AddMvc()
                        .SetCompatibilityVersion(CompatibilityVersion.Version_2_1)"""

        csharp
        |> reduceIndent
        |> Converter.runWithConfig false
        |> logConverted
        |> should equal (formatFsharpWithClass fsharp)  

    [<Test>]
    member this.``parse class object initialization`` () = 
        let csharp = 
             """
                public static void SetupJsonSettings()
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
             """
                static member SetupJsonSettings() =
                    let mutable jsonSerializerSettings =
                        new JsonSerializerSettings(ContractResolver = new CamelCasePropertyNamesContractResolver(),
                                                   DateFormatHandling = DateFormatHandling.IsoDateFormat,
                                                   DateTimeZoneHandling = DateTimeZoneHandling.Utc)
                    jsonSerializerSettings.Converters.Add(new JsonDateConverter())
                    JsonConvert.DefaultSettings <- fun () -> jsonSerializerSettings"""

        csharp
        |> reduceIndent
        |> Converter.runWithConfig false
        |> logConverted
        |> should equal (formatFsharpWithClass fsharp) 



    