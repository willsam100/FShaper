namespace Tests

open NUnit.Framework
open FSharper.Core
open FsUnit
            
[<TestClass>]
type TestClass () =

    let formatFsharp (s:string) = s.Replace("                ", "").Replace("\n    \n", "\n\n").Replace("\n            \n", "\n\n")


    [<Test>]
    member this.``using statements convert to open statements`` () = 
        """using MvvmCross.Forms.Views;
           using TipCalc.Core.ViewModels; """ 
        |> Converter.run |> should equal (
            "open MvvmCross.Forms.Views\nopen TipCalc.Core.ViewModels")
        

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

                    member val Bar: Baz = null with get, set
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
    member this.``Can Android Application`` () = 
        let csharp = 
            """[Activity(Label = "Activity A", MainLauncher = true)]
                public class MainApplication : MvxAndroidApplication
                {
                    public MainApplication(IntPtr javaReference, JniHandleOwnership transfer) : base(javaReference, transfer)
                    {
                    }
                }"""

        let fsharp = 
            """[<Activity(Label = "Activity A", MainLauncher = True)>]
                type MainApplication(javaReference: IntPtr, transfer: JniHandleOwnership) =
                    inherit MvxAndroidApplication(javaReference, transfer)"""
                   
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

                open Android.Util
                open Firebase.Iid
                open System
                open Android.App
            
                [<Service(); IntentFilter([| "com.google.firebase.INSTANCE_ID_EVENT" |])>]
                type MyFirebaseIIDService() =
                    inherit FirebaseInstanceIdService()
                    let TAG = "MyFirebaseIIDService"

                    member this.OnTokenRefresh() =
                        let mutable refreshedToken = FirebaseInstanceId.Instance.Token
                        Log.Debug(TAG, "Refreshed token: " + refreshedToken)
                        this.SendRegistrationToServer(refreshedToken)
                
                    member this.SendRegistrationToServer(token: string) = ()"""
                   
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
                    let mutable notificationBuilder =
                        new Notification.Builder(this).SetContentTitle("FCM Message").SetSmallIcon(Resource.Drawable.ic_launcher)
                            .SetContentText(messageBody).SetAutoCancel(True).SetContentIntent(pendingIntent)
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
             """member this.Foo() =
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
             """member this.Foo() =
                    if x then 0
                    else
                        Bar()
                        1"""
                       
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
             """member this.Foo() =
                    if x then 0
                    else
                        Baz()
                        if y then 42
                        else Bar()
                        1"""
                       
        csharp |> Converter.run 
        |> (fun x -> printfn "%s" x; x)
        |> should equal (formatFsharp fsharp)

    // TODO: this test sis currently failing. 
    //[<Test>]
    //member this.``convert continue and foreach loop`` () = 
        //let csharp = 
        //     """ContextMenu CreateContextMenu (CodeFixMenu entrySet)
        //        {
        //            var menu = new ContextMenu ();
        //            foreach (var item in entrySet.Items) {
        //                if (item == CodeFixMenuEntry.Separator) {
        //                    menu.Items.Add (new SeparatorContextMenuItem ());
        //                    continue;
        //                }

        //                var menuItem = new ContextMenuItem (item.Label);
        //                menuItem.Context = item.Action;
        //                if (item.Action == null) {
        //                    if (!(item is CodeFixMenu itemAsMenu) || itemAsMenu.Items.Count <= 0) {
        //                        menuItem.Sensitive = false;
        //                    }
        //                }
        //                var subMenu = item as CodeFixMenu;
        //                if (subMenu != null) {
        //                    menuItem.SubMenu = CreateContextMenu (subMenu);
        //                    menuItem.Selected += delegate {
        //                        RefactoringPreviewTooltipWindow.HidePreviewTooltip ();
        //                    };
        //                    menuItem.Deselected += delegate { RefactoringPreviewTooltipWindow.HidePreviewTooltip (); };
        //                } else {
        //                    menuItem.Clicked += (sender, e) => ((System.Action)((ContextMenuItem)sender).Context) ();
        //                    menuItem.Selected += (sender, e) => {
        //                        RefactoringPreviewTooltipWindow.HidePreviewTooltip ();
        //                        if (item.ShowPreviewTooltip != null) {
        //                            item.ShowPreviewTooltip (e);
        //                        }
        //                    };
        //                    menuItem.Deselected += delegate { RefactoringPreviewTooltipWindow.HidePreviewTooltip (); };
        //                }
        //                menu.Items.Add (menuItem);
        //            }
        //            menu.Closed += delegate { RefactoringPreviewTooltipWindow.HidePreviewTooltip (); };
        //            return menu;
        //        }"""
    
        //let fsharp = 
        //     """member this.SendNotification(messageBody: string) =
        //            let mutable notificationBuilder =
        //                new Notification.Builder(this).SetContentTitle("FCM Message").SetSmallIcon(Resource.Drawable.ic_launcher)
        //                    .SetContentText(messageBody).SetAutoCancel(True).SetContentIntent(pendingIntent)
        //            let mutable notificationManager = NotificationManager.FromContext(this)
        //            notificationManager.Notify(0, notificationBuilder.Build())"""
                   
        //csharp |> Converter.run 
        //|> (fun x -> printfn "%s" x; x)
        //|> should equal (formatFsharp fsharp)