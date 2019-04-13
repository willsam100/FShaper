module Program = let [<EntryPoint>] main _ = 0

// TODO: 
// The following sections of code should all be tested and convert


//let input = """
//    using MvvmCross.Forms.Views;
//    using TipCalc.Core.ViewModels;

//    namespace TipCalc.Forms.UI.Pages
//    {
//        public partial class TipView<T, Z> : MvxContentPage<TipViewModel>, IDisposable
//        {
//            public TipView(string s, int i) : base(message)
//            {
//                InitializeComponent();
//            }

//            public void Foo(Class a, HudidIt fifity) {
//                Console.WriteLine("Hello, world");
//            }

//            private Foo _foo = 34;
//            public Foo Foo { 
//                get { return _foo; } 
//                set {
//                    _foo = value;
//                    RaisePropertyChanged("Foo");
//                } }

//            public Baz Bar { get; set; }

//            p
//        }
//    } """

//let input = """
//    public void Foo(Class a, HudidIt fifity) {
//        Console.WriteLine("Hello, world");
//    } """

//let input = """
    //[Activity(Label = "Activity A", MainLauncher = true)]
    //public class MainApplication : MvxAndroidApplication
    //{
    //    public MainApplication(IntPtr javaReference, JniHandleOwnership transfer) : base(javaReference, transfer)
    //    {
    //    }
    //} """


//let input = """
    //using System;
    //using System.Collections.Specialized;
    //using System.Windows.Input;
    //using Android.App;
    //using Android.Views;
    //using Android.Widget;
    //using MvvmCross.Binding.BindingContext;
    //using MvvmCross.Navigation;
    //using MvvmCross.ViewModels;

    //namespace StarWarsSample.Forms.Droid
    //{
    //    // This class is never actually executed, but when Xamarin linking is enabled it does how to ensure types and properties
    //    // are preserved in the deployed app
    //    [Android.Runtime.Preserve(AllMembers = true)]
    //    public class LinkerPleaseInclude
    //    {
    //        public void Include(Button button)
    //        {
    //            button.Click += (s, e) => button.Text = button.Text + "";
    //        }

    //        public void Include(CheckBox checkBox)
    //        {
    //            checkBox.CheckedChange += (sender, args) => checkBox.Checked = !checkBox.Checked;
    //        }

    //        public void Include(Switch @switch)
    //        {
    //            @switch.CheckedChange += (sender, args) => @switch.Checked = !@switch.Checked;
    //        }

    //        public void Include(View view)
    //        {
    //            view.Click += (s, e) => view.ContentDescription = view.ContentDescription + "";
    //        }

    //        public void Include(TextView text)
    //        {
    //            text.AfterTextChanged += (sender, args) => text.Text = "" + text.Text;
    //            text.Hint = "" + text.Hint;
    //        }

    //        public void Include(CheckedTextView text)
    //        {
    //            text.AfterTextChanged += (sender, args) => text.Text = "" + text.Text;
    //            text.Hint = "" + text.Hint;
    //        }

    //        public void Include(CompoundButton cb)
    //        {
    //            cb.CheckedChange += (sender, args) => cb.Checked = !cb.Checked;
    //        }

    //        public void Include(SeekBar sb)
    //        {
    //            sb.ProgressChanged += (sender, args) => sb.Progress = sb.Progress + 1;
    //        }

    //        public void Include(RadioGroup radioGroup)
    //        {
    //            radioGroup.CheckedChange += (sender, args) => radioGroup.Check(args.CheckedId);
    //        }

    //        public void Include(RadioButton radioButton)
    //        {
    //            radioButton.CheckedChange += (sender, args) => radioButton.Checked = args.IsChecked;
    //        }

    //        public void Include(RatingBar ratingBar)
    //        {
    //            ratingBar.RatingBarChange += (sender, args) => ratingBar.Rating = 0 + ratingBar.Rating;
    //        }

    //        public void Include(Activity act)
    //        {
    //            act.Title = act.Title + "";
    //        }

    //        public void Include(INotifyCollectionChanged changed)
    //        {
    //            changed.CollectionChanged += (s, e) => { var test = $"{e.Action}{e.NewItems}{e.NewStartingIndex}{e.OldItems}{e.OldStartingIndex}"; };
    //        }

    //        public void Include(ICommand command)
    //        {
    //            command.CanExecuteChanged += (s, e) => { if (command.CanExecute(null)) command.Execute(null); };
    //        }

    //        public void Include(MvvmCross.IoC.MvxPropertyInjector injector)
    //        {
    //            injector = new MvvmCross.IoC.MvxPropertyInjector();
    //        }

    //        public void Include(System.ComponentModel.INotifyPropertyChanged changed)
    //        {
    //            changed.PropertyChanged += (sender, e) =>
    //            {
    //                var test = e.PropertyName;
    //            };
    //        }

    //        public void Include(MvxTaskBasedBindingContext context)
    //        {
    //            context.Dispose();
    //            var context2 = new MvxTaskBasedBindingContext();
    //            context2.Dispose();
    //        }

    //        public void Include(MvxNavigationService service, IMvxViewModelLoader loader)
    //        {
    //            service = new MvxNavigationService(null, loader);
    //        }

    //        public void Include(ConsoleColor color)
    //        {
    //            Console.Write("");
    //            Console.WriteLine("");
    //            color = Console.ForegroundColor;
    //            Console.ForegroundColor = ConsoleColor.Red;
    //            Console.ForegroundColor = ConsoleColor.Yellow;
    //            Console.ForegroundColor = ConsoleColor.Magenta;
    //            Console.ForegroundColor = ConsoleColor.White;
    //            Console.ForegroundColor = ConsoleColor.Gray;
    //            Console.ForegroundColor = ConsoleColor.DarkGray;
    //        }

    //        public void Include(MvvmCross.Plugin.Json.Plugin plugin)
    //        {
    //            plugin.Load();
    //        }
    //    }
    //}"""

//let input = 
    //"""
    //[CommandHandler (RefactoryCommands.QuickFix)]
    //void OnQuickFixCommand ()
    //{
    //    if (!AnalysisOptions.EnableFancyFeatures || smartTagMarginMarker == null) {
    //        //Fixes = RefactoringService.GetValidActions (Editor, DocumentContext, Editor.CaretLocation).Result;

    //        PopupQuickFixMenu (null, null);
    //        return;
    //    }

    //    CancelSmartTagPopupTimeout ();
    //    PopupQuickFixMenu (null, menu => { });
    //}
    //"""

//let input = 
//    """
//        class CodeActionEditorExtension : TextEditorExtension
//        {
//            const int menuTimeout = 150;
//        }
//    """

//let input = """
//    ContextMenu CreateContextMenu (CodeFixMenu entrySet)
//    {
//        var menu = new ContextMenu ();
//        foreach (var item in entrySet.Items) {
//            if (item == CodeFixMenuEntry.Separator) {
//                menu.Items.Add (new SeparatorContextMenuItem ());
//                continue;
//            }

//            var menuItem = new ContextMenuItem (item.Label);
//            menuItem.Context = item.Action;
//            if (item.Action == null) {
//                if (!(item is CodeFixMenu itemAsMenu) || itemAsMenu.Items.Count <= 0) {
//                    menuItem.Sensitive = false;
//                }
//            }
//            var subMenu = item as CodeFixMenu;
//            if (subMenu != null) {
//                menuItem.SubMenu = CreateContextMenu (subMenu);
//                menuItem.Selected += delegate {
//                    RefactoringPreviewTooltipWindow.HidePreviewTooltip ();
//                };
//                menuItem.Deselected += delegate { RefactoringPreviewTooltipWindow.HidePreviewTooltip (); };
//            } else {
//                menuItem.Clicked += (sender, e) => ((System.Action)((ContextMenuItem)sender).Context) ();
//                menuItem.Selected += (sender, e) => {
//                    RefactoringPreviewTooltipWindow.HidePreviewTooltip ();
//                    if (item.ShowPreviewTooltip != null) {
//                        item.ShowPreviewTooltip (e);
//                    }
//                };
//                menuItem.Deselected += delegate { RefactoringPreviewTooltipWindow.HidePreviewTooltip (); };
//            }
//            menu.Items.Add (menuItem);
//        }
//        menu.Closed += delegate { RefactoringPreviewTooltipWindow.HidePreviewTooltip (); };
//        return menu;
//    }
//"""

////let input = 
//    //"""
//        //ContextMenu CreateContextMenu (CodeFixMenu entrySet)
//        //{
//            //menuItem.Clicked += (sender, e) => sender.Foo();

//        //}"""

//let input = 
//    """
//        public bool IsPlayServicesAvailable ()
//        {
//            int resultCode = GoogleApiAvailability.Instance.IsGooglePlayServicesAvailable (this);
//            if (resultCode != ConnectionResult.Success)
//            {
//                if (GoogleApiAvailability.Instance.IsUserResolvableError (resultCode))
//                    msgText.Text = GoogleApiAvailability.Instance.GetErrorString (resultCode);
//                else
//                {
//                    msgText.Text = "This device is not supported";
//                    Finish ();
//                }
//                return false;
//            }
//            else
//            {
//                msgText.Text = "Google Play Services is available.";
//                return true;
//            }
//        }"""

//let input = 
//    """
//        using System;
//        using Android.App;
//        using Firebase.Iid;
//        using Android.Util;

//        namespace FCMClient
//        {
//            [Service]
//            [IntentFilter(new[] { "com.google.firebase.INSTANCE_ID_EVENT" })]
//            public class MyFirebaseIIDService : FirebaseInstanceIdService
//            {
//                const string TAG = "MyFirebaseIIDService";
//                public override void OnTokenRefresh()
//                {
//                    var refreshedToken = FirebaseInstanceId.Instance.Token;
//                    Log.Debug(TAG, "Refreshed token: " + refreshedToken);
//                    SendRegistrationToServer(refreshedToken);
//                }
//                void SendRegistrationToServer(string token)
//                {
//                    // Add custom implementation, as needed.
//                }
//            }
//        }"""

//let input = 
//    """
//    using System;
//    using Android.App;
//    using Firebase.Iid;
//    using Android.Util;"""

//let input = """AppCenter.Start("cf32a57f-60ce-4a42-aac1-a84148c29b0d", typeof(Push));"""

//let input = 
    //"""
    //void SendNotification(string messageBody)
    //{
    //    var intent = new Intent(this, typeof(MainActivity));
    //    intent.AddFlags(ActivityFlags.ClearTop);
    //    var pendingIntent = PendingIntent.GetActivity(this, 0, intent, PendingIntentFlags.OneShot);

    //    var notificationBuilder = new Notification.Builder(this)
    //                .SetContentTitle("FCM Message")
    //                .SetSmallIcon(Resource.Drawable.ic_launcher)
    //                .SetContentText(messageBody)
    //                .SetAutoCancel(true)
    //                .SetContentIntent(pendingIntent);

    //    var notificationManager = NotificationManager.FromContext(this);

    //    notificationManager.Notify(0, notificationBuilder.Build());
    //}
    //"""