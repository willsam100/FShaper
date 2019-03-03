// Learn more about F# at http://fsharp.org
open System
open Microsoft.CodeAnalysis.CSharp
open CsToFs
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.SourceCodeServices.Structure
open Microsoft.FSharp.Compiler.SourceCodeServices 
open Fantomas
open Fantomas.FormatConfig
open TreeOps
    
[<EntryPoint>]
let main argv =

    let mvvmCross = """
        using MvvmCross.Forms.Views;
        using TipCalc.Core.ViewModels;

        namespace TipCalc.Forms.UI.Pages
        {
            public partial class TipView<T, Z> : MvxContentPage<TipViewModel>, IDisposable
            {
                public TipView(string s, int i) : base(message)
                {
                    InitializeComponent();
                }

                public void Foo(Class a, HudidIt fifity) {
                    Console.WriteLine("Hello, world");
                }

                private Foo _foo = 34;
                public Foo Foo { 
                    get { return _foo; } 
                    set {
                        _foo = value;
                        RaisePropertyChanged("Foo");
                    } }

                public Baz Bar { get; set; }

                p
            }
        } """

    let mvvmCross = """
        public void Foo(Class a, HudidIt fifity) {
            Console.WriteLine("Hello, world");
        } """

    let mvvmCross = """
        [Activity(Label = "Activity A", MainLauncher = true)]
        public class MainApplication : MvxAndroidApplication
        {
            public MainApplication(IntPtr javaReference, JniHandleOwnership transfer) : base(javaReference, transfer)
            {
            }
        } """


    //let mvvmCross = """
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

    //let mvvmCross = 
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

    let mvvmCross = 
        """
            class CodeActionEditorExtension : TextEditorExtension
            {
                const int menuTimeout = 150;
            }
        """

    let mvvmCross = """
        ContextMenu CreateContextMenu (CodeFixMenu entrySet)
        {
            var menu = new ContextMenu ();
            foreach (var item in entrySet.Items) {
                if (item == CodeFixMenuEntry.Separator) {
                    menu.Items.Add (new SeparatorContextMenuItem ());
                    continue;
                }

                var menuItem = new ContextMenuItem (item.Label);
                menuItem.Context = item.Action;
                if (item.Action == null) {
                    if (!(item is CodeFixMenu itemAsMenu) || itemAsMenu.Items.Count <= 0) {
                        menuItem.Sensitive = false;
                    }
                }
                var subMenu = item as CodeFixMenu;
                if (subMenu != null) {
                    menuItem.SubMenu = CreateContextMenu (subMenu);
                    menuItem.Selected += delegate {
                        RefactoringPreviewTooltipWindow.HidePreviewTooltip ();
                    };
                    menuItem.Deselected += delegate { RefactoringPreviewTooltipWindow.HidePreviewTooltip (); };
                } else {
                    menuItem.Clicked += (sender, e) => ((System.Action)((ContextMenuItem)sender).Context) ();
                    menuItem.Selected += (sender, e) => {
                        RefactoringPreviewTooltipWindow.HidePreviewTooltip ();
                        if (item.ShowPreviewTooltip != null) {
                            item.ShowPreviewTooltip (e);
                        }
                    };
                    menuItem.Deselected += delegate { RefactoringPreviewTooltipWindow.HidePreviewTooltip (); };
                }
                menu.Items.Add (menuItem);
            }
            menu.Closed += delegate { RefactoringPreviewTooltipWindow.HidePreviewTooltip (); };
            return menu;
        }
    """

    //let mvvmCross = 
        //"""
            //ContextMenu CreateContextMenu (CodeFixMenu entrySet)
            //{
                //menuItem.Clicked += (sender, e) => sender.Foo();

            //}"""

    let mvvmCross = 
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

    let mvvmCross = 
        """
            using System;
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

    let mvvmCross = 
        """
        using System;
        using Android.App;
        using Firebase.Iid;
        using Android.Util;"""
                    
            

    let visitor = new FileContentsDumper()
    let indent = " " |> List.replicate 4 |> String.concat ""

    let tree = 
        //System.Console.In.ReadToEnd()
        mvvmCross
        |> SyntaxFactory.ParseSyntaxTree

    let file = "unknown.fs"
    let createOpenStatements (name: UsingStatement) = 
        (LongIdentWithDots (name.Namespace |> toIdent, [range0]), range0) |> SynModuleDecl.Open 

    let toNamespace (ns:Namespace) mods = 
        SynModuleOrNamespace (toIdent ns.Name,false,false, mods, PreXmlDocEmpty, [], None, range0)

    let defaultModule mods = 
        [SynModuleOrNamespace (toIdent "Program",false,true, mods, PreXmlDocEmpty, [], None, range0)]

    let toFile moduleOrNs = 
        ParsedImplFileInput (file, true, QualifiedNameOfFile (Ident()), [], [], moduleOrNs, (true, true)) 
        |> ParsedInput.ImplFile

    let toMethod (x:Method) = 
        let methodName = LongIdentWithDots (toIdent ("this." + x.Name), [range0])

        let argInfos = 
            let args = 
                x.Parameters |> List.map (fun x -> SynArgInfo ([],false, Ident(x.Name, range0) |> Some  ) )
            let returnArgInfo = SynArgInfo ([],false, Ident(x.ReturnType, range0) |> Some  ) 
            [returnArgInfo] :: args :: []

        let namedArgs = 
            let typeArgs = 
                x.Parameters |> List.map (fun x -> 
                    SynPat.Typed (
                        SynPat.Named (SynPat.Wild range0, Ident(x.Name, range0), false, None, range0),
                        SynType.LongIdent ( x.Type |> fixKeywords |> toLongIdentWithDots),  
                        range0) )
            SynPat.Paren (SynPat.Tuple (typeArgs, range0), range0)

        let attributres = 
            x.Attributes |> List.map (fun (name, args) -> 
                {
                    SynAttribute.Target = None
                    SynAttribute.AppliesToGetterAndSetter = false
                    SynAttribute.Range = range0
                    SynAttribute.TypeName = name
                    SynAttribute.ArgExpr = 
                        match args with 
                        | None -> SynExpr.Const (SynConst.Unit, range0)
                        | Some x -> toSynExpr x
                }
            )

        let trandformedTree = x.Body |> rewriteInLetExp |> rewriteReturnInIf

        SynMemberDefn.Member 
            (SynBinding.Binding ( x.Accessibility, SynBindingKind.NormalBinding, false, false, attributres,
                PreXmlDoc.PreXmlDocEmpty,
                SynValData (
                    Some {
                        MemberFlags.IsInstance = true
                        MemberFlags.IsDispatchSlot = false 
                        MemberFlags.IsOverrideOrExplicitImpl = false 
                        MemberFlags.IsFinal = false
                        MemberFlags.MemberKind = MemberKind.Member
                    }, SynValInfo (argInfos, SynArgInfo ([], false, None)), None), // valData:SynValData *
                SynPat.LongIdent
                    (methodName, None, None, 
                    Pats [namedArgs], None, range0 ), // headPat:SynPat *
                None, // (SynType.LongIdent (toIdent "return", range0,[]) ), // returnInfo:SynBindingReturnInfo option *
                trandformedTree |> toSynExpr, //|> addNewLineToLet file |> snd,
                range0, //range:range *
                NoSequencePointAtInvisibleBinding
            ), range0)

    let toProperty (x:Prop) = 

        let methodName = LongIdentWithDots (toIdent ("this." + x.Name), [range0])

        //let memberType = 
            //match x.Get, x.Set with 
            //| Some _, Some _ -> MemberKind.PropertyGetSet
            //| Some _, _ -> MemberKind.PropertyGet
            //| _, Some _ -> MemberKind.PropertySet
            //| _,  _ -> 

                //AutoProperty
                  //([],false,smartTagPopupTimeoutId,
                   //Some (LongIdent (LongIdentWithDots ([uint],[]))),
                   //PropertyGetSet,<fun:_fsyacc_reductions@1421-324>,
                   //PreXmlDoc
                   //  ((5,50),Microsoft.FSharp.Compiler.Ast+XmlDocCollector),
                   //None,
                   //Null /home/user/Test.fsx (5,53--5,57) IsSynthetic=false,
                   //Some /home/user/Test.fsx (5,58--5,70) IsSynthetic=false,
                   ///home/user/Test.fsx (5,19--5,57) IsSynthetic=false);
                //toLongIndent x.Type


        let memberFlags = function 
        | MemberKind.ClassConstructor
        | MemberKind.Constructor
        | MemberKind.Member
        | MemberKind.PropertyGet
        | MemberKind.PropertySet
        | MemberKind.PropertyGetSet -> 
            {
                MemberFlags.IsDispatchSlot = false
                MemberFlags.IsFinal = false
                MemberFlags.IsOverrideOrExplicitImpl = false
                MemberFlags.IsInstance = true
                MemberFlags.MemberKind = MemberKind.PropertyGetSet

            }

        SynMemberDefn.AutoProperty 
            ([],false, 
                ident (x.Name, range0), 
                SynType.LongIdent (LongIdentWithDots (toIdent x.Type,[range0]) ) |> Some,
                MemberKind.PropertyGetSet, memberFlags, PreXmlDoc.PreXmlDocEmpty, x.Access, SynExpr.Null range0, None, range0
                  )


        //SynMemberDefn.Member 
        //    (SynBinding.Binding ( Some SynAccess.Public, SynBindingKind.NormalBinding, false, false, [],
        //        PreXmlDoc.PreXmlDocEmpty, //xmlDoc:PreXmlDoc *
        //        SynValData (
        //            Some {
        //                MemberFlags.IsInstance = true
        //                MemberFlags.IsDispatchSlot = false 
        //                MemberFlags.IsOverrideOrExplicitImpl = false 
        //                MemberFlags.IsFinal = false
        //                MemberFlags.MemberKind = memberType
        //            }, SynValInfo ([], SynArgInfo ([], false, Ident (x.Name, range0) |> Some )), None), // valData:SynValData *
        //        SynPat.LongIdent
        //            (methodName, None, None, 
        //            Pats ([SynPat.Const (SynConst.Unit, range0 )]), None, range0 ), // headPat:SynPat *
        //        None, // (SynType.LongIdent (toIdent "return", range0,[]) ), // returnInfo:SynBindingReturnInfo option *
        //        Expr.Null |> toSynExpr,
        //        range0, //range:range *
        //        NoSequencePointAtInvisibleBinding
        //), range0)


    let toDefaultClass method = 
        let x = 
            ComponentInfo ([], [], [], toIdent "X", PreXmlDocEmpty, false, None, range0)

        let methods = [method]
        let ctor = SynMemberDefn.ImplicitCtor (None,[],[],None, range0)

        SynTypeDefn.TypeDefn (x, SynTypeDefnRepr.ObjectModel (TyconUnspecified, [ctor] @ methods, range0), [], range0)
        |> List.singleton
        |> (fun x -> SynModuleDecl.Types (x, range0))

    let toLet (x:Field) = 

        let init = 
            match x.Initilizer with 
            | Some x -> x
            | None -> Expr.Null

        let binding = 
            SynBinding.Binding (None, SynBindingKind.NormalBinding, false, true, [], PreXmlDocEmpty, 
                SynValData (
                    None, SynValInfo ([], SynArgInfo ([], false, Ident (x.Name, range0) |> Some )), None), // valData:SynValData *
                SynPat.LongIdent
                    (LongIdentWithDots (toIdent x.Name, [range0]), None, None, 
                    Pats ([]), None, range0 ), None, 
                    toSynExpr init, range0 , SequencePointAtBinding range0)
        SynMemberDefn.LetBindings ([binding], false, false, range0)
       

    let toClass (cn:Class) = 
        let att = 
            cn.Attributes |> List.map (fun x -> 
                let arg = 
                    match x.Parameters with
                    | None -> SynExpr.Paren (SynExpr.Ident (Ident("",range0)), range0, None, range0) 
                    | Some attr -> toSynExpr attr

                {
                    SynAttribute.TypeName = LongIdentWithDots (toIdent x.Name, [range0])
                    SynAttribute.ArgExpr = arg
                    // SynExpr.Paren(SynExpr.Const SynConst.Char ',', range0, range0) 
                    SynAttribute.AppliesToGetterAndSetter = false
                    SynAttribute.Range = range0
                    SynAttribute.Target = None
                }
            )
        let x = 
            ComponentInfo (att, [], [], toIdent cn.Name.Name, PreXmlDocEmpty, false, None, range0)

        let properties = cn.Properties |> List.map toProperty
        let methods = cn.Methods |> List.map toMethod
        let fields = cn.Fields |> List.map toLet

        let ctors = 
            let ctor = SynMemberDefn.ImplicitCtor (None,[],[],None, range0)
            cn.BaseClass |> Option.map (fun baseClass -> 
                SynMemberDefn.ImplicitInherit (SynType.LongIdent (baseClass |> toLongIdentWithDots), SynExpr.Const (SynConst.Unit, range0), None, range0)
            ) |> function 
            | Some x -> [ctor; x]
            | None -> [ctor]

        SynTypeDefn.TypeDefn (x, SynTypeDefnRepr.ObjectModel (TyconUnspecified, ctors @ fields @ properties @ methods, range0), [], range0)
        |> List.singleton
        |> (fun x -> SynModuleDecl.Types (x, range0))

    let t = tree.GetRoot()
    t.ChildNodes()
    |> Seq.fold (visitor.ParseSyntax) None
    |> Option.map (fun x -> 
        match x with 
        | CsToFs.File f -> 
            let mods = f.UsingStatements |> List.map createOpenStatements 
            let ns = 
                match f.Namespaces with 
                | [] -> [{Name = "Foo"; Interfaces = []; Classes = [] }]
                | xs -> xs 

            let namespaces = 
                ns |> List.map (fun x -> 
                    let classes = 
                        x.Classes |> List.map toClass
                    toNamespace x (mods @ classes) )
            toFile namespaces

        | CsToFs.UsingStatement us -> 
            let ns = {Name = "Foo"; Interfaces = []; Classes = [] }           
            toNamespace ns [createOpenStatements us] |> List.singleton |> toFile
        
                //   |> List.singleton |> defaultModule |> toFile
        | CsToFs.Namespace ns -> toNamespace ns [] |> List.singleton |> toFile
        | CsToFs.Class cn ->  cn  |> toClass |> List.singleton |> defaultModule |> toFile
        | CsToFs.Method m ->  m |> toMethod |> toDefaultClass |> List.singleton |> defaultModule |> toFile
        )
    |> Option.map(removeFhsarpIn file)
    //|> Option.map (fun tree -> 
    //    let config = 
    //        {
    //            FormatConfig.Default with FormatConfig.PageWidth = 1000
    //        }

    //    let x = CodeFormatter.FormatAST(tree, file, None, FormatConfig.Default) 
    //    printfn "%s" x
    //    x)
    //|> Option.map(fun fsharpSource -> 
        //let tree = getUntypedTree(file, fsharpSource)
        //removeFhsarpIn file tree)
    |> Option.iter (fun tree -> 
        // printfn "------------------------------------------------------------"
        // printfn "%A" tree

        // printfn "------------------------------------------------------------"

        let config = 
            {
                FormatConfig.Default with 
                    FormatConfig.SemicolonAtEndOfLine = false
                    FormatConfig.StrictMode = true
                    FormatConfig.PageWidth = 80
                    FormatConfig.SpaceAfterComma = true
                    FormatConfig.SpaceBeforeArgument = true
                    FormatConfig.SpaceBeforeColon = false
            }

        if CodeFormatter.IsValidAST tree then
            CodeFormatter.FormatAST(tree, file, None, config) |> printfn "%s"
        else 
            printfn "Bad F# syntax tree"
    )



    //let t = tree.GetRoot()
    //t.ChildNodes()
    //|> Seq.fold (fun file node -> visitor.ParseSyntax file node) None
    //|> Option.map (ProgramPrinter.prettyPrint indent >> String.concat "\n")
    //|> Option.defaultValue "failed to parse"
    //|> Console.WriteLine

    //visitor.Visit <| tree.GetRoot()

    //printfn "--\nParsed C# Code. F# below:\n--"
    //let lines = ProgramPrinter.prettyPrint visitor.GetFsharpProgram
    //lines |> Seq.iter (printfn "%s")



    //public void RemoveWidget ()
    //{
    //    if (smartTagMarginMarker != null) {
    //        Editor.RemoveMarker (smartTagMarginMarker);
    //        smartTagMarginMarker.ShowPopup -= SmartTagMarginMarker_ShowPopup;
    //        smartTagMarginMarker = null;
    //    }
    //    CancelSmartTagPopupTimeout ();
    //}

    let input = """
        [<IntentFilter([| "com.google.firebase.INSTANCE_ID_EVENT" |])>]
        type MyFirebaseIIDService() = 
            ()

    """
    // File name in Unix format
    let file = "/home/user/Test.fsx"

    let config = 
        {
            FormatConfig.Default with 
                FormatConfig.SemicolonAtEndOfLine = false
                FormatConfig.StrictMode = true
                FormatConfig.PageWidth = 120
                FormatConfig.SpaceAfterComma = true
                FormatConfig.SpaceBeforeArgument = true
                FormatConfig.SpaceBeforeColon = false
                FormatConfig.IndentSpaceNum = 4
                FormatConfig.PreserveEndOfLine = false
        }


    // Get the AST of sample F# code
    // let tree = getUntypedTree(file, input)
    // printfn "%A" tree
    // CodeFormatter.FormatAST(tree, file, None, FormatConfig.Default) |> printfn "%s"

    //let tree = 
        //ParsedImplFileInput (file, false, QualifiedNameOfFile (Ident()), [], [], [
            //    SynModuleOrNamespace ([Ident ("FooBar", range0)],false,true, [], PreXmlDocEmpty, [], None, range0)
            //], (true, true)) |> ParsedInput.ImplFile



    //let visitModulesAndNamespaces modulesOrNss =
    //  for moduleOrNs in modulesOrNss do
    //    let (SynModuleOrNamespace(lid, isRec, isMod, decls, xml, attrs, _, m)) = moduleOrNs
    //    lid |> List.iter (fun x -> printfn "%A" x.idRange)
    //    printfn "Namespace or module: %A" lid

    //match tree with
    //| ParsedInput.ImplFile(implFile) ->
    //    // Extract declarations and walk over them
    //    let (ParsedImplFileInput(fn, script, name, _, _, modules, _)) = implFile
    //    visitModulesAndNamespaces modules
    //| _ -> failwith "F# Interface file (*.fsi) not supported."

    0


