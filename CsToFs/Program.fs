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


    let mvvmCross = """
        using System;
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

                public void Include(Switch @switch)
                {
                    @switch.CheckedChange += (sender, args) => @switch.Checked = !@switch.Checked;
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
                    changed.CollectionChanged += (s, e) => { var test = $"{e.Action}{e.NewItems}{e.NewStartingIndex}{e.OldItems}{e.OldStartingIndex}"; };
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

    let mvvmCross = 
        """
        [CommandHandler (RefactoryCommands.QuickFix)]
        void OnQuickFixCommand ()
        {
            if (!AnalysisOptions.EnableFancyFeatures || smartTagMarginMarker == null) {
                //Fixes = RefactoringService.GetValidActions (Editor, DocumentContext, Editor.CaretLocation).Result;

                PopupQuickFixMenu (null, null);
                return;
            }

            CancelSmartTagPopupTimeout ();
            PopupQuickFixMenu (null, menu => { });
        }
        """

    let mvvmCross = 
        """
            public void RemoveWidget ()
            {
                if (smartTagMarginMarker != null) {
                    Editor.RemoveMarker (smartTagMarginMarker);
                    smartTagMarginMarker.ShowPopup -= SmartTagMarginMarker_ShowPopup;
                    smartTagMarginMarker = null;
                }
                CancelSmartTagPopupTimeout ();
            }
        """

    //let visitor = new FileContentsDumper()
    let indent = " " |> List.replicate 4 |> String.concat ""
    
    let tree = 
        //System.Console.In.ReadToEnd()
        mvvmCross
        |> SyntaxFactory.ParseSyntaxTree


    let checker = FSharpChecker.Create()

    let getUntypedTree (file, input) = 
        // Get compiler options for the 'project' implied by a single script file
        let projectOptions, _errors =  
            checker.GetProjectOptionsFromScript(file, input)
            |> Async.RunSynchronously

        let parsingOptions, _errors = checker.GetParsingOptionsFromProjectOptions(projectOptions)

        // Run the first phase (untyped parsing) of the compiler
        let parseFileResults = 
            checker.ParseFile(file, input, parsingOptions) 
            |> Async.RunSynchronously

        match parseFileResults.ParseTree with
        | Some tree -> tree
        | None -> failwith "Something went wrong during parsing!"


    let toIdent (s:string) = 
        s.Split('.') 
        |> Array.toList
        |> List.map (fun x -> Ident(x, range0))

    let createOpenStatements (name: UsingStatement) = 
        (LongIdentWithDots (name.Namespace |> toIdent, [range0]), range0) |> SynModuleDecl.Open 
        
    let toNamespace (ns:Namespace) mods = 
        SynModuleOrNamespace (toIdent ns.Name,false,false, mods, PreXmlDocEmpty, [], None, range0)

    let defaultModule mods = 
        [SynModuleOrNamespace (toIdent "Program",false,true, mods, PreXmlDocEmpty, [], None, range0)]

    let file = "unknown.fs"
    let toFile moduleOrNs = 

        ParsedImplFileInput (file, true, QualifiedNameOfFile (Ident()), [], [], moduleOrNs, (true, true)) 
        |> ParsedInput.ImplFile

    let toMethod (x:Method) = 
        let methodName = LongIdentWithDots (toIdent ("this." + x.Name), [range0])

        let lines = SynExpr.Const (SynConst.Unit, range0) // TODO 

            //x.Body |> List.singleton
            ////|> List.map (fun (Line l) -> toIdent l)
            //|> function 
            //| [] -> 
            //| x::[] -> 
            //    SynExpr.LongIdent (false, LongIdentWithDots (x, [range0]), None, range0)
            //| xs -> 
                //xs 
                //|> List.map (fun x -> SynExpr.LongIdent (false, LongIdentWithDots (x, [range0]), None, range0))
                //|> List.reduce (fun a b ->  SynExpr.Sequential (SequencePointsAtSeq, true, a,b,range0)) 
                //|> (fun x -> 
                    //let a = 
                    //    SynExpr.App (ExprAtomicFlag.NonAtomic,false, SynExpr.Ident (Ident("CancelSmartTagPopupTimeout", range0)), SynExpr.Const(SynConst.Unit, range0), range0)
                    //SynExpr.Sequential (SequencePointsAtSeq, true, x,a,range0)  )

        SynMemberDefn.Member 
            (SynBinding.Binding (
                x.Accessibility, 
                SynBindingKind.NormalBinding,
                false, //mustInline:bool *
                false, //isMutable:bool *
                [], //attrs:SynAttributes *
                PreXmlDoc.PreXmlDocEmpty, //xmlDoc:PreXmlDoc *
                SynValData (
                    Some {
                        MemberFlags.IsInstance = true
                        MemberFlags.IsDispatchSlot = false 
                        MemberFlags.IsOverrideOrExplicitImpl = false 
                        MemberFlags.IsFinal = false
                        MemberFlags.MemberKind = MemberKind.Member
                    }, SynValInfo ([], SynArgInfo ([], false, Ident (x.Name, range0) |> Some )), None), // valData:SynValData *
                SynPat.LongIdent
                    (methodName, None, None, 
                    Pats ([SynPat.Const (SynConst.Unit, range0 )]), None, range0 ), // headPat:SynPat *
                None, // (SynType.LongIdent (toIdent "return", range0,[]) ), // returnInfo:SynBindingReturnInfo option *
                lines,
                range0, //range:range *
                NoSequencePointAtInvisibleBinding
        ), range0)

    let toDefaultClass method = 
        let x = 
            ComponentInfo ([], [], [], toIdent "X", PreXmlDocEmpty, false, None, range0)

        let methods = [method]
        let ctor = SynMemberDefn.ImplicitCtor (None,[],[],None, range0)

        SynTypeDefn.TypeDefn (x, SynTypeDefnRepr.ObjectModel (TyconUnspecified, [ctor] @ methods, range0), [], range0)
        |> List.singleton
        |> (fun x -> SynModuleDecl.Types (x, range0))

    let toClass (cn:Class) = 
        let att = 
            cn.Attributes |> List.map (fun x -> 
                let arg = SynExpr.Ident (Ident("",range0))

                {
                    SynAttribute.TypeName = LongIdentWithDots (toIdent x.Name, [range0])
                    SynAttribute.ArgExpr = SynExpr.Paren (arg, range0, None, range0)
                    // SynExpr.Paren(SynExpr.Const SynConst.Char ',', range0, range0) 
                    SynAttribute.AppliesToGetterAndSetter = false
                    SynAttribute.Range = range0
                    SynAttribute.Target = None
                }
            )
        let x = 
            ComponentInfo (att, [], [], toIdent cn.Name.Name, PreXmlDocEmpty, false, None, range0)
            
        let methods = cn.Methods |> List.map toMethod
        let ctor = SynMemberDefn.ImplicitCtor (None,[],[],None, range0)
        SynTypeDefn.TypeDefn (x, SynTypeDefnRepr.ObjectModel (TyconUnspecified, [ctor] @ methods, range0), [], range0)
        |> List.singleton
        |> (fun x -> SynModuleDecl.Types (x, range0))
        
    //let t = tree.GetRoot()
    //t.ChildNodes()
    //|> Seq.fold (fun file node -> visitor.ParseSyntax file node) None
    //|> Option.map (function 
    //    | CsToFs.File f -> 
    //        let mods = f.UsingStatements |> List.map createOpenStatements 

    //        let namespaces = 
    //            f.Namespaces |> List.map (fun x -> 
    //                let classes = 
    //                    x.Classes |> List.map toClass
    //                toNamespace x (mods @ classes) )
    //        toFile namespaces

    //    | CsToFs.UsingStatement us -> createOpenStatements us  |> List.singleton |> defaultModule |> toFile
    //    | CsToFs.Namespace ns -> toNamespace ns [] |> List.singleton |> toFile

    //    | CsToFs.Class cn -> 
    //        cn 
    //        |> toClass
    //        |> List.singleton
    //        |> defaultModule
    //        |> toFile

    //    | CsToFs.Method m -> 
    //        m
    //        |> toMethod
    //        |> toDefaultClass
    //        |> List.singleton
    //        |> defaultModule
    //        |> toFile
    //    )
    //|> Option.iter (fun tree -> 
    //    printfn "------------------------------------------------------------"
    //    printfn "%A" tree

    //    printfn "------------------------------------------------------------"

    //    if CodeFormatter.IsValidAST tree then
    //        CodeFormatter.FormatAST(tree, file, None, FormatConfig.Default) |> printfn "%s"
    //    else 
    //        printfn "Bad F# syntax tree"
    //)



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
        type Foo() = 

            let x = if x = 10 || x >= 20 then 30 else 546

            member this.RemoveWidget() = 
                if smartTagMargin then 
                    Editor.RemoveMarker (smartTagMarginMarker);
                    //        smartTagMarginMarker.ShowPopup -= SmartTagMarginMarker_ShowPopup;
                    smartTagMarginMarker <- null;
                CancelSmartTagPopupTimeout ();
                    """
    // File name in Unix format
    let file = "/home/user/Test.fsx"

    // Get the AST of sample F# code
    let tree = getUntypedTree(file, input)

    printfn "%A" tree

    //let tree = 
        //ParsedImplFileInput (file, false, QualifiedNameOfFile (Ident()), [], [], [
            //    SynModuleOrNamespace ([Ident ("FooBar", range0)],false,true, [], PreXmlDocEmpty, [], None, range0)
            //], (true, true)) |> ParsedInput.ImplFile

    if CodeFormatter.IsValidAST tree then
        CodeFormatter.FormatAST(tree, file, None, FormatConfig.Default) |> printfn "%s"
    else 
        printfn "Bad F# syntax tree"

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


