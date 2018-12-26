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

module Range = 
    let addLine file (range:range) = 

        let pos1 = mkPos 0 0
        let pos2 = mkPos (range.EndLine + 1) range.EndColumn

        mkRange file pos1 pos2

    let moveDownLine file (range:range) = 
        let pos1 = mkPos (range.StartLine + 1) range.StartColumn
        let pos2 = mkPos (range.EndLine + 1) range.EndColumn

        mkRange file pos1 pos2
    
[<EntryPoint>]
let main argv =

    let rec toSynPat p = 
        match p with 
        | Pat.LongIdent (a, b, c, d, e) -> SynPat.LongIdent (a, b, c, d, e, range0)
        | Pat.Wild -> SynPat.Wild range0
        | Pat.Named (a,b,c,d)-> SynPat.Named (toSynPat a,b,c,d, range0)

    let rec toBinding b = 
        let (LetBind (accessibility, kind, mustInline, isMutable, attrs, valData, headPat, expr)) = b
        SynBinding.Binding (accessibility, kind, mustInline, isMutable, attrs, PreXmlDocEmpty, valData, toSynPat headPat, None, toSynExpr expr,range0, SequencePointAtBinding range0)
        
    and 
        toSynExpr (expr:Expr): SynExpr = 
        match expr with  
        | Expr.Paren x -> SynExpr.Paren (toSynExpr x, range0, None, range0)
        | Expr.Const x -> SynExpr.Const (x, range0)
        | Expr.Typed (expr, typeName) -> SynExpr.Typed (toSynExpr expr, typeName, range0)
        | Expr.Tuple xs -> SynExpr.Tuple (xs |> List.map toSynExpr, [range0], range0) 


        //| New of isProtected:bool * typeName:SynType
        //| While of whileSeqPoint:SequencePointInfoForWhileLoop * whileExpr:Expr * doExpr:Expr
        //| For of forSeqPoint:SequencePointInfoForForLoop * ident:Ident * identBody:Expr * bool * toBody:Expr * doBody:Expr

        //| ForEach of forSeqPoint:SequencePointInfoForForLoop * seqExprOnly:SeqExprOnly * isFromSource:bool * pat:SynPat * enumExpr:Expr * bodyExpr:Expr

        //| ArrayOrListOfSeqExpr of isArray:bool * expr:Expr

        //| CompExpr of isArrayOrList:bool * isNotNakedRefCell:bool ref * expr:Expr

        //| Lambda of  fromMethod:bool * inLambdaSeq:bool * args:SynSimplePats * body:Expr

        //| Assert of expr:Expr

        | Expr.App (a,b,c,d) -> SynExpr.App (a,b, toSynExpr c, toSynExpr d, range0)

        | Expr.LetOrUse (a,b,c,d) -> // of isRecursive:bool * isUse:bool * bindings:SynBinding list * body:Expr
            
            SynExpr.LetOrUse (a,b,c |> List.map toBinding, toSynExpr d, range0)

        //| TryWith of tryExpr:Expr * withCases:SynMatchClause list * trySeqPoint:SequencePointInfoForTry * withSeqPoint:SequencePointInfoForWith

        //| TryFinally of tryExpr:Expr * finallyExpr:Expr * trySeqPoint:SequencePointInfoForTry * finallySeqPoint:SequencePointInfoForFinally

        //| Lazy of Expr

        | Expr.Sequential (a,b,c,d) -> SynExpr.Sequential (a,b,toSynExpr c,toSynExpr d, range0)

        | Expr.IfThenElse (a,b,c,d,e) -> SynExpr.IfThenElse (toSynExpr a, toSynExpr b,c |> Option.map toSynExpr ,d,e, range0, range0) 

        | Expr.Ident s -> SynExpr.Ident (Ident(s, range0))
        | Expr.LongIdent (a,b) -> SynExpr.LongIdent (a,b, None, range0)

        | Expr.LongIdentSet (id, e) -> SynExpr.LongIdentSet (id, toSynExpr e, range0) //of longDotId:LongIdentWithDots * expr:Expr
        //| DotGet of expr:Expr
        //| DotSet of Expr * longDotId:LongIdentWithDots * Expr
        | Expr.Set (left, right) -> SynExpr.Set (toSynExpr left, toSynExpr right, range0)
        //| DotIndexedGet of Expr * SynIndexerArg list

        //| DotIndexedSet of objectExpr:Expr * indexExprs:SynIndexerArg list * valueExpr:Expr
        //| NamedIndexedPropertySet of longDotId:LongIdentWithDots * Expr * Expr
        //| TypeTest of  expr:Expr * typeName:SynType
        //| Upcast of  expr:Expr * typeName:SynType 
        //| Downcast of  expr:Expr * typeName:SynType 
        //| InferredUpcast of  expr:Expr 

        //| InferredDowncast of  expr:Expr 
        | Expr.Null -> SynExpr.Null range0
        //| AddressOf of  isByref:bool * Expr 
        //| TraitCall of SynTypar list * SynMemberSig * Expr 
        //| LetOrUseBang    of bindSeqPoint:SequencePointInfoForBinding * isUse:bool * isFromSource:bool * SynPat * Expr * Expr

        //| DoBang      of expr:Expr
        //| Fixed of expr:Expr

        | Expr.ReturnFromIf -> SynExpr.Const (SynConst.Unit, range0)


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
            void HandleCaretPositionChanged (object sender, EventArgs e)
            {
                CancelQuickFixTimer ();
                var token = quickFixCancellationTokenSource.Token;
                CancelQuickFixTimer ();
            }
        }
        """

    let visitor = new FileContentsDumper()
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


    let rewriteReturnInIf tree = 

        let rec isReturnFrom = function 
            | Expr.ReturnFromIf -> true
            | Expr.Sequential (_,_,c,d) -> isReturnFrom c || isReturnFrom d
            | Expr.IfThenElse (a,b,c,d,e) -> isReturnFrom b || c |> Option.map isReturnFrom |> Option.defaultValue false
            | Expr.App (a,b,c,d) -> isReturnFrom c || isReturnFrom d
            | _ -> false

        let rec walker tree = 
            match tree with 
            | Expr.Sequential (s1,s2,s3,s4) -> 
                match s3 with 
                | Expr.IfThenElse (x,y,z,i,j) when Option.isNone z -> 
                    Expr.IfThenElse (x,y, Some s4, i, j)

                | _ -> 
                    Expr.Sequential (s1,s2,walker s3,walker s4)

            | Expr.IfThenElse (a,b,c,d,e) -> 
                let a = walker a
                let b = walker b
                let c = c |> Option.map walker
                Expr.IfThenElse (a,b,c,d,e)

            | Expr.App (a,b,c,d) -> 

                let c = walker c
                let d = walker d
                Expr.App (a,b,c,d)
            | _ -> tree
        walker tree
        
    let rec rewriteInLetExp tree = 

        let isLetPlaceholder exp = 
            match exp with 
            | Expr.InLetPlaceholder -> true
            | _ -> false

        match tree with 
        | Expr.Sequential (s1,s2,s3,s4) -> 
            match s3 with 
            | Expr.LetOrUse (x,y,z,i) when isLetPlaceholder i -> 
                let e = rewriteInLetExp s4 //|> addNewLine file
                Expr.LetOrUse (x,y,z,e)
            | _ -> 
                Expr.Sequential (s1,s2,rewriteInLetExp s3,rewriteInLetExp s4)

        | Expr.App (a,b,c,d) -> 

            match c with 
            | Expr.LetOrUse (x,y,z,i) when isLetPlaceholder i -> Expr.LetOrUse(x,y,z, d)
            | _ -> Expr.App (a,b, rewriteInLetExp c, rewriteInLetExp d)
        | Expr.IfThenElse (a,b,c,d,e) -> 
            //match b with 
            //| Expr.LetOrUse (x,y,z,i) when isLetPlaceholder i -> 
            //    Expr.LetOrUse(x,y,z, Expr.Const  SynConst.Unit)
            //| _ -> 
                Expr.IfThenElse (a,rewriteInLetExp b,c |> Option.map rewriteInLetExp,d,e)
        | _ -> tree

    let rec addNewLineToLet file tree = 
        let moveDown = Range.moveDownLine file

        let rec loop tree = 
            match tree with 
            | SynExpr.Paren (x, r1, None, r) -> SynExpr.Paren (x, moveDown r1, None, moveDown r)
            | SynExpr.Const (x, r1) -> SynExpr.Const (x, moveDown r1)
            | SynExpr.Typed (expr, typeName, r1) -> SynExpr.Typed (expr, typeName, moveDown r1)
            | SynExpr.Tuple (xs, rs, r)  -> SynExpr.Tuple (xs, rs, moveDown r)
            | SynExpr.App (a,b, c, d, r) -> SynExpr.App (a,b, loop c, loop d, moveDown r)
            | SynExpr.LetOrUse (a,b,c, d, r1) -> SynExpr.LetOrUse (a,b,c, d, moveDown r1)
            | SynExpr.Sequential (a,b,c,d, r1) -> 
                SynExpr.Sequential (a,b, loop  c, loop d,  moveDown r1 )
            | SynExpr.IfThenElse (a, b,  c,d, e, r1, r2) -> 

                SynExpr.IfThenElse (loop a, loop b, c |> Option.map (loop ),d,e, moveDown r1, moveDown r2)
            | SynExpr.Ident (r) -> SynExpr.Ident (Ident(r.idText, moveDown r.idRange))
            | SynExpr.LongIdent (a,b, c, r) -> SynExpr.LongIdent (a,b, c, moveDown r)
            | SynExpr.Set (left, right, r) -> SynExpr.Set (left, right, moveDown r)
            | SynExpr.Null r -> SynExpr.Null <| moveDown r

        match tree with 
        | SynExpr.Sequential (s1,s2,s3,s4,s5) -> 
            let s3 = addNewLineToLet file s3
            let s4 = addNewLineToLet file s4
            SynExpr.Sequential (s1,s2, s3, s4, s5)

        | SynExpr.App (a,b,c,d,e) -> 
            let c = addNewLineToLet file c
            let d = addNewLineToLet file d
            SynExpr.App (a, b, c, d, e)

        | SynExpr.IfThenElse (a,b,c,d,e,f,g) -> 
            let a = addNewLineToLet file a
            let b = addNewLineToLet file b
            let c = c |> Option.map (addNewLineToLet file)
            SynExpr.IfThenElse (a,b, c, d, e, f,g)

        | SynExpr.LetOrUse (a,b,c,d,e) -> SynExpr.LetOrUse (a,b,c,loop d,e)
        | _ -> tree

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

        SynMemberDefn.Member 
            (SynBinding.Binding ( x.Accessibility, SynBindingKind.NormalBinding, false, false, [],
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
                x.Body |> rewriteInLetExp |> rewriteReturnInIf |> toSynExpr, //|> addNewLineToLet file |> snd,
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

        let properties = cn.Properties |> List.map toProperty
        let methods = cn.Methods |> List.map toMethod
        let fields = cn.Fields |> List.map toLet

        let ctor = SynMemberDefn.ImplicitCtor (None,[],[],None, range0)
        SynTypeDefn.TypeDefn (x, SynTypeDefnRepr.ObjectModel (TyconUnspecified, [ctor] @ fields @ properties @ methods, range0), [], range0)
        |> List.singleton
        |> (fun x -> SynModuleDecl.Types (x, range0))

    let t = tree.GetRoot()
    t.ChildNodes()
    |> Seq.fold (fun file node -> visitor.ParseSyntax file node) None
    |> Option.map (function 
        | CsToFs.File f -> 
            let mods = f.UsingStatements |> List.map createOpenStatements 

            let namespaces = 
                f.Namespaces |> List.map (fun x -> 
                    let classes = 
                        x.Classes |> List.map toClass
                    toNamespace x (mods @ classes) )
            toFile namespaces

        | CsToFs.UsingStatement us -> createOpenStatements us  |> List.singleton |> defaultModule |> toFile
        | CsToFs.Namespace ns -> toNamespace ns [] |> List.singleton |> toFile
        | CsToFs.Class cn ->  cn  |> toClass |> List.singleton |> defaultModule |> toFile
        | CsToFs.Method m ->  m |> toMethod |> toDefaultClass |> List.singleton |> defaultModule |> toFile
        )
    |> Option.map (fun tree -> CodeFormatter.FormatAST(tree, file, None, FormatConfig.Default))
    |> Option.map(fun fsharpSource -> 
        let tree = getUntypedTree(file, fsharpSource)

        match tree with
        | ParsedInput.ImplFile(implFile) ->
            // Extract declarations and walk over them
            let (ParsedImplFileInput(fn, script, name, a, b, modules, c)) = implFile

            let modules = 
                modules |> List.map (fun (SynModuleOrNamespace (a,b,c,modules,e,f,g,h)) -> 
                    let modules = 
                        modules |> List.map (fun x -> 
                            match x with 
                            | SynModuleDecl.Types (a,b) -> 
                                let a = 
                                    a |> List.map (fun (SynTypeDefn.TypeDefn (a,b,members,d)) -> 

                                        let b = 
                                            match b with 
                                            | SynTypeDefnRepr.ObjectModel (a,b,c) -> 
                                                let b = 
                                                    b |> List.map (fun x -> 
                                                        match x with 
                                                        | SynMemberDefn.Member ((Binding (a,b,c,d,e,f,g,h,i,expr,k,l)),m2) -> 
                                                            let m2 = Range.addLine file m2
                                                            printfn "%A" m2


                                                            let expr = expr |> addNewLineToLet file
                                                            printfn "%A" expr

                                                            SynMemberDefn.Member ((Binding (a,b,c,d,e,f,g,h,i,expr,k,l)),m2)

                                                        | x -> x
                                                    )

                                                SynTypeDefnRepr.ObjectModel (a,b,c)
                                            | x -> x

                                        let members = 
                                            members |> List.map (fun x -> 
                                                match x with 
                                                | SynMemberDefn.Member ((Binding (a,b,c,d,e,f,g,h,i,expr,k,l)),m2) -> 
                                                    let m2 = Range.addLine file m2
                                                    printfn "%A" m2


                                                    let expr = expr |> addNewLineToLet file
                                                    printfn "%A" expr

                                                    SynMemberDefn.Member ((Binding (a,b,c,d,e,f,g,h,i,expr,k,l)),m2)

                                                | x -> x
                                            )
                                        SynTypeDefn.TypeDefn (a,b,members,d)
                                    )
                                SynModuleDecl.Types (a,b)
                            | x -> x )
                    SynModuleOrNamespace (a,b,c,modules,e,f,g,h)
                )
            ParsedImplFileInput(fn, script, name, a, b, modules, c) |> ParsedInput.ImplFile
        | _ -> failwith "F# Interface file (*.fsi) not supported."

        )
    |> Option.iter (fun tree -> 
        printfn "------------------------------------------------------------"
        printfn "%A" tree

        printfn "------------------------------------------------------------"

        let config = 
            {
                FormatConfig.Default with 
                    FormatConfig.SemicolonAtEndOfLine = false
                    FormatConfig.StrictMode = true
                    FormatConfig.PageWidth = 80
                    FormatConfig.SpaceAfterComma = true
                    FormatConfig.SpaceBeforeArgument = true
                    FormatConfig.SpaceBeforeColon = true
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
        type Foo() = 
            x

            member val smartTagPopupTimeoutId:uint = null with get,set

            member this.RemoveWidget () = 
                CancelQuickFixTimer ()
                let mutable token = quickFixCancellationTokenSource.Token 
                CancelSmartTagPopupTimeout ()
            
    //                """
    // File name in Unix format
    let file = "/home/user/Test.fsx"

    // Get the AST of sample F# code
    let tree = getUntypedTree(file, input)

    printfn "%A" tree
    CodeFormatter.FormatAST(tree, file, None, FormatConfig.Default) |> printfn "%s"

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


