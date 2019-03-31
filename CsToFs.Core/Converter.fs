module CsToFs.Core.Converter
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Range
open Microsoft.CodeAnalysis.CSharp
open Fantomas.FormatConfig
open Fantomas
open CsToFs
open CsToFs.Core.TreeOps

module FormatOuput = 

    let file = "unknown.fs"
    let createOpenStatements (name: UsingStatement) = 
        (LongIdentWithDots (name.Namespace |> toIdent, [range0]), range0) |> SynModuleDecl.Open 

    let toNamespace (ns:Namespace) mods = 
        SynModuleOrNamespace (toIdent ns.Name,false,false, mods, PreXmlDocEmpty, [], None, range0)

    let defaultModule mods = 
        [SynModuleOrNamespace (toIdent "Program35949ae4-3f6e-11e9-b4dc-230deb73e77f",false,true, mods, PreXmlDocEmpty, [], None, range0)]

    let toFile moduleOrNs = 
        ParsedImplFileInput (file, true, QualifiedNameOfFile (Ident()), [], [], moduleOrNs, (true, true)) 
        |> ParsedInput.ImplFile

    let inMethod (x:Field) = 
        let init = 
            match x.Initilizer with 
            | Some x -> x
            | None -> Expr.Const SynConst.Unit

        let var = 
            CsToFsBinding.LetBind 
                (
                    None, SynBindingKind.NormalBinding, false, false, [], 
                    SynValData (None, SynValInfo ([], SynArgInfo ([], false, None )), None), 
                    Pat.Named (Pat.Wild, Ident(x.Name, range0), false, None), init)
        let x = Expr.LetOrUse (false, false, [var], Expr.Const <| SynConst.String ("Program35949ae4-3f6e-11e9-b4dc-230deb73e77f", range0))

        {
            Name = "Method156143763f6e11e984e11f16c4cfd728"
            Parameters = []
            Body = x
            ReturnType = "void"
            IsVirtual = false
            IsAsync = false
            IsPrivate = false
            IsOverride = false
            Accessibility = None
            Attributes = []
        }

    let toMethod methodNames (x:Method) = 
        let methodName = LongIdentWithDots (toIdent ("this." + x.Name), [range0])

        let argInfos = 
            let args = 
                x.Parameters |> List.map (fun x -> SynArgInfo ([],false, Ident(x.Name, range0) |> Some  ) )
            let returnArgInfo = SynArgInfo ([],false, Ident(x.ReturnType, range0) |> Some  ) 
            [returnArgInfo] :: [args]

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
            
        let trandformedTree = 
            x.Body 
            |> replaceDotGetIfNotInLetBinding
            |> rewriteInLetExp 
            |> rewriteReturnInIf 
            |> wrapNewKeyword
            |> rewriteMethodWithPrefix methodNames

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

        let properyName = LongIdentWithDots (toIdent ("this." + x.Name), [range0])

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

        let makeGetter getter = 
            let memberOptions = 
                {   
                    IsInstance = true
                    IsDispatchSlot = false
                    IsOverrideOrExplicitImpl = false
                    IsFinal = false
                    MemberKind = MemberKind.PropertyGet }

            let synVaInfo = 
                SynValInfo
                    ([[SynArgInfo ([],false,None)]; []],SynArgInfo ([],false,None))
                         
            let headPat = 
                SynPat.LongIdent
                    (properyName, toSingleIdent "set" |> Some, None, 
                            SynConstructorArgs.Pats (
                                [SynPat.Paren (SynPat.Const (SynConst.Unit, range0), range0  )]),
                                None, range0)

            let returnInfo = SynBindingReturnInfo (toLongIdentWithDots x.Name |> SynType.LongIdent , range0, [])

            SynMemberDefn.Member (
                SynBinding.Binding
                    (None, NormalBinding, false, false, [], 
                        PreXmlDocEmpty, SynValData (Some memberOptions, synVaInfo, None), headPat, Some returnInfo, getter |> toSynExpr, range0, NoSequencePointAtInvisibleBinding), range0)   

        let makeSetter setter = 
            let memberOptions = 
                {   
                    IsInstance = true
                    IsDispatchSlot = false
                    IsOverrideOrExplicitImpl = false
                    IsFinal = false
                    MemberKind = MemberKind.PropertySet }

            let setterIdent = toSingleIdent "value" |> Some

            let synVaInfo = 
               SynValInfo
                 ([
                    [SynArgInfo ([],false,None)]
                    [SynArgInfo ([],false, setterIdent)  ]],
                    SynArgInfo ([],false,None) )

            let headPat = 
                SynPat.LongIdent 
                    (properyName, toSingleIdent "set" |> Some, None, 
                            SynConstructorArgs.Pats (
                                [SynPat.Named (SynPat.Wild range0, toSingleIdent "value", false, None,range0)]),
                                None, range0)
                                
            SynMemberDefn.Member (
                SynBinding.Binding
                    (None, NormalBinding, false, false, [], 
                        PreXmlDocEmpty, SynValData (Some memberOptions, synVaInfo, None), headPat, None, setter |> toSynExpr, range0, NoSequencePointAtInvisibleBinding), range0)   

        match x.Get, x.Set with 
        | None, None -> 

            SynMemberDefn.AutoProperty 
                ([],false, 
                    ident (x.Name, range0), 
                    SynType.LongIdent (LongIdentWithDots (toIdent x.Type,[range0]) ) |> Some,
                    MemberKind.PropertyGetSet, memberFlags, PreXmlDoc.PreXmlDocEmpty, x.Access, SynExpr.Null range0, None, range0
                      ) |> List.singleton

        | Some getter, None -> [makeGetter getter]
        | None, Some setter -> [makeSetter setter;]
        | Some getter, Some setter -> [makeGetter getter; makeSetter setter;]

    let toDefaultClass method = 
        let x = 
            ComponentInfo ([], [], [], toIdent "Klass067803f4-3f6e-11e9-b4df-6f8305ceb4a6", PreXmlDocEmpty, false, None, range0)

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
                    | [] -> SynExpr.Paren (SynExpr.Ident (Ident("",range0)), range0, None, range0) 
                    | xs -> 

                        let items = xs |> List.map (function
                            | AttributeValue.AttributeValue x -> toSynExpr x
                            | AttributeValue.NamedAttributeValue (left, right) -> 
                                let left = toSynExpr left
                                let right = toSynExpr right
                                
                                let infixEquals = 
                                    SynExpr.App
                                        (ExprAtomicFlag.NonAtomic, true, toSingleIdent "op_Equality" |> SynExpr.Ident, left, range0)

                                SynExpr.App (ExprAtomicFlag.NonAtomic, false, infixEquals, right, range0)
                        )
                      
                        let tuple = 
                            SynExpr.Tuple (items, [], range0)

                        SynExpr.Paren (tuple, range0, None, range0) 

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

        let properties = cn.Properties |> List.collect toProperty
        let methodNames = cn.Methods |> List.map (fun x -> x.Name.Replace ("this.", ""))
        let methods = cn.Methods |> List.map (toMethod methodNames)
        let fields = cn.Fields |> List.map toLet

        let ctors = 

            let mainCtor = 
                cn.Constructors 
                |> List.sortByDescending (fun x -> x.Parameters.Length)
                |> List.tryHead

            let ctor = 
                mainCtor
                |> Option.map (fun x -> 

                    let classArgs = 
                        x.Parameters |> List.map (fun x -> 
                            SynSimplePat.Typed
                                (SynSimplePat.Id 
                                    (toSingleIdent x.Name, None, 
                                    false, false, false, range0), 
                                toLongIdentWithDots x.Type |> SynType.LongIdent, range0) )
                    SynMemberDefn.ImplicitCtor (None,[], classArgs, None, range0) )
                    
                |> function 
                | Some c -> c
                | None -> SynMemberDefn.ImplicitCtor (None,[],[],None, range0)

            cn.BaseClass |> Option.map (fun baseClass -> 

                let args = 
                    mainCtor
                    |> Option.map (fun x -> 
                        let items = x.SubclassArgs |> List.map (toSingleIdent >> SynExpr.Ident)
                        SynExpr.Paren (SynExpr.Tuple (items, [], range0), range0, None, range0) )
                        
                    |> function
                    | Some x -> x
                    | None -> SynExpr.Const (SynConst.Unit, range0)

                SynMemberDefn.ImplicitInherit 
                    (SynType.LongIdent (baseClass |> toLongIdentWithDots), args, None, range0)
                    
            ) |> function 
            | Some x -> [ctor; x]
            | None -> [ctor]

        SynTypeDefn.TypeDefn (x, SynTypeDefnRepr.ObjectModel (TyconUnspecified, ctors @ fields @ properties @ methods, range0), [], range0)
        |> List.singleton
        |> (fun x -> SynModuleDecl.Types (x, range0))

let run (input:string) = 

    let tree = 
        input
        |> SyntaxFactory.ParseSyntaxTree
        |> (fun x -> 
            let t = x.GetRoot()
            if t.GetDiagnostics() |> Seq.isEmpty then x
            else
                let x = 
                    sprintf """
                        public void Method156143763f6e11e984e11f16c4cfd728() {
                            %s
                        }
                    """ input
                    |> SyntaxFactory.ParseSyntaxTree

                if Seq.isEmpty <| x.GetDiagnostics() then x 
                else
                    printfn "Invalid or Incomplete C#"
                    x.GetDiagnostics() |> Seq.iter (printfn "%A")
                    exit 1
        )

    let visitor = new FileContentsDumper()
    let indent = " " |> List.replicate 4 |> String.concat ""

    let t = tree.GetRoot()
    t.ChildNodes()
    |> Seq.fold (visitor.ParseSyntax) None
    |> Option.map (fun x -> 
        match x with 
        | CsToFs.Core.File f -> 
            let mods = f.UsingStatements |> List.map FormatOuput.createOpenStatements 
            let ns = 
                match f.Namespaces with 
                | [] -> [{Name = "Namespace579084dc-3f6e-11e9-85bb-d721e145d6a1"; Interfaces = []; Classes = [] }]
                | xs -> xs 

            let namespaces = 
                ns |> List.map (fun x -> 
                    let classes = 
                        x.Classes |> List.map FormatOuput.toClass
                    FormatOuput.toNamespace x (mods @ classes) )
            FormatOuput.toFile namespaces

        | CsToFs.Core.UsingStatement us -> 
            let ns = {Name = "Namespace579084dc-3f6e-11e9-85bb-d721e145d6a1"; Interfaces = []; Classes = [] }           
            FormatOuput.toNamespace ns [FormatOuput.createOpenStatements us] |> List.singleton |> FormatOuput.toFile
        
                //   |> List.singleton |> defaultModule |> toFile
        | CsToFs.Core.Namespace ns -> FormatOuput.toNamespace ns [] |> List.singleton |> FormatOuput.toFile
        | CsToFs.Core.Class cn ->  cn  |> FormatOuput.toClass |> List.singleton |> FormatOuput.defaultModule |> FormatOuput.toFile
        | CsToFs.Core.Method m ->  m |> FormatOuput.toMethod [m.Name] |> FormatOuput.toDefaultClass |> List.singleton |> FormatOuput.defaultModule |> FormatOuput.toFile
        | CsToFs.Core.Field f ->  f |> Seq.head |> FormatOuput.inMethod |> FormatOuput.toMethod [] |> FormatOuput.toDefaultClass |> List.singleton |> FormatOuput.defaultModule |> FormatOuput.toFile
        )
    |> Option.map(removeFhsarpIn FormatOuput.file)
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
    |> Option.map (fun tree -> 
        printfn "------------------------------------------------------------"
        printfn "%A" tree
        printfn "------------------------------------------------------------"

        let config = 
            {
                FormatConfig.Default with 
                    FormatConfig.SemicolonAtEndOfLine = false
                    FormatConfig.StrictMode = true
                    FormatConfig.PageWidth = 120
                    FormatConfig.SpaceAfterComma = true
                    FormatConfig.SpaceBeforeArgument = true
                    FormatConfig.SpaceBeforeColon = false
            }

        if CodeFormatter.IsValidAST tree then

            let tree = CodeFormatter.FormatAST(tree, FormatOuput.file, None, config) 

            tree
            |> (fun x -> x.Replace("namespace ``Namespace579084dc-3f6e-11e9-85bb-d721e145d6a1``\n", ""))
            |> (fun x -> x.Replace("type ``Klass067803f4-3f6e-11e9-b4df-6f8305ceb4a6``() =\n", ""))
            |> (fun x -> x.Replace("module ``Program35949ae4-3f6e-11e9-b4dc-230deb73e77f``\n\n", ""))
            |> (fun x -> x.Replace("\"Program35949ae4-3f6e-11e9-b4dc-230deb73e77f\"\n", ""))
            |> (fun x -> x.Replace("member this.Method156143763f6e11e984e11f16c4cfd728() =\n", ""))
            |> (fun x -> 
                let newLines = x.ToCharArray() |> Array.filter (fun x -> x ='\n') |> Array.length
                if newLines >= 2 && (x.Contains "type" || x.Contains "module") then x 
                else 
                    let reduceIndent (x:string) = 
                        if x.StartsWith "    " then x.Substring 4 else x                        
                        
                    x.Split '\n' |> Array.map reduceIndent |> String.concat "\n" )
        else  "Bad F# syntax tree" ) 
    |> function 
    | Some x -> x.Trim()
    | None -> "Invalid C# syntax tree"
