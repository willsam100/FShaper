module FSharper.Core.Converter
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Range
open Microsoft.CodeAnalysis.CSharp
open Fantomas.FormatConfig
open Fantomas
open FSharper.Core.TreeOps
open System.Text
open System.IO
open Microsoft.CodeAnalysis.CSharp.Syntax

module FormatOuput = 

    let createOpenStatements (name: UsingStatement) = 
        (LongIdentWithDots (name.UsingNamespace |> toIdent, [range0]), range0) |> SynModuleDecl.Open 

    let toNamespace (ns:Namespace) mods = 
        SynModuleOrNamespace (toIdent ns.Name,false,false, mods, PreXmlDocEmpty, [], None, range0)

    let defaultModule mods = 
        [SynModuleOrNamespace (toIdent DefaultNames.namespaceName,false,true, mods, PreXmlDocEmpty, [], None, range0)]

    let toFile moduleOrNs = 
        ParsedImplFileInput (DefaultNames.file, true, QualifiedNameOfFile (Ident()), [], [], moduleOrNs, (true, true)) 
        |> ParsedInput.ImplFile

    let inMethod (x:Field) = 
        let init = 
            match x.Initilizer with 
            | Some x -> x
            | None -> Expr.Const SynConst.Unit

        let var = 
            FSharpBinding.LetBind 
                (
                    None, SynBindingKind.NormalBinding, false, not x.IsConst, [], 
                    SynValData (None, SynValInfo ([], SynArgInfo ([], false, None )), None), 
                    Pat.Named (Pat.Wild, Ident(x.Name, range0), false, None), init)
        let x = Expr.LetOrUse (false, false, [var], Expr.Const <| SynConst.String (DefaultNames.namespaceName, range0))

        {
            Name = DefaultNames.method
            Parameters = []
            Body = x
            ReturnType = SynType.StaticConstant (SynConst.Unit, range0)
            IsVirtual = false
            IsAsync = false
            IsPrivate = false
            IsOverride = false
            IsStatic = false
            Accessibility = None
            Attributes = []
        }

    let santizeCode methodNames expr = 

        
    
        let e = 
            expr
            |> simplifyTree
            |> rewriteReturnInIf 
            |> rewriteMatchIs
            |> replaceDotGetIfNotInLetBinding
            |> rewriteInLetExp 
            |> wrapNewKeyword
            |> rewriteMethodWithPrefix methodNames
            |> fixCsharpReservedNames
            |> rewriteActionOrFuncToUseCallInvoke
        match shouldWrapInComp e with 
        | Some x ->  wrapInComp x e
        | None -> e

    let toMethod methodNames (x:Method) = 
        let methodName = 
            if x.IsStatic 
            then toLongIdentWithDots x.Name
            else LongIdentWithDots (toIdent ("this." + x.Name), [range0])

        let argInfos = []
            //let args = 
            //    x.Parameters |> List.map (fun x -> SynArgInfo ([],false, Ident(x.Name, range0) |> Some  ) )
            //[args]

        let namedArgs = 
            let typeArgs = 
                x.Parameters 
                //|> List.map (fun x -> 
                    //SynPat.Typed (
                        //SynPat.Named (SynPat.Wild range0, Ident(x.Name, range0), false, None, range0), x.Type, range0) )
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
            let t = santizeCode methodNames x.Body
            match x.ReturnType with 
            | SynType.LongIdent (x) when joinLongIdentWithDots x = "unit" -> t
            | _ ->  Expr.Typed (t, x.ReturnType)
            
        printfn "Transformed Tree:"
        printfn "%A" trandformedTree

        let returnType = SynBindingReturnInfo.SynBindingReturnInfo (x.ReturnType, range0, [])

        SynMemberDefn.Member 
            (SynBinding.Binding ( x.Accessibility, SynBindingKind.NormalBinding, false, false, attributres,
                PreXmlDoc.PreXmlDocEmpty,
                SynValData (
                    Some {
                        MemberFlags.IsInstance = not x.IsStatic
                        MemberFlags.IsDispatchSlot = false 
                        MemberFlags.IsOverrideOrExplicitImpl = false 
                        MemberFlags.IsFinal = false
                        MemberFlags.MemberKind = MemberKind.Member
                    }, SynValInfo (argInfos, SynArgInfo ([], false, None)), None),
                SynPat.LongIdent
                    (methodName, None, None, 
                    Pats [namedArgs], None, range0 ), 
                Some returnType, 
                trandformedTree |> toSynExpr,
                range0, 
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
            let getter = santizeCode [] getter
        
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
            let setter = santizeCode [] setter
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
            ComponentInfo ([], [], [], toIdent DefaultNames.className, PreXmlDocEmpty, false, None, range0)

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
            SynBinding.Binding (None, SynBindingKind.NormalBinding, false, not x.IsConst, [], PreXmlDocEmpty, 
                SynValData (
                    None, SynValInfo ([], SynArgInfo ([], false, Ident (x.Name, range0) |> Some )), None), 
                SynPat.LongIdent
                    (LongIdentWithDots (toIdent x.Name, [range0]), None, None, 
                    Pats ([]), None, range0 ), None, 
                    toSynExpr init, range0 , SequencePointAtBinding range0)
        SynMemberDefn.LetBindings ([binding], x.IsStatic, false, range0)
       

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
                    SynAttribute.AppliesToGetterAndSetter = false
                    SynAttribute.Range = range0
                    SynAttribute.Target = None
                }
            )
            
        let typeVals = 
            cn.TypeParameters
            |> List.map (fun t -> TyparDecl ([], Typar (ident (t, range0),NoStaticReq, false)) )
            
        let x = 
            ComponentInfo (att, typeVals, [], toIdent cn.Name.Name, PreXmlDocEmpty, false, None, range0)

        
        let properties = cn.Properties |> List.collect toProperty
        let methodNames = cn.Methods |> List.map (fun x -> (if x.IsStatic then Some cn.Name.Name else None), x.Name.Replace ("this.", ""))
        let methods = cn.Methods |> List.map (toMethod methodNames)
        let fields = cn.Fields |> List.map toLet

        let interfaces = 
            cn.ImplementInterfaces 
            |> List.map (fun x -> 

                let method = {
                    Method.Accessibility = None
                    Method.Body = Expr.Const SynConst.Unit
                    Method.Name = "Todo"
                    Method.IsAsync = false
                    Method.IsOverride = false
                    Method.IsVirtual = false
                    Method.IsPrivate = true
                    Method.IsStatic = false
                    Method.Parameters = []
                    Method.Attributes = []
                    Method.ReturnType = SynType.LongIdent (toLongIdentWithDots "unit")
                }

                SynMemberDefn.Interface (x,method |> toMethod [] |> List.singleton |> Some, range0))

        let ctors = 

            let mainCtor = 
                cn.Constructors 
                |> List.sortByDescending (fun x -> x.Parameters.Length)
                |> List.tryHead

            let ctor = 
                mainCtor
                |> Option.map (fun x -> 
                    SynMemberDefn.ImplicitCtor (None,[], x.Parameters , None, range0) )
                    
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
                    (baseClass, args, None, range0)
                    
            ) |> function 
            | Some x -> [ctor; x]
            | None -> [ctor]

        SynTypeDefn.TypeDefn (x, SynTypeDefnRepr.ObjectModel (TyconUnspecified, ctors @ fields @ properties @ methods @ interfaces, range0), [], range0)
        |> List.singleton
        |> (fun x -> SynModuleDecl.Types (x, range0))

let toFsharpSynaxTree input = 
    match input with 
    | File f -> 

        let defaultNamespace = {Name = DefaultNames.namespaceName; Interfaces = []; Classes = [] }

        match f with 
        | FileWithUsing (usings, classes) -> 
            let ns = {defaultNamespace with Classes = classes }
            let mods = usings |> List.map FormatOuput.createOpenStatements
            let namespaces = 
                let classes = ns.Classes |> List.map FormatOuput.toClass
                [FormatOuput.toNamespace ns (mods @ classes)]
            FormatOuput.toFile namespaces

        | FileWithUsingNamespace (usngs, namespaces) -> 
            let mods = usngs |> List.map FormatOuput.createOpenStatements 
            let ns = 
                match namespaces with 
                | [] -> [defaultNamespace]
                | xs -> xs 

            let namespaces = 
                ns |> List.map (fun x -> 
                    let classes = 
                        x.Classes |> List.map FormatOuput.toClass
                    FormatOuput.toNamespace x (mods @ classes) )
            FormatOuput.toFile namespaces

        | FileWithUsingNamespaceAndDefault (usngs, namespaces, classes) -> 
            let mods = usngs |> List.map FormatOuput.createOpenStatements 
            let ns = 
                match namespaces, classes with 
                | [], [] -> [defaultNamespace]
                | [], classes -> [{defaultNamespace with Classes = classes }]
                | ns, [] -> ns
                | ns, classes -> {defaultNamespace with Classes = classes } :: ns

            let namespaces = 
                ns |> List.map (fun x -> 
                    let classes = 
                        x.Classes |> List.map FormatOuput.toClass
                    FormatOuput.toNamespace x (mods @ classes) )
            FormatOuput.toFile namespaces


    | UsingStatement us -> 
        let ns = {Name = DefaultNames.namespaceName; Interfaces = []; Classes = [] }           
        FormatOuput.toNamespace ns [FormatOuput.createOpenStatements us] |> List.singleton |> FormatOuput.toFile

    | Namespace ns -> FormatOuput.toNamespace ns [] |> List.singleton |> FormatOuput.toFile
    | Class cn ->  cn  |> FormatOuput.toClass |> List.singleton |> FormatOuput.defaultModule |> FormatOuput.toFile
    //| Method m ->  m |> FormatOuput.toMethod [m.Name] |> FormatOuput.toDefaultClass |> List.singleton |> FormatOuput.defaultModule |> FormatOuput.toFile
    //| FSharper.Core.Field f ->  f |> Seq.head |> FormatOuput.inMethod |> FormatOuput.toMethod [] |> FormatOuput.toDefaultClass |> List.singleton |> FormatOuput.defaultModule |> FormatOuput.toFile

// One of th goals of this project is to support converting non-complete C# ie that from a blog post. 
// To compile the code, scaffolding must be added ie a namespace, class etc. 
// the output should match the input; this removes the scaffolding.
let removeDefaultScaffolding (fsharpOutput:string) = 
    fsharpOutput
    |> (fun x -> x.Replace("namespace ``Program35949ae4-3f6e-11e9-b4dc-230deb73e77f``\n", ""))
    |> (fun x -> x.Replace("type ``Klass067803f4-3f6e-11e9-b4df-6f8305ceb4a6``() =\n", ""))
    |> (fun x -> x.Replace("module ``Program35949ae4-3f6e-11e9-b4dc-230deb73e77f``\n\n", ""))
    |> (fun x -> x.Replace("\"Program35949ae4-3f6e-11e9-b4dc-230deb73e77f\"\n", ""))
    |> (fun x -> x.Replace("    member this.Method156143763f6e11e984e11f16c4cfd728() =\n", ""))
    |> (fun x -> x.Replace("""\010""", "\\n"))
    |> (fun x -> x.Replace("""\009""", "\\t"))
    |> (fun x -> x.Replace("""\013""", "\\r"))


let toFsharpString validateCode config parseInput = 
    if CodeFormatter.IsValidAST parseInput then

        let source = 
             """type Klass067803f4-3f6e-11e9-b4df-6f8305ceb4a6() =  
                   member this.Foo() = Console.WriteLine('\n')"""

        let tree = CodeFormatter.FormatAST(parseInput, DefaultNames.file, Some source, config) 
        printfn "\n%s" tree
        let tree = 
            try
                CodeFormatter.FormatDocument(DefaultNames.file, tree, config) 
            with e -> 
                if validateCode 
                then raise e
                else tree

        tree
        |> removeDefaultScaffolding
        |> (fun x -> 

            let rec reduceIndent (x:string) = 
                if x.StartsWith "    " |> not then x 
                else 
                    let trim (x:string) =  if x.StartsWith "    " then x.Substring 4 else x
                    x.Split '\n' |> Array.map trim |> String.concat "\n" |> reduceIndent
            reduceIndent x
                
                )
    else  "Bad F# syntax tree" 

let runWithConfig validateCode (input:string) = 

    let config = 
            {
                FormatConfig.Default with 
                    FormatConfig.SemicolonAtEndOfLine = false
                    FormatConfig.StrictMode = false
                    FormatConfig.PageWidth = 120
                    FormatConfig.SpaceAfterComma = true
                    FormatConfig.SpaceBeforeArgument = true
                    FormatConfig.SpaceBeforeColon = false
            }

    let tree = 
        SyntaxFactory.ParseSyntaxTree (text = input, encoding = Encoding.UTF8)
        |> (fun x -> 
            let t = x.GetRoot()

            let hasValidNode = 
                t.ChildNodes() |> Seq.exists (function 
                    | :? UsingDirectiveSyntax
                    | :? NamespaceDeclarationSyntax
                    | :? MethodDeclarationSyntax 
                    | :? InterfaceDeclarationSyntax
                    | :? ClassDeclarationSyntax -> true
                    | _ -> false )

            if t.GetDiagnostics() |> Seq.isEmpty && hasValidNode then Some x
            else
                let x = 
                    sprintf """
                        public void Method156143763f6e11e984e11f16c4cfd728() {
                            %s
                        }
                    """ input
                    |> SyntaxFactory.ParseSyntaxTree

                if Seq.isEmpty <| x.GetDiagnostics() then Some x 
                else
                    printfn "Invalid or Incomplete C#"
                    x.GetDiagnostics() |> Seq.iter (printfn "%A")
                    None
        )

    let visitor = new FSharperTreeBuilder()

    let nodes = tree |> Option.map (fun x -> x.GetRoot().ChildNodes())

    nodes
    |> Option.bind(fun x ->  x |> Seq.fold (visitor.ParseSyntax) None)
    |> Option.map toFsharpSynaxTree 
    |> Option.map(removeFhsarpIn DefaultNames.file)
    |> Option.map (toFsharpString validateCode config) 
    |> function 
    | Some x -> x.Trim()
    | None -> "Invalid C# syntax tree"

let run (input:string) = 
    runWithConfig true input