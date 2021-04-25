module FShaper.Core.Converter
open FSharp.Compiler.SyntaxTree
open FSharp.Compiler.Range
open FSharp.Compiler.XmlDoc
open FShaper.Core
open Fantomas.TriviaTypes
open Microsoft.CodeAnalysis.CSharp
open Fantomas.FormatConfig
open Fantomas
open FShaper.Core.TreeOps
open System.Text
open Microsoft.CodeAnalysis.CSharp.Syntax
open Fantomas.SourceOrigin
open FSharp.Compiler.SourceCodeServices

module FormatOutput = 

    let createOpenStatements (name: UsingStatement) =
        (SynOpenDeclTarget.ModuleOrNamespace (toIdent name.UsingNamespace, range0), range0) |> SynModuleDecl.Open 

    let createSynAttribute x = 
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
                    SynExpr.Tuple (false, items, [], range0)

                SynExpr.Paren (tuple, range0, None, range0) 

        {
            SynAttribute.TypeName = x.Name
            SynAttribute.ArgExpr = arg
            SynAttribute.AppliesToGetterAndSetter = x.AppliesToGetterAndSetter
            SynAttribute.Range = range0
            SynAttribute.Target = x.Target
        }

    let createAttributeStatements (name: Attribute list) = 
        name 
        |> List.map createSynAttribute
        |> List.collect (fun x ->  
            let list = {Attributes = [x]; Range = range0}      
            [
                SynModuleDecl.Attributes ([list], range0); 
                SynModuleDecl.DoExpr (NoDebugPointAtDoBinding, 
                    SynExpr.Do (SynExpr.Const (SynConst.Unit, range0), range0), range0)
            ])

    let toNamespace (ns:Namespace) mods = 
        SynModuleOrNamespace (toIdent ns.Name, false, DeclaredNamespace, mods, PreXmlDocEmpty, [], None, range0)
   

    let defaultModule mods = 
        [SynModuleOrNamespace (toIdent DefaultNames.namespaceName,false, NamedModule, mods, PreXmlDocEmpty, [], None, range0)]

    let toFile moduleOrNs = 
        ParsedImplFileInput (DefaultNames.file, true, QualifiedNameOfFile (Ident()), [], [], moduleOrNs, (true, true)) 
        |> ParsedInput.ImplFile

    let inMethod (x:Field) = 
        let init = 
            match x.Initializer with 
            | Some x -> x
            | None -> Expr.Const SynConst.Unit

        let var = 
            FSharpBinding.LetBind 
                (
                    None, SynBindingKind.NormalBinding, false, not x.IsConst, [], 
                    SynValData (None, SynValInfo ([], SynArgInfo ([], false, None )), None), 
                    Pat.Named (Pat.Wild, Ident(x.Name |> SynPat.getName, range0), false, None), init)
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
            Trivia = NoTrivia
        }

    let sanitizeCode requiresReturn methodNames expr =
        let trivia = getTrivia expr
        
        let e = 
            expr
            |> removeTrivia
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
        | Some x ->  wrapInComp requiresReturn x e, trivia
        | None -> e, trivia

    let toMethod methodNames (x:Method) =
        
        let trivia = x.Trivia |> function | NoTrivia -> [] | Above s -> [x.Name, s] 
        
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
            SynPat.Paren (SynPat.Tuple (false, typeArgs, range0), range0)

        let attributes = 
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

        let returnType = SynBindingReturnInfo.SynBindingReturnInfo (x.ReturnType, range0, [])

        let transformedTree, comments = 
            let (t, comments) = sanitizeCode x.ReturnType methodNames x.Body
            match x.ReturnType with 
            | SynType.LongIdent (x) when joinLongIdentWithDots x = "unit" -> t, comments
            | _ ->  Expr.Typed (t, x.ReturnType), comments
            
        let body = transformedTree |> toSynExpr


        SynMemberDefn.Member 
            (SynBinding.Binding ( x.Accessibility, SynBindingKind.NormalBinding, false, false, [{Attributes = attributes; Range = range0}],
                PreXmlDoc.PreXmlDocEmpty,
                SynValData (
                    Some {
                        MemberFlags.IsInstance = not x.IsStatic
                        MemberFlags.IsDispatchSlot = false 
                        MemberFlags.IsOverrideOrExplicitImpl = x.IsOverride 
                        MemberFlags.IsFinal = false
                        MemberFlags.MemberKind = MemberKind.Member
                    }, SynValInfo (argInfos, SynArgInfo ([], false, None)), None),
                SynPat.LongIdent
                    (methodName, None, None, 
                    Pats [namedArgs], None, range0 ), 
                Some returnType, 
                body,
                range0, 
                NoDebugPointAtDoBinding
            ), range0), trivia @ comments

    let toProperty (x:Prop) = 

        let propertyName = LongIdentWithDots (toIdent ("this." + x.Name), [range0])

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
            let getter, comments = sanitizeCode x.Type [] getter
        
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
                    (propertyName, toSingleIdent "set" |> Some, None, 
                            Pats (
                                [SynPat.Paren (SynPat.Const (SynConst.Unit, range0), range0  )]),
                                None, range0)

            let returnInfo = SynBindingReturnInfo (x.Type, range0, [])

            SynMemberDefn.Member (
                SynBinding.Binding
                    (None, NormalBinding, false, false, [], 
                        PreXmlDocEmpty, SynValData (Some memberOptions, synVaInfo, None), headPat, Some returnInfo, getter |> toSynExpr, range0, NoDebugPointAtDoBinding), range0), comments   

        let makeSetter setter = 
            let setter, comments = sanitizeCode x.Type [] setter
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
                    (propertyName, toSingleIdent "set" |> Some, None, 
                            Pats (
                                [SynPat.Named (SynPat.Wild range0, toSingleIdent "value", false, None,range0)]),
                                None, range0)
                                
            SynMemberDefn.Member (
                SynBinding.Binding
                    (None, NormalBinding, false, false, [], 
                        PreXmlDocEmpty, SynValData (Some memberOptions, synVaInfo, None), headPat, None, setter |> toSynExpr, range0, NoDebugPointAtDoBinding), range0), comments   

        match x.Get, x.Set with 
        | None, None -> 

            let typeArg = Expr.TypeApp (toLongIdent "Unchecked.defaultof", [x.Type])
            SynMemberDefn.AutoProperty 
                ([],false, 
                    Ident (x.Name, range0), Some x.Type,
                    MemberKind.PropertyGetSet, memberFlags, PreXmlDoc.PreXmlDocEmpty, x.Access, toSynExpr typeArg, None, range0
                      ) |> List.singleton, []

        | Some getter, None ->
            let g, comments = makeGetter getter
            [g], comments
        | None, Some setter ->
            let s, comments = makeSetter setter
            [s], comments
        | Some getter, Some setter ->
            let g, commentsGet = makeGetter getter
            let s, commentsSet = makeSetter setter
            
            let comments = 
                match commentsGet, commentsSet with
                | xs, [] -> xs
                | _ -> failwithf "Comments are not clear: %A %A" commentsGet commentsSet
            
            [g; s], comments 

    let toDefaultClass method = 
        let x = 
            ComponentInfo ([], [], [], toIdent DefaultNames.className, PreXmlDocEmpty, false, None, range0)

        let methods = [method]
        let ctor = SynMemberDefn.ImplicitCtor (None,[],SynSimplePats.SimplePats ([], range0) ,None, PreXmlDoc.Empty, range0)

        SynTypeDefn.TypeDefn (x, SynTypeDefnRepr.ObjectModel (TyconUnspecified, [ctor] @ methods, range0), [], range0)
        |> List.singleton
        |> (fun x -> SynModuleDecl.Types (x, range0))

    let toLet (x:Field) = 

        let init = 
            match x.Initializer with 
            | Some x -> x
            | None -> Expr.TypeApp (toLongIdent "Unchecked.defaultof", [x.Type])

        let binding = 
            SynBinding.Binding (None, SynBindingKind.NormalBinding, false, not x.IsConst, [], PreXmlDocEmpty, 
                SynValData (
                    None, SynValInfo ([], SynArgInfo ([], false, None )), None), 
                x.Name, None, 
                    toSynExpr init, range0 , NoDebugPointAtDoBinding)
        SynMemberDefn.LetBindings ([binding], x.IsStatic, false, range0)

    let toInterface (name, methods) =    
        let types = 
            methods |> List.map (fun (InterfaceMethod.InterfaceMethod (name, parameters)) -> 
                let parameters = parameters |> List.reduce (fun a b -> SynType.Fun (a, b, range0))
                let valsFn = 
                    ValSpfn ([], name, SynValTyparDecls ([], true, []),
                                parameters,
                                SynValInfo ([[SynArgInfo ([],false,None)]], SynArgInfo ([],false,None)),
                                false,false, PreXmlDocEmpty,
                                None,  None, range0)
                            
                SynMemberDefn.AbstractSlot (    
                    valsFn,
                    {IsInstance = true;
                          IsDispatchSlot = true;
                          IsOverrideOrExplicitImpl = false;
                          IsFinal = false;
                          MemberKind = MemberKind.Member;}, range0) )

        let info = ComponentInfo ([], [], [], [name], PreXmlDocEmpty, false, None, range0)
        let model = SynTypeDefnRepr.ObjectModel (TyconUnspecified,types,range0)
        TypeDefn (info, model, [], range0) 

    let toClass (cn:Class) = 
        let att = cn.Attributes |> List.map createSynAttribute
        
        let classTrivia =
            match cn.Trivia with
            | NoTrivia -> []
            | Above t -> [cn.Name.Name, t]

        let x =
            let typeVals = 
                cn.TypeParameters
                |> List.map (fun t -> TyparDecl ([], Typar (Ident (t, range0), NoStaticReq, false)) )
            
            ComponentInfo ([{ Attributes = att; Range = range0}], typeVals, [], toIdent cn.Name.Name, PreXmlDocEmpty, true, None, range0)

        
        let properties, getterSetterComments = cn.Properties |> List.map toProperty |> List.unzip |> fun (props, comments) -> List.concat props, comments
        let methodNames = cn.Methods |> List.map (fun x -> (if x.IsStatic then Some cn.Name.Name else None), x.Name.Replace ("this.", ""))
        let (methods, methodTrivia) = cn.Methods |> List.map (toMethod methodNames) |> List.unzip
        let fields = cn.Fields |> List.map toLet

        let interfaces = 

            cn.ImplementInterfaces 
            |> List.map (fun x -> 
                let (name, expr, interfaceParameters) = 
                    match cn.Methods |> List.filter (fun x -> not x.IsOverride && not x.IsStatic) with 
                    | [x] -> 
                        let args = x.Parameters |> List.map SynPat.getIdent |> Expr.Tuple
                        let invoke = sprintf "this.%s" x.Name |> toLongIdent
                        let app = ExprOps.toApp invoke args
                        x.Name, app, x.Parameters
                    | _ -> "Todo", Expr.Const SynConst.Unit, []

                let method = {
                    Method.Accessibility = None
                    Method.Body = expr
                    Method.Name = name
                    Method.IsAsync = false
                    Method.IsOverride = false
                    Method.IsVirtual = false
                    Method.IsPrivate = true
                    Method.IsStatic = false
                    Method.Parameters = interfaceParameters
                    Method.Attributes = []
                    Method.ReturnType = SynType.LongIdent (toLongIdentWithDots "unit")
                    Method.Trivia = Trivia.NoTrivia
                }

                // TODO: fix this for comments on interfaces
                SynMemberDefn.Interface (x,method |> toMethod [] |> fst |> List.singleton |> Some, range0))

        let mainCtor = 
            cn.Constructors 
            |> List.sortByDescending (fun x -> x.Parameters.Length)
            |> List.tryHead
            
        let ctors = 

            let ctor = 
                mainCtor
                |> Option.map (fun x -> 
                    SynMemberDefn.ImplicitCtor (None,[], SynSimplePats.SimplePats (x.Parameters, range0), None, PreXmlDocEmpty, range0) )
                    
                |> function 
                | Some c -> c
                | None -> SynMemberDefn.ImplicitCtor (None,[],SynSimplePats.SimplePats ([], range0), None, PreXmlDocEmpty, range0)


            cn.BaseClass |> Option.map (fun baseClass -> 

                let args = 
                    mainCtor
                    |> Option.map (fun x -> 
                        let items = x.SubclassArgs |> List.map (toSingleIdent >> SynExpr.Ident)
                        SynExpr.Paren (SynExpr.Tuple (false, items, [], range0), range0, None, range0) )
                        
                    |> function
                    | Some x -> x
                    | None -> SynExpr.Const (SynConst.Unit, range0)

                SynMemberDefn.ImplicitInherit 
                    (baseClass, args, None, range0)
                    
            ) |> function 
            | Some x -> [ctor; x]
            | None -> [ctor]

        let (ctors, fields, ctorInit, properties) = 
            match mainCtor with
            | None -> ctors, fields, [], properties
            | Some mainCtor ->  
                // let body = mainCtor.Body
                // printfn "%d" mainCtor.Body.Length
                // printfn "%d" mainCtor.Parameters.Length
                
                let (body, removedFields) = 
                    mainCtor.Body |> List.fold (fun (acc, removed) x ->
                        match x with 
                        | Expr.LongIdentSet (a,_) -> 
                            let name = joinLongIdentWithDots a |> (fun x -> x.Replace("this.", ""))
                            let matchedName = 
                                mainCtor.Parameters |> List.tryFind (fun x -> 
                                    let ident = SynSimplePat.getIdent x
                                    name.Contains ident.idText)

                            match matchedName with
                            | Some cName -> 
                                let ident = SynSimplePat.getIdent cName
                                if name = ident.idText then 
                                    (acc, Choice1Of2 (ident.idText) :: removed)    
                                else 
                                    (acc, Choice2Of2 (name, ident.idText) :: removed)                                    
                            | None -> (x :: acc, removed)                            
                        | _ -> x :: acc, removed
                    ) ([], []) 
                    |> (fun (body, removed) -> body |> List.rev, removed) // preserve the ordering of constructor lines

                let ctorPats = 
                    mainCtor.Parameters 
                    |> List.map (fun ident -> 
                        let cIdent = SynSimplePat.getIdent ident
                        let ctorField =
                            removedFields 
                            |> List.choose (function Choice2Of2 (name, cName) -> Some (name, cName) | Choice1Of2 _ -> None)
                            |> List.tryFind (fun (name, ctorName) -> ctorName = cIdent.idText)
                            |> Option.map fst
                        match ctorField with 
                        | Some newName ->                          
                            ident |> SynSimplePat.renameIdent (fun x -> Ident(newName, x.idRange))
                        | None -> ident                        
                    )

                let ctors = 
                    match ctors with 
                    | c::cs -> 
                        match c with 
                        | SynMemberDefn.ImplicitCtor (a,b,c,d,e,f) -> SynMemberDefn.ImplicitCtor (a,b, SynSimplePats.SimplePats (ctorPats, range0), d,e, f) :: cs
                        | _ -> cs
                    | [] -> []                    

                let fields = 

                    let names = 
                        removedFields 
                        |> List.map (function | Choice1Of2 x -> x | Choice2Of2 x -> x |> fst)

                    let isNameMatch fieldIdent = 
                        names |> List.exists (fun x -> x.Contains fieldIdent)

                    fields
                    |> List.filter (fun x -> 
                        match x with 
                        | SynMemberDefn.LetBindings (a,_,_,_) -> 
                            a |> List.exists (function 
                                | Binding (_,_,_,_,_,_,_,name,_,_,_,_) -> 
                                    match name with 
                                    | SynPat.LongIdent (name,_,_,_,_,_) -> joinLongIdentWithDots name |> isNameMatch |> not
                                    | _ -> true )
                        | _ -> true ) 

                let properties = 

                    let names = 
                        removedFields 
                        |> List.map (function | Choice1Of2 x -> x | Choice2Of2 x -> x |> fst)

                    let isNameMatch fieldIdent = 
                        names |> List.exists (fun x -> x.Contains fieldIdent)

                    properties
                    |> List.map (fun x -> 
                        match x with 
                        | SynMemberDefn.AutoProperty (a,b,name,d,e,f,g,h,i,j,k) when isNameMatch name.idText -> 
                                let getter = toLongIdent name.idText
                            
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
                                        (sprintf "this.%s" name.idText |> toLongIdentWithDots, toSingleIdent "get" |> Some, None, 
                                                Pats (
                                                    [SynPat.Paren (SynPat.Const (SynConst.Unit, range0), range0  )]),
                                                    None, range0)

                                SynMemberDefn.Member (
                                    SynBinding.Binding
                                        (None, NormalBinding, false, false, [], 
                                            PreXmlDocEmpty, SynValData (Some memberOptions, synVaInfo, None), headPat, None, getter |> toSynExpr, range0, NoDebugPointAtDoBinding), range0)   

                        | x -> x) 

                let expr = body |> sequential |> toSynExpr
                match expr with
                | SynExpr.Const (SynConst.Unit, _) -> ctors, fields, [], properties // `do ()` means nothings
                | _ -> 
                    let ctorInit = 
                        SynMemberDefn.LetBindings([
                            SynBinding.Binding (None, DoBinding, false, false, [], PreXmlDocEmpty, 
                                SynValData (None,SynValInfo ([],SynArgInfo ([],false,None)),None), SynPat.Const (SynConst.Unit, range0),
                                None, expr, range0, NoDebugPointAtDoBinding)
                            ], false, false, range0) |> List.singleton
                    ctors, fields, ctorInit, properties                      

        let trivia = (classTrivia @ List.concat getterSetterComments @ List.concat methodTrivia)
        SynTypeDefn.TypeDefn (x, SynTypeDefnRepr.ObjectModel (TyconUnspecified, ctors @ fields @ ctorInit @ properties @ methods @ interfaces, range0), [], range0)
        |> List.singleton
        |> (fun x -> SynModuleDecl.Types (x , range0), trivia)

    let toEnum (enum: Enum) =  
        let createEnumCase name synConst = 
            EnumCase(
                (* SynAttributes *) 
                SynAttributes.Empty,
                (* ident:Ident *) 
                toSingleIdent name,
                (* SynConst *) 
                synConst,
                (* PreXmlDoc *) 
                PreXmlDoc.Empty,
                (* range:range *) 
                range0
        )

        let enumCases = 
            enum.Members
            |> List.choose (fun (name, expr) -> 
                match toSynExpr expr with
                | SynExpr.Const(synConst, _) -> createEnumCase name synConst |> Some
                | _ -> None )

        let theEnum: SynTypeDefnSimpleRepr = 
            SynTypeDefnSimpleRepr.Enum(
                (* SynEnumCases *)
                enumCases,
                (* range *) 
                range0
        )

        let att = enum.Attributes |> List.map createSynAttribute

        let info = ComponentInfo ([{ Attributes = att; Range = range0}], [], [], (toIdent enum.Name), PreXmlDocEmpty, false, None, range0)
        let model = SynTypeDefnRepr.Simple (theEnum, range0)
        let typeDef = TypeDefn (info, model, [], range0) 
        SynModuleDecl.Types ([typeDef], range0)

    let parseStructure = function
        | C c -> toClass c
        | Interface (name, methods) -> (name,methods) |> toInterface |> (fun xs -> SynModuleDecl.Types ([xs], range0)), [] 
        | E e -> toEnum e, []

let toFsharpSyntaxTree input =

    let processStructures s = 
        s 
        |> TreeOps.reorderStructures
        |> List.map FormatOutput.parseStructure
        |> List.fold (fun (trees, comments) (tree, comment) ->
            tree :: trees, comments @ comment
            ) ([], [])
        |> (fun (xs,ys) -> List.rev xs, ys)

    let defaultNamespace = {Name = DefaultNames.namespaceName; Structures = []}
    let toNamespace ns mods decls = FormatOutput.toNamespace ns (mods @ decls)

    let processNamespaces usings attributes ns =
        let usingTrivia =
            usings |> List.choose (fun us ->
                match us.Trivia with
                | NoTrivia -> None
                | Above s -> Some (us.UsingNamespace, s)
                )
        
        let opens = usings |> List.map FormatOutput.createOpenStatements
        let attributes = FormatOutput.createAttributeStatements attributes
        ns |> List.map (fun ns ->
            ns.Structures |> processStructures |> (fun (tree, comment) -> toNamespace ns (opens @ attributes) tree, comment ) )
        |> List.unzip
        |> fun (tree, comments) -> tree |> FormatOutput.toFile, usingTrivia @ List.concat comments

    let usingsAttributesStructures usings namespaces attributes s = 
        let ns = 
            match namespaces, s with 
            | [], [] -> [defaultNamespace]
            | [], s -> [{defaultNamespace with Structures = s}]
            | ns, [] -> ns
            | ns, s -> {defaultNamespace with Structures = s } :: ns
        let ns = correctXamarinFormsPage usings ns
        processNamespaces usings attributes ns 
        
    match input with 
    | File f -> 
        match f with 
        | FileWithUsingNamespaceAttributeAndDefault (u, ns, a, s) ->
            usingsAttributesStructures u ns a s
           
    | UsingStatement us -> 
        let ns = {Name = DefaultNames.namespaceName; Structures = [] }
        let trivia =
            match us.Trivia with
            | NoTrivia ->[]
            | Above s -> [us.UsingNamespace, s]
                
        FormatOutput.toNamespace ns [FormatOutput.createOpenStatements us] |> List.singleton |> FormatOutput.toFile, trivia

    | FsharpSyntax.Namespace ns ->
        let decls, comments = ns.Structures |> processStructures
        FormatOutput.toNamespace ns decls |> List.singleton |> FormatOutput.toFile, comments

    | RootAttributes xs -> processNamespaces [] xs [defaultNamespace]
    | Structures s ->
        let tree, comments = s |> processStructures
        tree |> FormatOutput.defaultModule |> FormatOutput.toFile, comments
    //| Method m ->  m |> FormatOutput.toMethod [m.Name] |> FormatOutput.toDefaultClass |> List.singleton |> FormatOutput.defaultModule |> FormatOutput.toFile
    //| FSharper.Core.Field f ->  f |> Seq.head |> FormatOutput.inMethod |> FormatOutput.toMethod [] |> FormatOutput.toDefaultClass |> List.singleton |> FormatOutput.defaultModule |> FormatOutput.toFile

// One of th goals of this project is to support converting non-complete C# ie that from a blog post. 
// To compile the code, scaffolding must be added ie a namespace, class etc. 
// the output should match the input; this removes the scaffolding.
let removeDefaultScaffolding (fsharpOutput:string) = 
    let newLine = System.Environment.NewLine
    fsharpOutput
    |> (fun x -> x.Replace("namespace ``Program35949ae4-3f6e-11e9-b4dc-230deb73e77f``" + newLine, ""))
    |> (fun x -> x.Replace("type ``Klass067803f4-3f6e-11e9-b4df-6f8305ceb4a6``() =" + newLine, ""))
    |> (fun x -> x.Replace("module ``Program35949ae4-3f6e-11e9-b4dc-230deb73e77f``" + newLine + newLine, ""))
    |> (fun x -> x.Replace("\"Program35949ae4-3f6e-11e9-b4dc-230deb73e77f\"" + newLine, ""))
    |> (fun x -> x.Replace("``Program35949ae4-3f6e-11e9-b4dc-230deb73e77f``" + newLine, ""))
    |> (fun x -> x.Replace("    member this.Method156143763f6e11e984e11f16c4cfd728() =" + newLine, ""))
    |> (fun x -> x.Replace("member this.Method156143763f6e11e984e11f16c4cfd728() = ", ""))
    |> (fun x -> x.Replace("""\010""", "\\n"))
    |> (fun x -> x.Replace("""\009""", "\\t"))
    |> (fun x -> x.Replace("""\013""", "\\r"))

let createParsingOptionsFromFile fileName =
    { FSharpParsingOptions.Default with
          SourceFiles = [| fileName |] }
    
let sharedChecker = lazy (FSharpChecker.Create())


let isComment (line:string) =
    line.Trim().StartsWith "//"
    
let markLinesWithTrivia lines =
    lines
    |> List.fold (fun acc line ->
        match acc with
        | [] ->
            if isComment line then
                [true, line]
            else
                [false, line]
        | (_, prev)::rest ->
            if not (isComment line) && isComment prev then
                (true, line) :: acc
            elif isComment line then
                (true, line) :: acc
            else
                (false, line) :: acc                       
        ) []
    |> List.rev
    
let applyIndentationToComments (line:string) (comment:string) =
    let prefix = line.ToCharArray() |> Array.takeWhile (System.Char.IsWhiteSpace) |> System.String
    comment.Split('\n')
    |> Array.toList
    |> List.map (fun x ->
        let x = x.Replace("/*", "(*").Replace("*/", "*)").TrimEnd()
        prefix + x)
    
let insertTriviaIntoCode name (comment: string) linesOfCode =
    linesOfCode
    |> List.fold (fun (matched, acc) (hasComment, line) ->
       if not matched then
            printfn "Comment: %b  Line:%s" hasComment line
       if not matched && not hasComment && line.Contains name then           
           let comments = applyIndentationToComments line comment 
            // We need to reverse as we are building up the acc list in reverse order
           true, line :: (List.rev comments) @ acc
       else matched, line :: acc ) (false, [])
    |> snd
    |> List.rev
    

let rec applyTriviaToFormattedCode trivia (code: string) =
    match trivia |> List.distinct with
    | [] -> code
    | (name, comment:string)::trivias ->
        
        printfn "------ %s :: %s --------" name comment
        
        code.Split '\n'
        |> List.ofArray
        |> markLinesWithTrivia
        |> insertTriviaIntoCode name comment
        |> String.concat "\n"
        |> applyTriviaToFormattedCode trivias
            
let toFsharpString validateCode config (parseInput, trivia) = 
    if CodeFormatter.IsValidASTAsync parseInput |> Async.RunSynchronously then
        
//        printfn "----------- Output F# AST------------:\n%A\n----------------End AST----------------" parseInput

        let tree =
                CodeFormatter.FormatASTAsync(parseInput, DefaultNames.file, [], None, config)
                |> Async.RunSynchronously
        
//        printfn "----------- First Format------------:\n%s\n----------------EndTree----------------" tree
        
        let tree =
            // Formatting a second time may fail. E.g when only a method without an enclosing class.
            try
                let parseInput =
                    CodeFormatter.ParseAsync(
                        DefaultNames.file,
                        SourceOrigin.SourceString tree,
                        createParsingOptionsFromFile DefaultNames.file,
                        sharedChecker.Value
                    )
                    |> Async.RunSynchronously
                    |> Seq.head
                    |> fst
                
                CodeFormatter.FormatASTAsync(parseInput, DefaultNames.file, [], None, config) |> Async.RunSynchronously
            with
            | e ->
                printfn "--------Failed Second Format Parse--------------"
                printfn "%s" <| e.ToString()
                printfn "--------------End second Parse------------------"
                tree
                
        tree
        |> applyTriviaToFormattedCode trivia
        |> removeDefaultScaffolding
        |> (fun (x: string) ->
            
//            let x = x.Replace("\n\n\n", "\n\n")
            
            let rec reduceIndent (x:string) =
                if x.Replace("\n", "").StartsWith "    " |> not then x 
                else 
                    let trim (x:string) =
                        if x.StartsWith "    " then x.Substring 4
                        else 
                            if x.StartsWith "\n    " then "\n" + x.Substring 5 else x
                    
                    x.Split '\n' |> Array.map trim |> String.concat "\n" |> reduceIndent
            x |> reduceIndent
                
                )
    else  "Bad F# syntax tree"
    
let isLetterOrCloseParen (input: string) =
    let lastChar = input.TrimEnd().ToCharArray() |> Array.last
    let isLetterWithNoParen = System.Char.IsLetter lastChar && lastChar <> ')'
    
    if isLetterWithNoParen && input.Contains "new" then false else isLetterWithNoParen
    

let parseCsharp (input: string) =
    
    let specialChars = ["{"; "}"; ";"; "namespace"; "class"; "void"; "if"; "//";]
    let preProcessLines (x:string) =
        if x.TrimStart().Trim().StartsWith "..."then 
                "//" + x
//        elif specialChars |> List.forall (fun c -> x.Contains c |> not)
//             && x.StartsWith "   "
//                && x.Length >= 1
//                  && isLetterOrCloseParen x then
//            x.TrimEnd() + ";"
        else x 

    let input = 
        input.Split('\n')
        |> Array.map preProcessLines
        |> String.concat "\n"
        
    printfn "%s" input

    SyntaxFactory.ParseSyntaxTree (text = input, encoding = Encoding.UTF8)
    |> (fun x -> 
        let t = x.GetRoot()

        let hasValidNode = 
            t.ChildNodes() |> Seq.exists (function 
                | :? UsingDirectiveSyntax
                | :? NamespaceDeclarationSyntax
                | :? MethodDeclarationSyntax 
                | :? InterfaceDeclarationSyntax
                | :? ClassDeclarationSyntax 
                | :? EnumDeclarationSyntax -> true
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
                let error = x.GetDiagnostics() |> Seq.head |> (fun x -> 
                    sprintf "%s %s" (x.Descriptor.Title.ToString()) (x.Descriptor.Description.ToString()))
                printfn "Invalid or Incomplete C#: %s" error
                x.GetDiagnostics() |> Seq.iter (printfn "%A")
                None
    )
    
let config = {
    FormatConfig.Default with 
        FormatConfig.IndentSize = 4
        FormatConfig.MaxLineLength = 120
        FormatConfig.SemicolonAtEndOfLine = false
        FormatConfig.SpaceBeforeParameter = true
        FormatConfig.SpaceBeforeLowercaseInvocation = false
        FormatConfig.SpaceBeforeUppercaseInvocation = false
        FormatConfig.SpaceBeforeClassConstructor = false
        FormatConfig.SpaceBeforeMember = false
        FormatConfig.SpaceBeforeColon = false
        FormatConfig.SpaceAfterComma = true
        FormatConfig.SpaceBeforeSemicolon = false
        FormatConfig.SpaceAfterSemicolon = true
        FormatConfig.IndentOnTryWith = true
        FormatConfig.SpaceAroundDelimiter = true
        FormatConfig.MaxIfThenElseShortWidth = 40
        FormatConfig.MaxInfixOperatorExpression = 50
//        FormatConfig.MaxRecordWidth = 40
        FormatConfig.MaxRecordNumberOfItems = 2
        FormatConfig.RecordMultilineFormatter = MultilineFormatterType.NumberOfItems
//        FormatConfig.MaxArrayOrListWidth = 120
        FormatConfig.MaxArrayOrListNumberOfItems = 5
        FormatConfig.ArrayOrListMultilineFormatter = MultilineFormatterType.NumberOfItems
        FormatConfig.MaxValueBindingWidth = 120
        FormatConfig.MaxFunctionBindingWidth = 120
        FormatConfig.MaxDotGetExpressionWidth = 120
        FormatConfig.MultilineBlockBracketsOnSameColumn = true
        FormatConfig.NewlineBetweenTypeDefinitionAndMembers = false
        FormatConfig.KeepIfThenInSameLine = true
        FormatConfig.MaxElmishWidth = 120
        FormatConfig.SingleArgumentWebMode = false
        FormatConfig.AlignFunctionSignatureToIndentation = false
        FormatConfig.AlternativeLongMemberDefinitions = false
        FormatConfig.MultiLineLambdaClosingNewline = false
        FormatConfig.DisableElmishSyntax = true
        FormatConfig.EndOfLine = EndOfLineStyle.LF
    }    

let runWithConfig validateCode (input:string) = 

    let tree = parseCsharp input 
    let visitor = FSharperTreeBuilder()
    let nodes = tree |> Option.map (fun x -> x.GetRoot().ChildNodes())

    nodes
    |> Option.bind(fun x ->  x |> Seq.fold (visitor.ParseSyntax) None)
    |> Option.map toFsharpSyntaxTree
//    |> Option.map(fun x -> printfn "------------------------------First Parse--------------------:\n%A\n--------------------" x; x)
    |> Option.map (removeFsharpIn DefaultNames.file)
    |> Option.map (toFsharpString validateCode config) 
    |> function 
    | Some x -> x.Trim()
    | None -> "Invalid C# syntax tree"

let orderFiles files = 

    let getStructures = function
        | File f -> 
            match f with 
            | FileWithUsingNamespaceAttributeAndDefault (_, ns, _, s) -> 
                (ns |> List.collect (fun x -> x.Structures)) @ s

        | UsingStatement _ -> []
        | FsharpSyntax.Namespace ns -> ns.Structures
        | RootAttributes xs -> []
        | Structures s ->  s 

    let isEqual x y = 
        match x, y with 
        | C x, C y 
            when x.Name = y.Name && 
                x.Methods.Length = y.Methods.Length && 
                    x.Properties.Length = y.Properties.Length && 
                        x.Fields.Length = y.Fields.Length
            -> true
        | E x, E y when x.Name = y.Name &&  x.Members.Length = y.Members.Length -> true
        | Interface (x,y), Interface (x',y') when x.idText = x'.idText && y.Length = y'.Length -> true
        | Structure.RootAttributes _, Structure.RootAttributes _ -> true
        | _, _ -> false

    let visitor = FSharperTreeBuilder()
    let fsharpSyntax = 
        files 
        |> List.choose (fun file -> 
            parseCsharp file
            |> Option.map (fun x -> x.GetRoot().ChildNodes())
            |> Option.bind(fun x ->  
                x 
                |> Seq.fold (visitor.ParseSyntax) None 
                |> Option.map (fun x -> file, x)) )

    let fileStructures = 
        fsharpSyntax |> List.map (fun (file, x) -> file, getStructures x)

    fsharpSyntax
    |> List.fold (fun tree (file, fsharpSyntax) -> visitor.MergeFsharpSyntax (tree, fsharpSyntax) ) None
    |> Option.map getStructures
    |> Option.map reorderStructures
    |> Option.map (fun structures -> 
        structures 
        |> List.choose (fun s -> 
            fileStructures 
            |> List.tryFind (fun (file, xs) -> xs |> List.exists (fun x -> isEqual x s) )
            |> Option.map fst ) )
    |> Option.map List.distinct
    |> Option.defaultValue files // if there is nothing then the orignal order is fine


let run (input:string) = 
    runWithConfig true input