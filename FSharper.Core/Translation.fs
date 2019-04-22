namespace FSharper.Core

open Microsoft.CodeAnalysis.CSharp
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.SourceCodeServices.Structure
open Microsoft.FSharp.Compiler.SourceCodeServices 
open Fantomas

module Range = 
    let addLine file (range:range) = 

        let pos1 = mkPos 0 0
        let pos2 = mkPos (range.EndLine + 1) range.EndColumn

        mkRange file pos1 pos2

    let moveDownLine file (range:range) = 
        let pos1 = mkPos (range.StartLine + 1) range.StartColumn
        let pos2 = mkPos (range.EndLine + 1) range.EndColumn

        mkRange file pos1 pos2

module TreeOps = 
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


    let rec toSynPat p = 
        match p with 
        | Pat.LongIdent (a, b, c, d, e) -> SynPat.LongIdent (a, b, c, d, e, range0)
        | Pat.Wild -> SynPat.Wild range0
        | Pat.Named (a,b,c,d)-> SynPat.Named (toSynPat a,b,c,d, range0)

    let rec toBinding b = 
        let (LetBind (accessibility, kind, mustInline, isMutable, attrs, valData, headPat, expr)) = b
        SynBinding.Binding (accessibility, kind, mustInline, isMutable, attrs, PreXmlDocEmpty, valData, toSynPat headPat, None, toSynExpr expr,range0, SequencePointAtBinding range0)

    and toSynMatchExpr m = 
        match m with 
        | MatchClause.Clause (a,b,c) -> SynMatchClause.Clause (a, b |> Option.map toSynExpr,toSynExpr c,range0, SequencePointInfoForTarget.SequencePointAtTarget)
        
    and 
        toSynExpr (expr:Expr): SynExpr = 
        match expr with  
        | Expr.Paren x -> SynExpr.Paren (toSynExpr x, range0, None, range0)
        | Expr.Const x -> SynExpr.Const (x, range0)
        | Expr.Typed (expr, typeName) -> SynExpr.Typed (toSynExpr expr, typeName, range0)
        | Expr.Tuple xs -> SynExpr.Tuple (xs |> List.map toSynExpr, [range0], range0) 

        | Expr.New (isProtected, typeName, expr) -> SynExpr.New (isProtected, typeName, toSynExpr expr, range0)
        //| While of whileSeqPoint:SequencePointInfoForWhileLoop * whileExpr:Expr * doExpr:Expr
        //| For of forSeqPoint:SequencePointInfoForForLoop * ident:Ident * identBody:Expr * bool * toBody:Expr * doBody:Expr

        | Expr.ForEach (a,b,c,d,e,f) -> SynExpr.ForEach (a,b,c,toSynPat d,toSynExpr e, toSynExpr f,range0)

        | Expr.ArrayOrListOfSeqExpr (isArray, expr) -> SynExpr.ArrayOrListOfSeqExpr (isArray, toSynExpr expr, range0)
        | Expr.CompExpr (isArrayOrList, isNotNakedRefCell, expr) -> SynExpr.CompExpr (isArrayOrList, isNotNakedRefCell, toSynExpr expr, range0)

        | Expr.Lambda (a,b,c,d) -> SynExpr.Lambda (a,b,c, toSynExpr d, range0)  //of  fromMethod:bool * inLambdaSeq:bool * args:SynSimplePats * body:Expr

        //| Assert of expr:Expr
        | Expr.Match (a,b,c,d) -> SynExpr.Match (a,toSynExpr b,c |> List.map toSynMatchExpr, d, range0)

        | Expr.App (a,b,c,d) -> SynExpr.App (a,b, toSynExpr c, toSynExpr d, range0)
        | Expr.TypeApp (a,b) -> SynExpr.TypeApp (toSynExpr a, range0, b, [], None, range0, range0)

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
        | Expr.DotGet (e,a) -> SynExpr.DotGet (toSynExpr e, range0, a, range0)
        //| DotSet of Expr * longDotId:LongIdentWithDots * Expr
        | Expr.Set (left, right) -> SynExpr.Set (toSynExpr left, toSynExpr right, range0)
        //| DotIndexedGet of Expr * SynIndexerArg list

        //| DotIndexedSet of objectExpr:Expr * indexExprs:SynIndexerArg list * valueExpr:Expr
        //| NamedIndexedPropertySet of longDotId:LongIdentWithDots * Expr * Expr
        //| TypeTest of  expr:Expr * typeName:SynType
        //| Upcast of  expr:Expr * typeName:SynType 
        | Expr.Downcast (a,b) -> SynExpr.Downcast (toSynExpr a,b,range0)
        //| InferredUpcast of  expr:Expr 

        //| InferredDowncast of  expr:Expr 
        | Expr.Null -> SynExpr.Null range0
        //| AddressOf of  isByref:bool * Expr 
        //| TraitCall of SynTypar list * SynMemberSig * Expr 
        //| LetOrUseBang    of bindSeqPoint:SequencePointInfoForBinding * isUse:bool * isFromSource:bool * SynPat * Expr * Expr

        //| DoBang      of expr:Expr
        //| Fixed of expr:Expr

        //| Expr.ReturnFromIf e -> toSynExpr e //SynExpr.Const (SynConst.Unit, range0)

    let rec (|TypeNameContains|_|) text v = 
        match v with 
        | SynType.LongIdent s when (ParserUtil.joinLongIdentWithDots s).Contains text -> Some ()
        | SynType.App (a,b,c,d,e,f,g) -> 
            match a with
            | TypeNameContains text _ -> Some ()
            | _ -> None
        | _ -> None
        
    let rec containsExpr f tree = 
        let containsExpr = containsExpr f
        match tree with 
        | Expr.Sequential (s1,s2,s3,s4) -> 
            containsExpr s3 || containsExpr s4

        | Expr.Downcast (e,_) when f tree -> true
        | Expr.Downcast (e,_) -> containsExpr e
        | Expr.DotSet (a,b,c) -> containsExpr a || containsExpr c
        | Expr.DotGet (e, _) -> containsExpr e

        | Expr.IfThenElse (a,b,c,d,e) -> containsExpr a || containsExpr b

        | Expr.App (a,b,c,d) -> 
            containsExpr c || containsExpr d

        | Expr.ForEach (a,b,c,d,e,f) -> 
            containsExpr e || containsExpr f

        | Expr.Match (a,b,c,d) -> 
            c |> List.map (fun (MatchClause.Clause (a,b,c)) -> containsExpr c) |> List.reduce (||)

        | Expr.Lambda (a,b,c,d) -> containsExpr d

        | Expr.Paren e -> containsExpr e

        | x when f x -> true
        | _ -> false

    let containsCsharpIsMatch a = containsExpr (function | Expr.CsharpIsMatch _ -> true | _ -> false) a
    let isReturnFrom = containsExpr (function | Expr.ReturnFromIf _ -> true | _ -> false ) 

    let rec replaceExpr f tree = 
        let replaceExpr = replaceExpr f
        match f tree with 
        | Some x -> x
        | None -> 
            match tree with 
            | Expr.Sequential (s1,s2,s3,s4) -> Expr.Sequential (s1,s2, replaceExpr s3, replaceExpr s4)
            | Expr.Downcast (e,a) ->  Expr.Downcast (replaceExpr e,a)
            | Expr.DotSet (a,b,c) -> Expr.DotSet (replaceExpr a, b, replaceExpr c)
            | Expr.DotGet (e, a) -> Expr.DotGet (replaceExpr e, a)
            | Expr.IfThenElse (a,b,c,d,e) -> Expr.IfThenElse (replaceExpr a,replaceExpr b,c |> Option.map replaceExpr,d,e)
            | Expr.LetOrUse (x,y,z,i) -> Expr.LetOrUse (x,y,z, replaceExpr i)
            | Expr.App (a,b,c,d) -> Expr.App (a,b, replaceExpr c, replaceExpr d)
            | Expr.ForEach (a,b,c,d,e,f) -> Expr.ForEach (a,b,c,d, replaceExpr e, replaceExpr f)    
            | Expr.Match (a,b,c,d) -> Expr.Match (a, replaceExpr b,c,d)
            | Expr.Lambda (a,b,c,d) -> Expr.Lambda (a,b,c, replaceExpr d)
            | Expr.Paren e -> replaceExpr e |> Expr.Paren
            | e -> e

    let rec replaceSynExpr f tree = 
        let replaceSynExpr = replaceSynExpr f
        match f tree with 
        | Some x -> x
        | None -> 
            match tree with 
            | SynExpr.Sequential (s1,s2,s3,s4,r) -> SynExpr.Sequential (s1,s2, replaceSynExpr s3, replaceSynExpr s4, r)
            | SynExpr.Downcast (e,a, r) ->  SynExpr.Downcast (replaceSynExpr e,a,r)
            | SynExpr.DotSet (a,b,c,r) -> SynExpr.DotSet (replaceSynExpr a, b, replaceSynExpr c,r)
            | SynExpr.DotGet (e, a, b, c) -> SynExpr.DotGet (replaceSynExpr e, a,b,c)
            | SynExpr.IfThenElse (a,b,c,d,e,f,g) -> SynExpr.IfThenElse (replaceSynExpr a,replaceSynExpr b,c |> Option.map replaceSynExpr,d,e,f,g)
            | SynExpr.LetOrUse (x,y,z,i,j) -> SynExpr.LetOrUse (x,y,z, replaceSynExpr i,j)
            | SynExpr.App (a,b,c,d,e) -> SynExpr.App (a,b, replaceSynExpr c, replaceSynExpr d,e)
            | SynExpr.ForEach (a,b,c,d,e,f,g) -> SynExpr.ForEach (a,b,c,d, replaceSynExpr e, replaceSynExpr f,g)    
            | SynExpr.Match (a,b,c,d,f) -> SynExpr.Match (a, replaceSynExpr b,c,d,f)
            | SynExpr.Lambda (a,b,c,d,e) -> SynExpr.Lambda (a,b,c, replaceSynExpr d,e)
            | SynExpr.Paren (e,a,b,c) -> replaceSynExpr e |> (fun e -> SynExpr.Paren (e,a,b,c))
            | e -> e

    let rewriteReturnInIf tree = 
        let rec walker tree = 
            match tree with 
            | Expr.Sequential (s1,s2,s3,s4) -> 
                match s3 with 
                | Expr.IfThenElse (x,y,z,i,j) when Option.isNone z && isReturnFrom y -> 
                    Expr.IfThenElse (replaceExpr walker x, replaceExpr walker y, Some s4 |> Option.map (replaceExpr walker), i, j) |> Some

                | Expr.IfThenElse (x,y,z,i,j) when Option.map isReturnFrom z = Some true -> 
                    let first = Expr.Sequential (SequencePointInfoForSeq.SequencePointsAtSeq, true, replaceExpr walker y, replaceExpr walker s4)  
                    Expr.IfThenElse (replaceExpr walker x,first, z |> Option.map (replaceExpr walker), i, j) |> Some
                | _ -> None
            | Expr.ReturnFromIf e -> Some e
            | _ -> None
        replaceExpr walker tree

    let logicalNegateExpr = function 
        | Expr.App (_, _, (Expr.Ident x), leftWithOutNot) when x = "not" -> leftWithOutNot
        | Expr.Const (SynConst.Bool true) -> Expr.Const (SynConst.Bool false)
        | Expr.Const (SynConst.Bool false) -> Expr.Const (SynConst.Bool true)
        | Expr.App (isAtomic, isInfix, Expr.App (_, true, (Expr.Ident x), left), right) -> 
            let notOperatorApplied = 
                match x with 
                | "op_LessThanOrEqual" -> Some "op_GreaterThan"
                | "op_LessThan" -> Some "op_GreaterThanOrEqual"
                | "op_GreaterThan" -> Some "op_LessThanOrEqual"
                | "op_GreaterThanOrEqual" -> Some "op_LessThan"
                | "op_Equality" -> Some "op_Inequality"
                | "op_Inequality" -> Some "op_Equality"
                | _ -> None
                
            match notOperatorApplied with 
            | Some x -> Expr.App (isAtomic, isInfix, Expr.App (isAtomic, true, (Expr.Ident x), left), right)
            | None -> 
                let e = Expr.App (isAtomic, isInfix, Expr.App (isAtomic, true, (Expr.Ident x), left), right)
                Expr.App (ExprAtomicFlag.NonAtomic, false, (Expr.Ident "not"), e) |> Expr.Paren
        | e -> Expr.App (ExprAtomicFlag.NonAtomic, false, (Expr.Ident "not"), e) |> Expr.Paren

    let rec convertLogicalOrToLogicalAnd a = 
        match a with 
        | Expr.App (isAtomic, isInfix, Expr.App (_, true, (Expr.Ident "op_BooleanOr"), left), right) when containsCsharpIsMatch left  ->
            let left' = left |> convertLogicalOrToLogicalAnd |> logicalNegateExpr
            let right' = right |> convertLogicalOrToLogicalAnd |> logicalNegateExpr
            let e = Expr.App (isAtomic, isInfix, Expr.App (isAtomic, true, (Expr.Ident "op_BooleanAnd"), left'), right') |> Expr.Paren
            Expr.App(isAtomic, false, Expr.Ident "not", e)

        | Expr.App (isAtomic, isInfix, Expr.App (_, true, (Expr.Ident "op_BooleanOr"), left), right) ->
            let right' = right |> convertLogicalOrToLogicalAnd
            Expr.App (isAtomic, isInfix, Expr.App (isAtomic, true, (Expr.Ident "op_BooleanOr"), left), right')
            
        | Expr.App (isAtomic, isInfix, left, right) -> Expr.App (isAtomic, isInfix, convertLogicalOrToLogicalAnd left, convertLogicalOrToLogicalAnd right)
        | Expr.Paren e -> convertLogicalOrToLogicalAnd e
        | _ -> a
    
    let rec rewriteMatchIs tree = 

        let whenClauseJoinAnd x y = 
            match x,y with 
            | Some x, Some y -> 
                let e = Expr.App(ExprAtomicFlag.NonAtomic, true, Expr.Ident "op_BooleanAnd", x)
                (Expr.App(ExprAtomicFlag.NonAtomic, false, e, y)) |> Some
            | Some x, None -> Some x
            | None, Some x -> Some x
            | None, None -> None

        let rec removeMatchPlaceholder testExpr firstResult secondResult = 

            let logicalNotonMatchExpr (a,b,c,d) = 
                match c with 
                | x::y::[] -> 
                    let firstExpr = MatchClause.getExpr x
                    let secondExpr = MatchClause.getExpr y
                    let c = 
                        [ x |> MatchClause.mapExpr (fun _ -> secondExpr); y |> MatchClause.mapExpr (fun _ -> firstExpr) ]
                    Expr.Match (a,b,c,d)
                | _ -> Expr.Match (a,b,c,d) // possible from a merge of two match, the not causes have already been solved

            let logicalAndonMatchExpr (a,b,c,d) = 
                match c with 
                | x::y::[] -> 
                    let firstExpr = MatchClause.getExpr x
                    let secondExpr = MatchClause.getExpr y
                    let c = 
                        [ x |> MatchClause.mapExpr (fun _ -> secondExpr); y |> MatchClause.mapExpr (fun _ -> firstExpr) ]
                    Expr.Match (a,b,c,d)
                | x::y::xs -> // There are three, which means a logical OR, x is the 'else' clause
                    let firstExpr = MatchClause.getExpr y
                    let secondExpr = MatchClause.getExpr x
                    let c = 
                        (xs |> List.map (MatchClause.mapExpr (fun _ -> secondExpr))) @
                            [ x |> MatchClause.mapExpr (fun _ -> secondExpr); y |> MatchClause.mapExpr (fun _ -> firstExpr) ]
                    Expr.Match (a,b,c,d)
                | _ -> Expr.Match (a,b,c,d)


            let joinOr a b =
                match a, b with 
                | Expr.Match (a,b,c,d), Expr.Match (_,_,g,_) -> 
                    let c = 
                        match c,g with 
                        | x::y::[], x'::y'::[] -> [ x; x'; y ]
                        | xs, ys -> 
                            let joined = xs @ ys 
                            let notWild = joined |> List.filter (MatchClause.isWild >> not) |> List.rev
                            let firstWild = joined |> List.filter MatchClause.isWild |> List.head
                            firstWild :: notWild |> List.rev
                    Expr.Match (a,b,c,d)
                    
                | Expr.Match (a,b,c,d), e -> 
                    let c = 
                        match List.rev c with 
                        | x::MatchClause.Clause(h,None,first)::[] -> 
                            let second = MatchClause.getExpr x
                            [
                                MatchClause.Clause(h,Some e,second)
                                MatchClause.Clause(h,None,first)
                                MatchClause.wild second
                            ]
                        | x::y::xs -> 
                            let first = MatchClause.getExpr y
                            let second = MatchClause.getExpr x
                            let xs = 
                                (xs |> List.map (fun (MatchClause.Clause(h,i,_)) -> MatchClause.Clause(h,(Some e), second))) @
                                    xs |> List.map (MatchClause.mapExpr (fun _ -> second))
                                |> List.rev

                            let baseClauses = 
                                [
                                    y |> MatchClause.mapExpr (fun _ -> first)
                                    x |> MatchClause.mapExpr (fun _ -> second)
                                ]
                            xs @ baseClauses

                        | [e] -> [e]
                        | [] -> []

                    Expr.Match (a,b, c,d)

                | left, Expr.Match (_,_,clauses,_) -> 
                    let thenExpr = 
                        match clauses |> List.rev with 
                        | _::MatchClause.Clause(_, _, thenExpr)::_ -> thenExpr
                        | _ -> Expr.Const SynConst.Unit // syntacially possible, but should never happen when translating C# if statement

                    Expr.IfThenElse (left, thenExpr, Some b, SequencePointInfoForBinding.SequencePointAtBinding range0, false)

            let rec isEqualExpr a b = 
                match a,b with 
                | Expr.Ident a, Expr.Ident b when a = b -> true
                | Expr.LongIdent (_,a), Expr.LongIdent (_,b) when ParserUtil.joinLongIdentWithDots a = ParserUtil.joinLongIdentWithDots b -> true
                | _, _ -> false

            let rec joinAnd a b =
                match a, b with 
                | Expr.Match (a,b,c,d), Expr.Match (_, b',c',_) -> 
                    if (isEqualExpr b b') then 
                        let left = 
                            c |> MatchClause.matchClauses 
                                (fun h whenExpr first _ -> [MatchClause.Clause(h, whenExpr, first)]) 
                                (fun _ _ (_,MatchClause.Clause(h, whenExpr, first),_) -> [MatchClause.Clause(h, whenExpr, first)]) 
                            |> List.head

                        let right = 
                            c' |> MatchClause.matchClauses 
                                (fun h whenExpr first second -> [MatchClause.Clause(h, whenExpr, first); ]) 
                                (fun second first (_,MatchClause.Clause(h, _, first),_) -> [MatchClause.Clause(h, None, first)]) 
                            |> List.head

                        let wild = 
                            c 
                            |> MatchClause.matchClauses (fun _ _ _ second -> [MatchClause.wild second]) (fun _ second _ -> [MatchClause.wild second]) 
                            |> List.head

                        let clauses = 
                            [   
                                left
                                right
                                wild
                            ]

                        Expr.Match(a,b,clauses,d)

                    else 

                        let (MatchClause.Clause(leftMatch, leftWhenExpr, first)) = 
                            c |> MatchClause.matchClauses 
                                (fun h whenExpr first _ -> [MatchClause.Clause(h, whenExpr, first)]) 
                                (fun _ _ (_,MatchClause.Clause(h, whenExpr, first),_) -> [MatchClause.Clause(h, whenExpr, first)]) 
                            |> List.head

                        let (MatchClause.Clause(rightMatch, rightWhenExpr, _)) = 
                            c' |> MatchClause.matchClauses 
                                (fun h whenExpr first second -> [MatchClause.Clause(h, whenExpr, first); ]) 
                                (fun second first (_,MatchClause.Clause(h, _, first),_) -> [MatchClause.Clause(h, None, first)]) 
                            |> List.head

                        let wild = 
                            c 
                            |> MatchClause.matchClauses (fun _ _ _ second -> [MatchClause.wild second]) (fun _ second _ -> [MatchClause.wild second]) 
                            |> List.head

                        let b = Expr.Tuple [b;b']

                        let clauses = 
                            [
                                MatchClause.Clause (SynPat.Tuple ([leftMatch; rightMatch], range0), whenClauseJoinAnd leftWhenExpr rightWhenExpr, first)
                                wild 
                            ]

                        Expr.Match(a,b,clauses,d)
                    
                | Expr.Match (a,b,c,d), e -> 
                    let c = 
                        match List.rev c with 
                        | x::(MatchClause.Clause(h,k,first))::[] -> 
                            [
                                MatchClause.Clause(h,(whenClauseJoinAnd k (Some e)),first); x
                            ]
                        | x::xs -> 
                            let xs = 
                                xs 
                                |> List.map (fun (MatchClause.Clause(h,i,first)) -> MatchClause.Clause(h,whenClauseJoinAnd i (Some e), first))
                            [x] @ xs
                        | [] -> []

                    Expr.Match (a,b, c,d)
                | Expr.App(isAtomic, isInfix, Expr.Ident "not", left), Expr.Match (_,_, clauses,_) -> // not e1 && e2 === e1 || e2
                    let thenExpr = 
                        match clauses |> List.rev with 
                        | _::MatchClause.Clause(_, _, thenExpr)::_ -> thenExpr
                        | _ -> Expr.Const SynConst.Unit // syntacially possible, but should never happen when translating C# if statement

                    Expr.IfThenElse (left, thenExpr, Some b, SequencePointInfoForBinding.SequencePointAtBinding range0, false)

                | e, Expr.Match (_,_, clauses,_) -> joinAnd b a // Swap around and will be sorted via match statement

                | a, b -> 
                    let a = Expr.App (ExprAtomicFlag.NonAtomic, true, Expr.Ident "op_BooleanAnd", a)
                    Expr.App (ExprAtomicFlag.NonAtomic, true, a, b) |> Expr.Paren
                    
            match testExpr  with 
            | Expr.CsharpIsMatch (expr, left) -> 
                let clauses = [ MatchClause.Clause(left,None,firstResult); MatchClause.wild secondResult ]
                Expr.Match (SequencePointInfoForBinding.SequencePointAtBinding range0, expr, clauses, false)

            | Expr.App (atomic,isInfix,left,right) -> 

                let left' = removeMatchPlaceholder left firstResult secondResult
                let right' = removeMatchPlaceholder right firstResult secondResult

                match left', right' with 
                | Expr.Ident "not", Expr.Match (a,b,c,d) -> logicalNotonMatchExpr (a,b,c,d)
                
                | Expr.App (e,f,(Expr.Ident "op_BooleanOr"),h), a -> joinOr h a
                | Expr.App (e,f,(Expr.Ident "op_BooleanAnd"),h), a -> joinAnd h a
                | _, _ -> Expr.App(atomic, isInfix, left', right')

            | Expr.Paren e -> removeMatchPlaceholder e firstResult secondResult
            | e -> e

        let rec walker tree = 
            match tree with 
            | Expr.IfThenElse (a,b,c,d,e) -> 

                if containsCsharpIsMatch a then 
                    let cClause = (match c with | Some x -> x | None -> Expr.Const SynConst.Unit)
                    removeMatchPlaceholder (convertLogicalOrToLogicalAnd a) b cClause |> Some
                else None
            | _ -> None
        replaceExpr walker tree

    let wrapNewKeyword tree = 
        let rec wrapNewInParen = 
            replaceExpr (fun tree ->
                match tree with 
                | Expr.New (a,b,c) -> Expr.Paren (Expr.New (a,b,c)) |> Some
                | _ -> None)

        let rec walker tree = 
            match tree with
            | Expr.DotGet (a,b) -> Expr.DotGet (wrapNewInParen a,b) |> Some
            | _ -> None
        replaceExpr walker tree

    let replaceDotGetIfNotInLetBinding tree = 

        let rec walker tree = 
            match tree with
            | Expr.DotGet (a,b) -> 
                match a with 
                | Expr.LongIdent (c, prefixName) -> 
                    let b = joinLongIdentWithDots b
                    let prefixName = joinLongIdentWithDots prefixName
                    sprintf "%s.%s" prefixName b |> toLongIdent |> Some
                | _ -> None

            | Expr.Sequential (s1,s2,s3,s4) -> 
                match s3 with 
                | Expr.IfThenElse (x,y,z,i,j) when Option.isNone z ->
                    Expr.IfThenElse (x,y, Some s4, i, j) |> Some
                | _ -> None
            | _ -> None
        replaceExpr walker tree

    let rewriteInLetExp tree =

        let rec walker tree = 
            match tree with 
            | Expr.LetOrUse (x,y,z,Expr.InLetPlaceholder) ->  
                Expr.LetOrUse (x,y,z, Expr.Const SynConst.Unit) |> Some
            | Expr.Sequential (s1,s2,s3,s4) -> 
                match s3 with  
                | Expr.LetOrUse (x,y,z,Expr.InLetPlaceholder) -> 
                    let e = replaceExpr walker s4
                    Expr.LetOrUse (x,y,z,e) |> Some
                | _ -> None
            | _ -> None
        replaceExpr walker tree

    let rec rewriteMethodWithPrefix methodNames tree = 
        let recurse = rewriteMethodWithPrefix methodNames

        let isLetPlaceholder exp = 
            match exp with 
            | Expr.InLetPlaceholder -> true
            | _ -> false

        match tree with 
        | Expr.LongIdent (a, b) -> 
            if methodNames |> Seq.exists (fun name -> b.ToString().Contains name ) then 

                let idents = b.Lid |> List.map (fun x -> x.idText) |> String.concat "."
                Expr.LongIdent (a, idents |> sprintf "this.%s" |> toLongIdentWithDots )
            else Expr.LongIdent (a,b)
            
        // root of tree should not be contain immeidate child of InLetPlaceholder (has no meaning)
        | Expr.LetOrUse (x,y,z,i) ->  
            Expr.LetOrUse (x,y,z,recurse i)

        | Expr.Sequential (s1,s2,s3,s4) -> Expr.Sequential (s1,s2,recurse s3,recurse s4)
        | Expr.App (a,b,c,d) -> Expr.App (a,b, recurse c, recurse d)
        | Expr.IfThenElse (a,b,c,d,e) -> Expr.IfThenElse (a,recurse b,c |> Option.map recurse,d,e)

        | Expr.ForEach (a,b,c,d,e,f) -> 
            let e2 = recurse e
            let f2 = recurse f

            Expr.ForEach (a,b,c,d, e2, f2)

        | _ -> tree

    let simplifyTree = 
        replaceExpr (fun tree -> 
            match tree with 
            | Expr.Paren (Expr.Tuple []) -> Expr.Const SynConst.Unit |> Some
            | Expr.Tuple [] -> Expr.Const SynConst.Unit |> Some
            | _ -> None)

    let rewriteActionOrFuncToUseCallInvoke tree = 

        let isDowncastToActionOrFunc p = 
            match p with
            | TypeNameContains "Action" _ -> true
            | TypeNameContains "Func" _ -> true
            | _ -> false

        let containsActionOrFunc =  
            containsExpr (function  | Expr.Downcast (_, p) -> isDowncastToActionOrFunc p | _ -> false )

        let walker tree = 
            match tree with 
            | Expr.App (a,b,c,_) when containsActionOrFunc c -> 
                replaceExpr 
                    (fun tree -> 
                        match tree with 
                        | Expr.App (a,b,Expr.Paren(Expr.Downcast(e, p)),c) when isDowncastToActionOrFunc p -> 
                            Expr.App (a,b,Expr.DotGet(Expr.Paren(Expr.Downcast(e, p)), toLongIdentWithDots "Invoke"),c) |> Some
                        | _ -> None) tree |> Some
            | _ -> None
            
        replaceExpr walker tree

    let rec addNewLineToLet file tree = 
        let moveDown = Range.moveDownLine file
        let moveAndAdd r1 = r1 |> Range.moveDownLine file |> Range.addLine file

        let rec loop tree = 
            match tree with 
            | SynExpr.Paren (x, r1, p, r) -> SynExpr.Paren (x, moveDown r1, p, moveDown r)
            | SynExpr.Const (x, r1) -> SynExpr.Const (x, moveDown r1)
            | SynExpr.Typed (expr, typeName, r1) -> SynExpr.Typed (expr, typeName, moveDown r1)
            | SynExpr.Tuple (xs, rs, r)  -> SynExpr.Tuple (xs, rs, moveDown r)
            | SynExpr.App (a,b, c, d, r) -> SynExpr.App (a,b, loop c, loop d, moveDown r)
            | SynExpr.LetOrUse (a,b,c, d, r1) -> SynExpr.LetOrUse (a,b,c, d, moveDown r1)
            | SynExpr.Ident (r) -> SynExpr.Ident (Ident(r.idText, moveDown r.idRange))
            | SynExpr.LongIdent (a,b, c, r) -> SynExpr.LongIdent (a,b, c, moveDown r)
            | SynExpr.Set (left, right, r) -> SynExpr.Set (left, right, moveDown r)
            | SynExpr.Null r -> SynExpr.Null <| moveDown r
            | SynExpr.LongIdentSet (a,b,c) -> SynExpr.LongIdentSet (a,b, moveDown c)
            | SynExpr.Sequential (a,b,c,d, r1) -> 
                SynExpr.Sequential (a,b, loop  c, loop d,  moveAndAdd r1 )
            | SynExpr.IfThenElse (a, b, c,d, e, r1, r2) -> 

                SynExpr.IfThenElse (loop a, loop b, c |> Option.map (loop ), d,e, moveAndAdd r1, moveAndAdd r2)
            | SynExpr.ForEach (a,b,c,d,e,f,g) ->  SynExpr.ForEach (a,b,c,d,e,f,moveAndAdd g)
            | SynExpr.TypeApp (a,b,c,d,e,f,g) -> SynExpr.TypeApp (loop a, moveDown b,c,d,e, moveDown f, moveDown g)
            | SynExpr.DotGet (a,b,c, d) -> SynExpr.DotGet (a, moveDown b, c, moveDown d) 
            | SynExpr.FromParseError (a,b) -> SynExpr.FromParseError (loop a, moveAndAdd b)
            | SynExpr.Match (a,b,c,d,e) -> SynExpr.Match (a,b,c,d,e)

        let walker tree = 
            match tree with 
            | SynExpr.ForEach (a,b,c,d,e,f,g) -> 
                SynExpr.ForEach (a,b,c,d, loop e |> addNewLineToLet file, loop f |> addNewLineToLet file, g) |> Some
            | SynExpr.LetOrUse (a,b,c,d,e) -> 
                let d = loop d |> addNewLineToLet file 
                SynExpr.LetOrUse (a,b,c, d, e) |> Some
            | _ -> None

        replaceSynExpr walker tree

    // For some reason fantomas does not format let bindings correclty and wants to use the let .... in statements. 
    // This is done becuase there are not line endings. 
    // To correclty the problem, walk the tree and bump the line number, via the range, to add a new line. 
    // Fantomas will then print the let binding with out the in keyword, and instead, move to the next line. 
    let removeFhsarpIn file tree =

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
                                                            let expr = expr |> addNewLineToLet file

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
                                                    let expr = expr |> addNewLineToLet file

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