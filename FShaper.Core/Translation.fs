namespace FSharper.Core

open FSharp.Compiler.SyntaxTree
open FSharp.Compiler.Range
open FSharp.Compiler.SourceCodeServices 
open FSharp.Compiler.XmlDoc
open Fantomas

module List = 

    let permuteWithOrder a = 
        let size = List.length a
        let rec loop acc int = 
            if int < 0 then acc 
            else 
                let result = 
                    [0 .. (size - 1)] 
                    |> List.map (fun x -> (x, x + int))
                    |> List.filter (fun (start, e) -> e < size)
                    |> List.map (fun (s,e) -> a.[s .. e])
                loop (result @ acc) (int - 1)  
        loop [] size   

    let tailSafe = function | [] -> [] | _::xs -> xs

    let rec zipAnyLength lists = 
        let rec loop acc lists = 
            match lists |> List.filter (List.isEmpty >> not) with 
            | [] -> acc
            | lists -> 
                let xs = 
                    lists 
                    |> List.map List.tryHead 
                    |> List.choose id 
                    |> List.distinct
                loop 
                    (xs :: acc) // don't distinct here as the order is incorrect
                    (lists |> List.map tailSafe)
        loop [] lists 
        |> List.rev |> List.concat // preserve the order 

// Directed acyclic graph
module DAG = 
    let successors n edges =
        edges |> List.filter (fun (s,_) -> s = n) |> List.map snd 

    let topologicalSort edges seed =
        let rec sort path visited = function
            | [] -> visited
            | n::nodes ->
                printfn "%A %A %A" n path visited
                if List.exists (fun x -> x = n) path then 
                    [] 
                else
                    let v' = 
                        if List.exists (fun x -> x = n) visited then 
                            visited 
                        else
                            n :: sort (n::path) visited (successors n edges)
                    sort path v' nodes

        sort [] [] [seed]


[<AutoOpen>]
type CompExpr = Seq | Async 

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
        | Pat.LongIdent (a, b, c, d) -> SynPat.LongIdent (a, None, b, c, d, range0)
        | Pat.Wild -> SynPat.Wild range0
        | Pat.Named (a,b,c,d)-> SynPat.Named (toSynPat a,b,c,d, range0)

    let rec toBinding b = 
        let (LetBind (accessibility, kind, mustInline, isMutable, attrs, valData, headPat, expr)) = b
        SynBinding.Binding (accessibility, kind, mustInline, isMutable, attrs, PreXmlDocEmpty, valData, toSynPat headPat, None, toSynExpr expr,range0, NoDebugPointAtDoBinding)

    and toSynMatchExpr m = 
        match m with 
        | MatchClause.Clause (a,b,c) -> SynMatchClause.Clause (a, b |> Option.map toSynExpr,toSynExpr c,range0, DebugPointForTarget.No)

    and toSynIndexerArg m = 
        match m with 
        | One e -> SynIndexerArg.One ((e |> toSynExpr), false, range0) // TODO: review 'false' here
        
    and 
        toSynExpr (expr:Expr): SynExpr = 
        match expr with  
        | Expr.Paren x -> SynExpr.Paren (toSynExpr x, range0, None, range0)
        | Expr.Const x -> SynExpr.Const (x, range0)
        | Expr.Typed (expr, typeName) -> SynExpr.Typed (toSynExpr expr, typeName, range0)
        | Expr.Tuple xs -> SynExpr.Tuple (false, xs |> List.map toSynExpr, [range0], range0) // TODO review 'true' for isStrict

        | Expr.New (isProtected, typeName, expr) -> SynExpr.New (isProtected, typeName, toSynExpr expr, range0)
        | Expr.While (b,c) -> SynExpr.While  (DebugPointAtWhile.No, toSynExpr b, toSynExpr c, range0)
        | Expr.For (b,c,d,e,f ) -> SynExpr.For (DebugPointAtFor.No, b,toSynExpr c,d, toSynExpr e, toSynExpr f, range0)

        | Expr.ForEach (b,c,d,e,f) -> SynExpr.ForEach (DebugPointAtFor.No, b,c,toSynPat d,toSynExpr e, toSynExpr f,range0)

        | Expr.ArrayOrListOfSeqExpr (isArray, expr) -> SynExpr.ArrayOrListOfSeqExpr (isArray, toSynExpr expr, range0)
        | Expr.CompExpr (isArrayOrList, isNotNakedRefCell, expr) -> SynExpr.CompExpr (isArrayOrList, isNotNakedRefCell, toSynExpr expr, range0)

        | Expr.Lambda (a,b,c,d, e) -> SynExpr.Lambda (a,b,c, toSynExpr d, e, range0)  //of  fromMethod:bool * inLambdaSeq:bool * args:SynSimplePats * body:Expr

        //| Assert of expr:Expr
        | Expr.Match (b,c) -> SynExpr.Match (DebugPointForBinding.NoDebugPointAtDoBinding, toSynExpr b,c |> List.map toSynMatchExpr, range0)

        | Expr.App (a,b,c,d) -> SynExpr.App (a,b, toSynExpr c, toSynExpr d, range0)
        | Expr.TypeApp (a,b) -> SynExpr.TypeApp (toSynExpr a, range0, b, [], None, range0, range0)

        | Expr.LetOrUse (a,b,c,d) -> // of isRecursive:bool * isUse:bool * bindings:SynBinding list * body:Expr

            SynExpr.LetOrUse (a,b,c |> List.map toBinding, toSynExpr d, range0)

        | Expr.LetOrUseBang (b,c,d,e,f) -> SynExpr.LetOrUseBang (NoDebugPointAtDoBinding,b,c,d, toSynExpr e, [], toSynExpr f, range0)

        | Expr.TryWith (a,b) -> SynExpr.TryWith (toSynExpr a,range0, b |> List.map toSynMatchExpr, range0, range0, DebugPointAtTry.No, DebugPointAtWith.No)

        //| TryFinally of tryExpr:Expr * finallyExpr:Expr * trySeqPoint:SequencePointInfoForTry * finallySeqPoint:SequencePointInfoForFinally

        //| Lazy of Expr

        | Expr.Sequential (b,c,d) -> SynExpr.Sequential (DebugPointAtSequential.ExprOnly,b,toSynExpr c,toSynExpr d, range0)

        | Expr.IfThenElse (a,b,c,d) -> SynExpr.IfThenElse (toSynExpr a, toSynExpr b,c |> Option.map toSynExpr, NoDebugPointAtDoBinding ,d, range0, range0) 

        | Expr.Ident s -> SynExpr.Ident (Ident(s, range0))
        | Expr.LongIdent (a,b) -> SynExpr.LongIdent (a,b, None, range0)

        | Expr.LongIdentSet (id, e) -> SynExpr.LongIdentSet (id, toSynExpr e, range0) //of longDotId:LongIdentWithDots * expr:Expr
        | Expr.DotGet (e,a) -> SynExpr.DotGet (toSynExpr e, range0, a, range0)
        //| DotSet of Expr * longDotId:LongIdentWithDots * Expr
        | Expr.Set (left, right) -> SynExpr.Set (toSynExpr left, toSynExpr right, range0)
        | Expr.DotIndexedGet (a,b) -> SynExpr.DotIndexedGet (toSynExpr a,b |> List.map toSynIndexerArg, range0,range0) //  of Expr * SynIndexerArg list

        | Expr.DotIndexedSet (a,b,c) -> SynExpr.DotIndexedSet (toSynExpr a, b |> List.map toSynIndexerArg , toSynExpr c, range0, range0, range0)
        //| NamedIndexedPropertySet of longDotId:LongIdentWithDots * Expr * Expr
        //| TypeTest of  expr:Expr * typeName:SynType
        //| Upcast of  expr:Expr * typeName:SynType 
        | Expr.Downcast (a,b) -> SynExpr.Downcast (toSynExpr a,b,range0)
        //| InferredUpcast of  expr:Expr 

        //| InferredDowncast of  expr:Expr 
        | Expr.Null -> SynExpr.Null range0

        | Expr.YieldOrReturn (a,b) -> SynExpr.YieldOrReturn (a, toSynExpr b, range0)
        //| AddressOf of  isByref:bool * Expr 
        //| TraitCall of SynTypar list * SynMemberSig * Expr 
        //| LetOrUseBang    of bindSeqPoint:SequencePointInfoForBinding * isUse:bool * isFromSource:bool * SynPat * Expr * Expr

        | Expr.DoBang e -> SynExpr.DoBang (toSynExpr e, range0)
        //| Fixed of expr:Expr

        | Expr.TypeTest (a,b) -> SynExpr.TypeTest (toSynExpr a,b,range0)
        //| Expr.ReturnFromIf e -> toSynExpr e //SynExpr.Const (SynConst.Unit, range0)

    let rec (|TypeNameContains|_|) text v = 
        match v with 
        | SynType.LongIdent s when (joinLongIdentWithDots s).Contains text -> Some ()
        | SynType.App (a,b,c,d,e,f,g) -> 
            match a with
            | TypeNameContains text _ -> Some ()
            | _ -> None
        | _ -> None
        
    let rec containsExpr f tree = 
       ParserUtil.containsExpr f tree |> List.isEmpty |> not

    let containsCsharpIsMatch a = containsExpr (function | Expr.CsharpIsMatch _ -> true | _ -> false) a
    let isReturnFrom = containsExpr (function | Expr.ReturnFromIf _ -> true | _ -> false ) 


    let rec replaceSynExpr f tree = 
        let replaceSynExpr = replaceSynExpr f
        match f tree with 
        | Some x -> x
        | None -> 
            match tree with 
            | SynExpr.App (a,b,c,d,e) -> SynExpr.App (a,b, replaceSynExpr c, replaceSynExpr d,e)
            | SynExpr.CompExpr (a,b,c,d) -> SynExpr.CompExpr (a,b, replaceSynExpr c,d)
            | SynExpr.Downcast (e,a, r) ->  SynExpr.Downcast (replaceSynExpr e,a,r)
            | SynExpr.DotSet (a,b,c,r) -> SynExpr.DotSet (replaceSynExpr a, b, replaceSynExpr c,r)
            | SynExpr.DotGet (e, a, b, c) -> SynExpr.DotGet (replaceSynExpr e, a,b,c)
            | SynExpr.ForEach (a,b,c,d,e,f,g) -> SynExpr.ForEach (a,b,c,d, replaceSynExpr e, replaceSynExpr f,g)    
            | SynExpr.IfThenElse (a,b,c,d,e,f,g) -> SynExpr.IfThenElse (replaceSynExpr a,replaceSynExpr b,c |> Option.map replaceSynExpr,d,e,f,g)
            | SynExpr.Lambda (a,b,c,d,e,f) -> SynExpr.Lambda (a,b,c, replaceSynExpr d,e, f)
            | SynExpr.LetOrUse (x,y,z,i,j) -> SynExpr.LetOrUse (x,y,z, replaceSynExpr i,j)
            | SynExpr.LetOrUseBang (x,y,z,i,j,k,l,m) -> SynExpr.LetOrUseBang (x,y,z, i, replaceSynExpr j, k, replaceSynExpr l, m)
            | SynExpr.Match (a,b,c,d) -> SynExpr.Match (a, replaceSynExpr b,c,d)
            | SynExpr.Paren (e,a,b,c) -> replaceSynExpr e |> (fun e -> SynExpr.Paren (e,a,b,c))
            | SynExpr.Sequential (s1,s2,s3,s4,r) -> SynExpr.Sequential (s1,s2, replaceSynExpr s3, replaceSynExpr s4, r)
            | SynExpr.TryWith (a,b,c,d,e,f,g) -> SynExpr.TryWith ( replaceSynExpr a,b,c,d,e,f,g)
            | SynExpr.While (a,b,c,d) -> SynExpr.While (a, replaceSynExpr b, replaceSynExpr c,d)
            | SynExpr.YieldOrReturn (a, b, c) -> SynExpr.YieldOrReturn (a, replaceSynExpr b, c)
            | SynExpr.Typed (a,b,c) -> SynExpr.Typed (replaceSynExpr a,b,c)
            | SynExpr.TypeApp (a,b,c,d,e,f,g) -> SynExpr.TypeApp (replaceSynExpr a,b,c,d,e,f,g)
            | e -> e

    let rewriteReturnInIf tree = 
        let rec walker tree = 
            match tree with 
            | Expr.Sequential (s2,s3,s4) -> 
                match s3 with 
                | Expr.IfThenElse (x,y,z,i) when Option.isNone z && isReturnFrom y -> 
                    Expr.IfThenElse (replaceExpr walker x, replaceExpr walker y, Some s4 |> Option.map (replaceExpr walker), i) |> Some

                | Expr.IfThenElse (x,y,z,i) when Option.map isReturnFrom z = Some true -> 
                    let first = Expr.Sequential (true, replaceExpr walker y, replaceExpr walker s4)  
                    Expr.IfThenElse (replaceExpr walker x,first, z |> Option.map (replaceExpr walker), i) |> Some
                    
                | Expr.IfThenElse (x,y,z,i) when Option.isSome z && isReturnFrom y ->
                    let z =
                        z
                        |> Option.map (replaceExpr walker)
                        |> Option.map (fun x -> Expr.Sequential (true, x, replaceExpr walker s4))
                       
                    Expr.IfThenElse (replaceExpr walker x, replaceExpr walker y , z, i) |> Some
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
            ExprOps.toApp (Expr.Ident "not") (ExprOps.toInfixApp left' (Expr.Ident "op_BooleanAnd") right' |> Expr.Paren)

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

            let logicalMatchExpr (b,c) = 
                match c with 
                | x::[ y ] -> 
                    let firstExpr = MatchClause.getExpr x
                    let secondExpr = MatchClause.getExpr y
                    let c = 
                        [ x |> MatchClause.mapExpr (fun _ -> secondExpr); y |> MatchClause.mapExpr (fun _ -> firstExpr) ]
                    Expr.Match (b,c)
                | _ -> Expr.Match (b,c) // possible from a merge of two match, the not causes have already been solved

            let logicalAndonMatchExpr (a,b,c,d) = 
                match c with 
                | x::[ y ] -> 
                    let firstExpr = MatchClause.getExpr x
                    let secondExpr = MatchClause.getExpr y
                    let c = 
                        [ x |> MatchClause.mapExpr (fun _ -> secondExpr); y |> MatchClause.mapExpr (fun _ -> firstExpr) ]
                    Expr.Match (b,c)
                | x::y::xs -> // There are three, which means a logical OR, x is the 'else' clause
                    let firstExpr = MatchClause.getExpr y
                    let secondExpr = MatchClause.getExpr x
                    let c = 
                        (xs |> List.map (MatchClause.mapExpr (fun _ -> secondExpr))) @
                            [ x |> MatchClause.mapExpr (fun _ -> secondExpr); y |> MatchClause.mapExpr (fun _ -> firstExpr) ]
                    Expr.Match (b,c)
                | _ -> Expr.Match (b,c)


            let joinOr a b =
                match a, b with 
                | Expr.Match (b,c), Expr.Match (_,g) -> 
                    let c = 
                        match c,g with 
                        | x::[ y ], x'::[ y' ] -> [ x; x'; y ]
                        | xs, ys -> 
                            let joined = xs @ ys 
                            let notWild = joined |> List.filter (MatchClause.isWild >> not) |> List.rev
                            let firstWild = joined |> List.filter MatchClause.isWild |> List.head
                            firstWild :: notWild |> List.rev
                    Expr.Match (b,c)
                    
                | Expr.Match (b,c), e -> 
                    let c = 
                        match List.rev c with 
                        | x::[ MatchClause.Clause(h,None,first) ] -> 
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
                    Expr.Match (b, c)

                | left, Expr.Match (_,clauses) -> 
                    let thenExpr = 
                        match clauses |> List.rev with 
                        | _::MatchClause.Clause(_, _, thenExpr)::_ -> thenExpr
                        | _ -> Expr.Const SynConst.Unit // syntactically possible, but should never happen when translating C# if statement

                    Expr.IfThenElse (left, thenExpr, Some b, false)

            let rec isEqualExpr a b = 
                match a,b with 
                | Expr.Ident a, Expr.Ident b when a = b -> true
                | Expr.LongIdent (_,a), Expr.LongIdent (_,b) when joinLongIdentWithDots a = joinLongIdentWithDots b -> true
                | _, _ -> false

            let rec joinAnd a b =
                match a, b with 
                | Expr.Match (b,c), Expr.Match (b',c') -> 
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

                        Expr.Match(b,clauses)

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
                                MatchClause.Clause (SynPat.Tuple (false, [leftMatch; rightMatch], range0), whenClauseJoinAnd leftWhenExpr rightWhenExpr, first)
                                wild 
                            ]

                        Expr.Match(b,clauses)
                    
                | Expr.Match (b,c), e -> 
                    let c = 
                        match List.rev c with 
                        | x::[ (MatchClause.Clause(h,k,first)) ] -> 
                            [
                                MatchClause.Clause(h,(whenClauseJoinAnd k (Some e)),first); x
                            ]
                        | x::xs -> 
                            let xs = 
                                xs 
                                |> List.map (fun (MatchClause.Clause(h,i,first)) -> MatchClause.Clause(h,whenClauseJoinAnd i (Some e), first))
                            [x] @ xs
                        | [] -> []

                    Expr.Match (b,c)
                | Expr.App(isAtomic, isInfix, Expr.Ident "not", left), Expr.Match (_, clauses) -> // not e1 && e2 === e1 || e2
                    let thenExpr = 
                        match clauses |> List.rev with 
                        | _::MatchClause.Clause(_, _, thenExpr)::_ -> thenExpr
                        | _ -> Expr.Const SynConst.Unit // syntactically possible, but should never happen when translating C# if statement

                    Expr.IfThenElse (left, thenExpr, Some b, false)

                | e, Expr.Match (_,clauses) -> joinAnd b a // Swap around and will be sorted via match statement

                | a, b -> 
                    let a = Expr.App (ExprAtomicFlag.NonAtomic, true, Expr.Ident "op_BooleanAnd", a)
                    Expr.App (ExprAtomicFlag.NonAtomic, true, a, b) |> Expr.Paren
                    
            match testExpr  with 
            | Expr.CsharpIsMatch (expr, left) -> 
                let clauses = [ MatchClause.Clause(left,None,firstResult); MatchClause.wild secondResult ]
                Expr.Match (expr, clauses)

            | Expr.App (atomic,isInfix,left,right) -> 

                let left' = removeMatchPlaceholder left firstResult secondResult
                let right' = removeMatchPlaceholder right firstResult secondResult

                match left', right' with 
                | Expr.Ident "not", Expr.Match (b,c) -> logicalMatchExpr (b,c)
                
                | Expr.App (e,f,(Expr.Ident "op_BooleanOr"),h), a -> joinOr h a
                | Expr.App (e,f,(Expr.Ident "op_BooleanAnd"),h), a -> joinAnd h a
                | _, _ -> Expr.App(atomic, isInfix, left', right')

            | Expr.Paren e -> removeMatchPlaceholder e firstResult secondResult
            | e -> e

        let rec walker tree = 
            match tree with 
            | Expr.IfThenElse (a,b,c,d) -> 

                if containsCsharpIsMatch a then 
                    let cClause = (match c with | Some x -> x | None -> Expr.Const SynConst.Unit)
                    removeMatchPlaceholder (convertLogicalOrToLogicalAnd a) b cClause |> Some
                else None
            | _ -> None
        replaceExpr walker tree

    let wrapNewKeyword tree = 
        replaceExpr (fun tree ->
            match tree with 
            | Expr.DotGet (Expr.New (a,b,c), d) -> Expr.DotGet (Expr.Paren (Expr.New (a,b,c)) ,d) |> Some
            | e -> None) tree

    let fixCsharpReservedNames tree = 
        replaceExpr (fun tree ->
            match tree with 
            | Expr.LongIdent (a, b) when (joinLongIdentWithDots b).StartsWith "@" -> Expr.LongIdent (a, (joinLongIdentWithDots b).Substring(1) |> toLongIdentWithDots) |> Some
            | e -> None) tree

    let replaceAsyncOps returnType tree = 
        let awaitTask expr = 
            ExprOps.toInfixApp (expr) (toLongIdent "op_PipeRight") (toLongIdent "Async.AwaitTask")

        let requiresReturnKeyword = 
            match returnType with 
            | SynType.App ((SynType.LongIdent longIdent),b,args,d, e,f,g) -> 
                let name = longIdent |> joinLongIdentWithDots
                name = "Task" && args |> List.isEmpty |> not
            | _ -> false

        let rec walker = function
            | Expr.DoBang _ as expr -> expr |> awaitTask |> Some
            | Expr.LetOrUseBang (b,c,d,e,Expr.LongIdent (f,g)) -> Expr.YieldOrReturn ((false, true), e |> awaitTask) |> Some
            | Expr.LetOrUseBang (b,c,d,e,Expr.Sequential(g,h,i)) -> 
                let f = Expr.Sequential(g,replaceExpr walker h, replaceExpr walker i)
                Expr.LetOrUseBang (b,c,d,e |> awaitTask, f) |> Some
            | Expr.LetOrUseBang (b,c,d,e,Expr.LetOrUseBang (g,h,i,j,k)) -> 
                let f = replaceExpr walker (Expr.LetOrUseBang (g,h,i,j,k))
                Expr.LetOrUseBang (b,c,d,e |> awaitTask, f) |> Some
            | Expr.LetOrUseBang (b,c,d,e,Expr.App (f,g,h,i)) -> 
                let f = Expr.YieldOrReturn ((false, true), Expr.App (f,g,h,i))
                Expr.LetOrUseBang (b,c,d,e |> awaitTask, f) |> Some
            | Expr.LetOrUseBang (b,c,d,e,f) -> 
                Expr.LetOrUseBang (b,c,d, e |> awaitTask, f) |> Some
            | _ -> None

        let rec replaceLetRootWithReturn = function 
            | Expr.LetOrUse (a,b,c, Expr.LetOrUse (e,f,g,h)) ->  
                let d = replaceLetRootWithReturn (Expr.LetOrUse (e,f,g,h))
                Expr.LetOrUse (a,b,c,d)
            | Expr.LetOrUse (a,b,c, Expr.LetOrUseBang (e,f,g,h,i)) ->  
                let d = replaceLetRootWithReturn (Expr.LetOrUseBang (e,f,g,h,i))
                Expr.LetOrUse (a,b,c,d)
            | Expr.LetOrUse (a,b,c, d) ->  
                Expr.LetOrUse (a,b,c,Expr.YieldOrReturn ((false, true), d))
            | Expr.LetOrUseBang (b,c,d,e,f) -> 
                Expr.LetOrUseBang (b,c,d,e, replaceLetRootWithReturn f)
            | Expr.Sequential (b,c,d) -> 
                Expr.Sequential (b,c, replaceLetRootWithReturn d)
            | e -> Expr.YieldOrReturn ((false, true), e)

        let handleReturnKeyword e = 
            match requiresReturnKeyword with 
            | true ->  replaceLetRootWithReturn e
            | false -> e 
                        
        tree 
        |> replaceExpr walker
        |> handleReturnKeyword

    let shouldWrapInComp tree = 

        ParserUtil.containsExpr (function 
            | Expr.YieldOrReturn ((_,_), _) as x -> true
            | Expr.DoBang (_) -> true
            | Expr.LetOrUseBang (_) -> true
            | _ -> false) tree
        |> function
        | [Expr.YieldOrReturn ((true,_), _)] -> Some CompExpr.Seq
        | [Expr.YieldOrReturn ((_,true), _)] -> Some CompExpr.Async
        | [Expr.DoBang (_)] -> Some CompExpr.Async
        | [Expr.LetOrUseBang (_)] -> Some CompExpr.Async
        | _ -> None

    let wrapInComp returnType name e = 
        let toComp name expr =
            ExprOps.toApp (toLongIdent name) (Expr.CompExpr (false, ref false, expr))
        match name with
        | CompExpr.Async -> ExprOps.toInfixApp (e |> replaceAsyncOps returnType |> toComp "async") (toLongIdent "op_PipeRight") (toLongIdent "Async.StartAsTask")
        | CompExpr.Seq -> toComp "seq" e

    let replaceDotGetIfNotInLetBinding tree = 
        let isLastExpressionUnit = 
            containsExpr (function 
                | Expr.Const SynConst.Unit -> true
                | _ -> false)

        let walker tree = 
            match tree with
            | Expr.DotGet (a,b) -> 
                match a with 
                | Expr.LongIdent (c, prefixName) -> 
                    let b = joinLongIdentWithDots b
                    let prefixName = joinLongIdentWithDots prefixName
                    sprintf "%s.%s" prefixName b |> toLongIdent |> Some
                | _ -> None

            | Expr.Sequential (s2,s3,s4) -> 
                match s3 with 
                | Expr.IfThenElse (x,y,z,i) when Option.isNone z ->
                    match isLastExpressionUnit y with 
                    | true -> Expr.IfThenElse (x,y, Some s4, i) |> Some
                    | false -> None
                | _ -> None
            | _ -> None
        replaceExpr walker tree

    let rewriteInLetExp tree =

        let rec walker tree = 
            match tree with 
            | Expr.LetOrUse (x,y,z,Expr.InLetPlaceholder) ->  
                Expr.LetOrUse (x,y,z, Expr.Const SynConst.Unit) |> Some
            | Expr.LetOrUseBang (b,c,d,e,Expr.InLetPlaceholder) -> Expr.YieldOrReturn ((false, true), e) |> Some
            | Expr.Sequential (s2,s3,s4) -> 
                match s3 with  
                | Expr.LetOrUse (x,y,z,Expr.InLetPlaceholder) -> 
                    let e = replaceExpr walker s4
                    Expr.LetOrUse (x,y,z,e) |> Some
                | Expr.LetOrUseBang (b,c,d,e,f) -> 
                    let f = replaceExpr walker s4
                    Expr.LetOrUseBang (b,c,d,e,f) |> Some
                | _ -> None
            | _ -> None
        replaceExpr walker tree

    let rec rewriteMethodWithPrefix methodNames tree = 
        let recurse = rewriteMethodWithPrefix methodNames

        let walker tree = 
            match tree with 
            | Expr.LongIdent (a, LongIdentWithDots(b, r)) -> 
                let firstIdent = (List.head b).idText
                methodNames |> Seq.tryFind (fun (className, name) -> firstIdent.Contains name )
                |> Option.map (fun (className: string option, _) -> 
                    let replacement = 
                        match className with 
                        | None -> sprintf "this.%s"
                        | Some staticMethodClassName when staticMethodClassName <> DefaultNames.className -> sprintf "%s.%s" staticMethodClassName
                        | _ -> sprintf "%s"

                    LongIdentWithDots(b, r) |> joinLongIdentWithDots  |> replacement |> toLongIdent)
            | e -> None
        replaceExpr walker tree
            
        //// root of tree should not be contain immediate child of InLetPlaceholder (has no meaning)
        //| Expr.LetOrUse (x,y,z,i) ->  
        //    Expr.LetOrUse (x,y,z,recurse i)

        //| Expr.Sequential (s1,s2,s3,s4) -> Expr.Sequential (s1,s2,recurse s3,recurse s4)
        //| Expr.App (a,b,c,d) -> Expr.App (a,b, recurse c, recurse d)
        //| Expr.IfThenElse (a,b,c,d,e) -> Expr.IfThenElse (a,recurse b,c |> Option.map recurse,d,e)

        //| Expr.ForEach (a,b,c,d,e,f) -> 
        //    let e2 = recurse e
        //    let f2 = recurse f

        //    Expr.ForEach (a,b,c,d, e2, f2)

        //| _ -> tree

    let simplifyTree = 
        replaceExpr (fun tree -> 
            match tree with 
            | Expr.Paren (Expr.Tuple []) -> Expr.Const SynConst.Unit |> Some
            | Expr.Tuple [] -> Expr.Const SynConst.Unit |> Some
            | _ -> None)

    let correctXamarinFormsPage u ns =
        if u |> List.exists (fun x -> x.UsingNamespace.Contains "Xamarin.Forms") then 
            let updateClass (c: Class) = 
                let loadXaml = 
                    // let synArgInfo = SynArgInfo ([], false, None)
                    // let synInfo = SynValInfo ([], synArgInfo)
                    // let synValData = SynValData (None,  synInfo, None)
                    let expr = 
                        let name = toLongIdent "base.LoadFromXaml"
                        let ofType = 
                            Expr.TypeApp (toLongIdent "typeof", [c.Name.Name |> toLongIdentWithDots |> SynType.LongIdent])
                        ExprOps.toAtomicApp name ofType
                    // LetBind (None, NormalBinding, false, false, [], synValData, Pat.Wild, expr )
                    {
                        IsPublic = false
                        Name = SynPat.Wild range0
                        Type =  SynType.StaticConstant (SynConst.Unit, range0)
                        Initializer = Some expr
                        IsConst = true
                        IsStatic = false
                    }

                let filterInitializeComponent = 
                    List.filter 
                        (containsExpr 
                            (function 
                            | Expr.App (_, _, Expr.Ident "InitializeComponent", _) -> true
                            | Expr.App (_, _, Expr.LongIdent (_, xs), _) 
                                when joinLongIdentWithDots xs = "InitializeComponent" -> true
                            | _ -> false) >> not)

                let mapCtorBody = 
                    List.map (fun (x: Ctor) -> 
                        {x with Body = filterInitializeComponent x.Body })

                {c with 
                    Constructors = mapCtorBody c.Constructors
                    Fields = loadXaml :: c.Fields}

            let ns = 
                ns  |> List.map (fun x -> 
                    { x with Structures = 
                                x.Structures |> List.map (fun x -> 
                                    match x with 
                                    | C c -> updateClass c |> C
                                    | x -> x ) })
            ns
        else ns


    // This is a work in progress. It only orders by count of dependencies that are known from the code supplied. 
    // This should be good enough, but will not work 
    // when d -> c, c -> b, b -> a the final order of b,c,d is non-deterministic. a will be first. 
    let reorderStructures s =

        let getName = function 
            | C c -> c.Name.Name
            | Interface (name, _) -> name.idText
            | E e -> e.Name
            | Structure.RootAttributes _ -> "" // Attributes don't have an order

        let names = s |> List.map getName
        let mapping = s |> List.map (fun x -> getName x, x) |> Map.ofList

        let getDependencies = function 
            | C c -> 
                [
                    yield! (c.BaseClass |> Option.map SynType.getName) |> Option.toList
                    yield! c.Fields |> List.map (fun x -> SynType.getName x.Type)
                    yield! c.Properties |> List.map (fun x -> SynType.getName x.Type)
                    yield! c.Methods |> List.collect (fun x -> 
                        SynType.getName x.ReturnType :: 
                            (x.Parameters |> List.choose (SynPat.getType >> Option.map SynType.getName)) )
                    yield! c.ImplementInterfaces |> List.map SynType.getName
                    yield! c.Constructors |> List.collect (fun x -> x.Body |> List.collect getNames)
                    yield! c.Methods |> List.collect (fun x -> getNames x.Body)
                    yield! c.Properties |> List.choose (fun x -> 
                        x.Get |> Option.map (getNames)) |> List.concat
                ]   
            | Interface (_, methods) -> 
                methods |> List.collect (fun (InterfaceMethod (_, types)) -> types |> List.map SynType.getName)
            | E _ -> []  
            | Structure.RootAttributes _ -> []        

        let dependencyCount = getDependencies >> List.filter (fun x -> names |> List.contains x)
  
        let filterDependencyPermutations lists = 
            if List.isEmpty lists then [] 
            else
                let longestOrderedPermutations = 
                    let longest = lists |> List.maxBy List.length
                    longest 
                    |> List.permuteWithOrder 
                    |> List.filter (fun x -> x <> longest)

                lists |> List.filter (fun x -> longestOrderedPermutations |> List.contains x |> not)
            
        let dependencyOrder = 
            let graph = s |> List.collect (fun x -> dependencyCount x |> List.map (fun d -> getName x, d))

            s
            |> List.map (getName >> DAG.topologicalSort graph >> List.rev)
            |> filterDependencyPermutations  
            |> List.zipAnyLength 

        let noDeps = 
            s 
            |> List.map (fun x -> x |> dependencyCount |> List.length, getName x)
            |> List.filter (fun (a,b) -> a = 0)
            |> List.map snd
        
        (noDeps @ dependencyOrder) |> List.distinct |> List.choose (fun x -> Map.tryFind x mapping )

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

    let addNewLineToLet file tree = 
        let moveDown = Range.moveDownLine file
        let moveAndAdd r1 = r1 |> Range.moveDownLine file |> Range.addLine file

        let rec loop tree = 
            match tree with 
            | SynExpr.Paren (x, r1, p, r) -> SynExpr.Paren (x, moveDown r1, p, moveDown r) |> Some
            | SynExpr.Const (x, r1) -> SynExpr.Const (x, moveDown r1) |> Some
            | SynExpr.Typed (expr, typeName, r1) -> SynExpr.Typed (expr, typeName, moveDown r1) |> Some
            | SynExpr.Tuple (b, xs, rs, r)  -> SynExpr.Tuple (b, xs, rs, moveDown r) |> Some
            | SynExpr.App (a,b, c, d, r) -> SynExpr.App (a, b, c, d, moveDown r) |> Some
            | SynExpr.LetOrUse (a,b,c, d, r1) -> SynExpr.LetOrUse (a,b,c, replaceSynExpr loop d, moveAndAdd r1) |> Some
            | SynExpr.Ident (r) -> SynExpr.Ident (Ident(r.idText, moveDown r.idRange)) |> Some
            | SynExpr.LongIdent (a,b, c, r) -> SynExpr.LongIdent (a,b, c, moveDown  r) |> Some
            | SynExpr.Set (left, right, r) -> SynExpr.Set (left, right, moveDown r) |> Some
            | SynExpr.Null r -> SynExpr.Null <| moveDown r |> Some
            | SynExpr.LongIdentSet (a,b,c) -> SynExpr.LongIdentSet (a,b, moveDown c) |> Some
            | SynExpr.Sequential (a,b,c,d, r1) -> 
                SynExpr.Sequential (a,b, replaceSynExpr loop  c, replaceSynExpr loop d,  moveAndAdd r1 ) |> Some
            | SynExpr.IfThenElse (a, b, c,d, e, r1, r2) -> 
                SynExpr.IfThenElse (replaceSynExpr loop a, replaceSynExpr loop b, c |> Option.map (replaceSynExpr loop ), d,e, moveAndAdd r1, moveAndAdd r2) |> Some
            | SynExpr.ForEach (a,b,c,d,e,f,g) ->  SynExpr.ForEach (a,b,c,d, replaceSynExpr loop e, replaceSynExpr loop f, g) |> Some
            | SynExpr.TypeApp (a,b,c,d,e,f,g) -> SynExpr.TypeApp (replaceSynExpr loop a, moveDown b,c,d,e, moveDown f, moveDown g) |> Some
            | SynExpr.DotGet (a,b,c, d) -> SynExpr.DotGet (a, moveDown b, c, moveDown d)  |> Some
            | SynExpr.FromParseError (a,b) -> SynExpr.FromParseError (replaceSynExpr loop a, moveAndAdd b) |> Some
            | SynExpr.Match (a,b,c,d) -> SynExpr.Match (a,b,c,d) |> Some
            | SynExpr.For (a,b,c,d,e,f,g) -> SynExpr.For (a,b, replaceSynExpr loop c,d, replaceSynExpr loop e, replaceSynExpr loop f,g) |> Some
            | SynExpr.DotIndexedGet (a,b,c,d) -> SynExpr.DotIndexedGet (a,b,c,d) |> Some
            | SynExpr.DotIndexedSet (a,b,c,d,f,g) -> SynExpr.DotIndexedSet (a,b,c,d,f,g) |> Some
            | SynExpr.While (a,b,c,d) -> SynExpr.While (a, replaceSynExpr loop b,c,d) |> Some
            | SynExpr.TryWith (a,b,c,d,e,f,g) -> SynExpr.TryWith (replaceSynExpr loop a,moveDown b,c, moveDown d, moveDown e,f,g) |> Some
            | SynExpr.Lambda (a,b,c,d,e,f) -> SynExpr.Lambda (a,b,c, replaceSynExpr loop d, e, moveAndAdd f) |> Some
            | SynExpr.LetOrUseBang (a,b,c,d,e,f,g, h) -> SynExpr.LetOrUseBang (a,b,c,d,replaceSynExpr loop e,f, replaceSynExpr loop g, moveAndAdd h) |> Some
            | SynExpr.YieldOrReturn (a,b,c) -> SynExpr.YieldOrReturn (a, replaceSynExpr loop b, moveAndAdd c) |> Some
            | _ -> None

        replaceSynExpr (fun tree -> 
            match tree with 
            | SynExpr.LetOrUse (a,b,c, d, r1) -> SynExpr.LetOrUse (a,b,c, replaceSynExpr loop d, moveAndAdd r1) |> Some
            | SynExpr.LetOrUseBang (a,b,c,d,e,f,g,h) -> SynExpr.LetOrUseBang (a,b,c,d,replaceSynExpr loop  e, f,replaceSynExpr loop g,moveAndAdd  h) |> Some
            | _ -> None) tree

    // For some reason Fantomas does not format let bindings correctly and wants to use the let .... in statements. 
    // This is done because there are not line endings. 
    // To correctly the problem, walk the tree and bump the line number, via the range, to add a new line. 
    // Fantomas will then print the let binding with out the in keyword, and instead, move to the next line. 
    let removeFsharpIn file tree =

        match tree with
        | ParsedInput.ImplFile(ParsedImplFileInput(fn, script, name, a, b, modules, c)) ->

            let replaceOnModuleOrNamespace func (SynModuleOrNamespace (a,b,c,modules,e,f,g,h)) = 
                SynModuleOrNamespace (a,b,c,modules |> List.map func, e,f,g,h)

            let replaceTypeDefn funcB funcMember (SynTypeDefn.TypeDefn (a,b,members,d)) =
                SynTypeDefn.TypeDefn (a, funcB b,funcMember members,d)

            let modules = 
                modules |> List.map (replaceOnModuleOrNamespace (fun x -> 
                    match x with 
                    | SynModuleDecl.Types (a,b) -> 
                        let a = 
                            a |> List.map (replaceTypeDefn (function 
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
                                    | x -> x) 
                                    (fun members -> 
                                        members |> List.map (fun x -> 
                                        match x with 
                                        | SynMemberDefn.Member ((Binding (a,b,c,d,e,f,g,h,i,expr,k,l)),m2) -> 
                                            let m2 = Range.addLine file m2
                                            let expr = expr |> addNewLineToLet file

                                            SynMemberDefn.Member ((Binding (a,b,c,d,e,f,g,h,i,expr,k,l)),m2)
                                        | x -> x)) )
                        SynModuleDecl.Types (a,b)
                    | x -> x ) )
            ParsedImplFileInput(fn, script, name, a, b, modules, c) |> ParsedInput.ImplFile
        | _ -> failwith "F# Interface file (*.fsi) not supported."