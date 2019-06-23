namespace FSharper.Core
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis
open System.Threading
open Microsoft.CodeAnalysis.CSharp.Syntax
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Range

module ExprOps = 
    let withParenIfReq expr = 
        match expr with 
        | Expr.App (_, false,Expr.App (_, true, _, _), _) as e -> Expr.Paren e
        | e -> e

    let toApp left right = 
        Expr.App (ExprAtomicFlag.NonAtomic, false, left, right)

    let toInfixApp left op right  = 
        toApp (Expr.App (ExprAtomicFlag.NonAtomic, true, op, left)) right

[<AutoOpen>]
module ParserUtil = 

    let toSingleIdent (s:string) = Ident(s, range0)

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
            | Expr.LetOrUse (x,y,z,i) -> 
                let z = z |> List.map (fun (FSharpBinding.LetBind(a,b,c,d,e,f,g,h)) -> FSharpBinding.LetBind(a,b,c,d,e,f,g, replaceExpr h) )
                Expr.LetOrUse (x,y,z, replaceExpr i)
            | Expr.App (a,b,c,d) -> Expr.App (a,b, replaceExpr c, replaceExpr d)
            | Expr.ForEach (a,b,c,d,e,f) -> Expr.ForEach (a,b,c,d, replaceExpr e, replaceExpr f)    
            | Expr.Match (a,b,c,d) -> Expr.Match (a, replaceExpr b,c,d)
            | Expr.Lambda (a,b,c,d) -> Expr.Lambda (a,b,c, replaceExpr d)
            | Expr.Paren e -> replaceExpr e |> Expr.Paren
            | Expr.For (a,b,c,d,e,f) -> Expr.For (a,b,replaceExpr c, d, replaceExpr e,replaceExpr f)
            | Expr.While (a,b,c) -> Expr.While (a, replaceExpr b, replaceExpr c)
            | Expr.YieldOrReturn (a, b) -> Expr.YieldOrReturn (a, replaceExpr b)
            | Expr.LongIdentSet(a,b) -> Expr.LongIdentSet(a, replaceExpr b)
            | Expr.DotIndexedSet(a,b,c) -> Expr.DotIndexedSet( replaceExpr a,b, replaceExpr c)
            | Expr.Set (a,b) -> Expr.Set ( replaceExpr a, replaceExpr b)
            | Expr.Tuple xs -> xs |> List.map replaceExpr |> Expr.Tuple
            | Expr.DoBang e -> e |> replaceExpr |> Expr.DoBang
            | Expr.CompExpr (a,b,c) -> Expr.CompExpr (a,b, replaceExpr c)
            | Expr.LetOrUseBang (a,b,c,d,e,f) -> Expr.LetOrUseBang (a,b,c,d, replaceExpr e,replaceExpr f)
            | e -> e

    let rec getFirstExpr f tree = 
        let getFirstExpr = getFirstExpr f
        match f tree with 
        | Some x -> Some x
        | None -> 
            match tree with 
            | Expr.Sequential (s1,s2,s3,s4) -> getFirstExpr s3 |> Option.orElse (getFirstExpr s4)
            | Expr.Downcast (e,a) ->  getFirstExpr e
            | Expr.DotSet (a,b,c) -> getFirstExpr a |> Option.orElse ( getFirstExpr c)
            | Expr.DotGet (e, a) -> getFirstExpr e
            | Expr.IfThenElse (a,b,c,d,e) -> getFirstExpr a |> Option.orElse (getFirstExpr b) |> Option.orElse (c |> Option.bind getFirstExpr)
            | Expr.LetOrUse (x,y,z,i) -> getFirstExpr i
            | Expr.LetOrUseBang (a,b,c,d,e,f) -> getFirstExpr e |> Option.orElse (getFirstExpr f)
            | Expr.App (a,b,c,d) -> getFirstExpr c |> Option.orElse (getFirstExpr d)
            | Expr.ForEach (a,b,c,d,e,f) -> getFirstExpr e |> Option.orElse (getFirstExpr f)
            | Expr.Match (a,b,c,d) -> getFirstExpr b
            | Expr.Lambda (a,b,c,d) -> getFirstExpr d
            | Expr.Paren e -> getFirstExpr e
            | Expr.For (a,b,c,d,e,f) -> getFirstExpr c |> Option.orElse (getFirstExpr e) |> Option.orElse (getFirstExpr f)
            | Expr.While (a,b,c) -> getFirstExpr b |> Option.orElse (getFirstExpr c)
            | Expr.YieldOrReturn (a, b) -> getFirstExpr b
            | Expr.DoBang e -> getFirstExpr e
            | Expr.CompExpr (a,b,c) -> getFirstExpr c
            | e -> None

    let rec containsExpr f tree = 
        let containsExpr = containsExpr f
        match tree with 
        | x when f x -> [x]
        | Expr.Sequential (s1,s2,s3,s4) -> containsExpr s3 @ containsExpr s4
        | Expr.Downcast (e,_) when f tree -> [tree]
        | Expr.Downcast (e,_) -> containsExpr e
        | Expr.DotSet (a,b,c) -> containsExpr a @ containsExpr c
        | Expr.DotGet (e, _) -> containsExpr e
        | Expr.IfThenElse (a,b,c,d,e) -> (containsExpr a) @ (containsExpr b)
        | Expr.LetOrUse (x,y,z,i) -> containsExpr i
        | Expr.LetOrUseBang (a,b,c,d,e,f) -> containsExpr e @ containsExpr f
        | Expr.App (a,b,c,d) ->  containsExpr c @ containsExpr d
        | Expr.ForEach (a,b,c,d,e,f) -> containsExpr e @ containsExpr f
        | Expr.Match (a,b,c,d) -> 
            c |> List.map (fun (MatchClause.Clause (a,b,c)) -> containsExpr c) |> List.reduce (@)
        | Expr.Lambda (a,b,c,d) -> containsExpr d
        | Expr.Paren e -> containsExpr e
        | Expr.For (a,b,c,d,e,f) -> containsExpr c @  containsExpr e @ containsExpr f
        | Expr.While (a,b,c) -> containsExpr b @ containsExpr c
        | Expr.YieldOrReturn (a, b) -> containsExpr b
        | Expr.DoBang e -> containsExpr e
        | Expr.CompExpr (a,b,c) -> containsExpr c
        | _ -> []

    let replaceAnyPostOrPreIncrement =
        replaceExpr 
            (function
                | Expr.Sequential(a,b,Expr.LongIdentSet (c,d), expr) -> Expr.LongIdentSet (c,d) |> Some
                | Expr.Sequential(a,b, expr, Expr.LongIdentSet (c,d)) -> Expr.LongIdentSet (c,d) |> Some
                | _ -> None)

    let fixKeywords k = 
        match k with 
        | "object" -> "obj"
        | "long" -> "int64"
        | "IEnumerable" -> "seq"
        | "void" -> "unit"
        | "ushort" -> "uint16"
        | x -> x

    let toIdent (s:string) = 
        s.Split('.') 
        |> Array.toList
        |> List.map toSingleIdent

    let toLongIdentWithDots (s:string) = 
        LongIdentWithDots (toIdent s, [range0])

    let toLongIdent (s:string) = 
        Expr.LongIdent (false, toLongIdentWithDots s)

    let sequential xs = 
        match xs |> Seq.toList with
        | [] -> Expr.Const SynConst.Unit
        | [x] -> x
        | xs ->
            // List is built in reverse order to preserve the structure allowing for transformatins     
             xs |> List.rev |> Seq.reduce (fun y x ->  // List is rev, y x are reversed to correct this. 
                match x with 
                | Expr.LetOrUse _ -> 
                    replaceExpr (fun tree -> 
                        match tree with 
                        | Expr.LetOrUse (a,b,c,Expr.InLetPlaceholder) -> Expr.LetOrUse (a,b,c,y) |> Some
                        | _ -> None) x
                | Expr.LetOrUseBang _ -> 
                    replaceExpr (fun tree -> 
                        match tree with 
                        | Expr.LetOrUseBang (a,b,c,d,e,Expr.InLetPlaceholder) -> Expr.LetOrUseBang (a,b,c,d,e,y) |> Some
                        | _ -> None) x
                
                | _ -> Expr.Sequential (SequencePointsAtSeq, true, x, y) ) 

    let joinLongIdentWithDots (b:LongIdentWithDots) = 
        b.Lid |> List.map (fun x -> x.idText) |> String.concat "." 

    let mapLongIdent f ident = 
        match ident with 
        | Expr.LongIdent (a,b) -> b |> joinLongIdentWithDots |> f |> toLongIdent
        | x -> x

    let toPatId ident = 
        SynSimplePat.Id (Ident(ident, range0), None, false, true, false, range0)

    let rec parseType (t: TypeSyntax) = 
        match t with 
        | :? ArrayTypeSyntax as ats ->             
            let a = ats.ElementType.WithoutTrivia().ToString() |> fixKeywords 
            SynType.Array (1, toLongIdentWithDots (sprintf "%s" a) |> SynType.LongIdent, range0)
        
        | :? NameSyntax as n -> 
            match n with 
            | :? GenericNameSyntax as genericName -> 
                let typeArgs = 
                    genericName.TypeArgumentList.Arguments
                    |> Seq.map (fun x -> x.WithoutTrivia().ToString())
                    |> Seq.map (toLongIdentWithDots >> SynType.LongIdent)
                    |> Seq.toList

                let x = genericName.Identifier.WithoutTrivia().ToFullString() |> fixKeywords |> toLongIdentWithDots |> SynType.LongIdent

                SynType.App  (x, None, typeArgs, [], None, false, range0)

            | _ -> n.WithoutTrivia().ToString() |> fixKeywords |> toLongIdentWithDots |> SynType.LongIdent
        | x -> x.WithoutTrivia().ToString() |> fixKeywords |> toLongIdentWithDots |> SynType.LongIdent 

    let createErorrCode (e:SyntaxNode) = 
        let c = CancellationToken()
        printfn "Input C#:"
        printfn "%s" <| (e.SyntaxTree.GetText c).ToString()
        let filePosition = e.SyntaxTree.GetLineSpan (e.FullSpan, c)

        let pos = sprintf "Line:%d, Col:%d" filePosition.StartLinePosition.Line filePosition.StartLinePosition.Character
        let s = sprintf "Not supported: %s %s" (e.WithoutTrivia().ToString()) pos
        printfn "%A - %s" (e.GetType()) s
        LongIdentWithDots([toSingleIdent s], [range0]) // Don't split on dots so the message is somewhat clear

    let parseVaribleName v = 
        let rec toPat v = 
            match v with 
            | Expr.Ident x -> Pat.Named (Pat.Wild, Ident(x, range0), false, None)
            | Expr.LongIdent (a,b) -> Pat.LongIdent (b,None, None, SynConstructorArgs.Pats [], None)
            | Expr.Tuple xs -> xs |> List.map toPat |> Pat.Tuple
            | _ -> failwith "Not valid for Pat"
        toPat v

    let (|IsIncrement|IsDecrment|Neither|) (x:SyntaxToken) = 
        match x.Kind() with 
        | SyntaxKind.PlusPlusToken -> IsIncrement
        | SyntaxKind.MinusMinusToken -> IsDecrment
        | _ -> Neither

    let (|ParseName|_|) (x:SyntaxNode) = 
        match x with 
        | :? IdentifierNameSyntax as x -> x.Identifier.ValueText |> toLongIdentWithDots |> Some
        | _ -> None

    let (|IsPostFixIncrement|IsPostFixDecrment|IsNotPostFix|) (x:SyntaxNode) = 
        match x with 
        | :? PostfixUnaryExpressionSyntax as x -> 
            match x.OperatorToken with 
            | IsIncrement -> IsPostFixIncrement
            | IsDecrment -> IsPostFixDecrment
            | Neither -> IsNotPostFix
        | _ -> IsNotPostFix

    let (|IsPreFixIncrement|IsPreFixDecrment|IsNotPreFix|) (x:SyntaxNode) = 
        match x with 
        | :? PrefixUnaryExpressionSyntax as x -> 
            match x.OperatorToken with 
            | IsIncrement -> IsPreFixIncrement
            | IsDecrment -> IsPreFixDecrment
            | Neither -> IsNotPreFix
        | _ -> IsNotPreFix

    let (|PostFixOperandName|_|) (x:SyntaxNode) = 
        match x with 
        | :? PostfixUnaryExpressionSyntax as x -> 
            match x.Operand with 
            | ParseName name -> Some name
            | _ -> None
        | _ -> None

    let (|BinaryOp|_|) (x:Expr) = 
        match x with 
        | Expr.App (_, _, Expr.App (_, _, Expr.Ident op, left), right) -> Some (left, op, right)
        | _ -> None

    let rec (|FindIdent|_|) (x:Expr) = 

        //let rec walker tree = 
        match x with 
        | Expr.LongIdent (_,b)  -> Some ([joinLongIdentWithDots b, None])
        //| Expr.Ident s -> Some [s]
        | Expr.Sequential (_, _, a,b) -> 
            match a,b with 
            | FindIdent a, FindIdent b -> Some (a @ b)
            | FindIdent a,  _ -> Some a
            | _, FindIdent a  -> Some a
            | _ -> None

        | Expr.DotIndexedSet (a,b,c) -> 
            match a, b with 
            | FindIdent xs, ys -> 
                let ys = 
                    ys 
                    |> List.choose (fun (IndexerArg.One (a)) -> 
                        match a with 
                        | FindIdent xs -> Some xs
                        | _ -> None)
                    |> List.concat 
                    |> List.map (fun (x,_) -> x, Some c)
                    
                let xs = xs |> List.map (fun (x,_) -> x, Some c)
                Some (xs @ ys)
            | _ -> None

        | Expr.Paren x -> 
            match x with
            | FindIdent xs -> Some xs
            | _ -> None 

        | Expr.LongIdentSet (name, x) -> Some([joinLongIdentWithDots name, Some x])
            
        | Expr.DotSet (a,b,c) -> 
            match a with 
            | FindIdent xs -> xs |> List.map (fun (x,_) -> x, Some c) |> Some
            | _ -> None

        | Expr.App (_, _, a,b) as tree -> 
            match a,b with 
            | FindIdent x, FindIdent y -> Some (x @ y)
            | FindIdent a,  _ -> Some a
            | _, FindIdent a  -> Some a
            | _ -> None

        | e -> printfn "Name: %A" e; None

    let identSetToIdentGet = function
        | Expr.LongIdentSet (l, Expr.InLetPlaceholder) -> Expr.LongIdent (false, l)
        | Expr.DotIndexedSet(e, [IndexerArg.One (x)], Expr.InLetPlaceholder) -> Expr.DotIndexedGet(e, [IndexerArg.One (x)])
        | Expr.Sequential (SequencePointsAtSeq, false, Expr.LongIdentSet (var, expr), Expr.DotIndexedSet(e, [IndexerArg.One (x)], Expr.InLetPlaceholder)) -> 
            Expr.DotIndexedGet(e, [IndexerArg.One (x)])
        | e -> e