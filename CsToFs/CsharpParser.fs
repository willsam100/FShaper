namespace CsToFs

open System
open Microsoft.FSharp.Compiler.Ast
open Microsoft.CodeAnalysis
open System.Linq
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax
open CsToFs
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Range

[<AutoOpen>]
module Parser = 
    let toIdent (s:string) = 
        s.Split('.') 
        |> Array.toList
        |> List.map (fun x -> Ident(x, range0))

    let toLongIdentWithDots (s:string) = 
        LongIdentWithDots (toIdent s, [range0])

    let toLongIdent (s:string) = 
        Expr.LongIdent (false, toLongIdentWithDots s)

    let sequential xs = 
        match xs |> Seq.toList with
        | _::_::_ as xs ->
            // List is built in reverse order to preserve the structure allowing for transformatins
            xs |> Seq.rev |> Seq.reduce (fun a b -> Expr.Sequential (SequencePointsAtSeq, true, b, a)) 
        | x::[] -> x
        | [] -> Expr.Const SynConst.Unit
 

type LineTransformer() = 

    static member ToCsharpSyntaxNode (node:SyntaxNode) =
        match node with
        | :? CSharpSyntaxNode as x -> Some x
        | _ -> None

    static member ParseAssignmentExpressionSyntax (node:AssignmentExpressionSyntax): Expr = 

        match node.Kind() with
        //| SyntaxKind.Assing -> 

        | SyntaxKind.SubtractAssignmentExpression -> 
            node.WithoutTrivia().ToFullString() |> Expr.Ident

        | SyntaxKind.SimpleAssignmentExpression -> 
            //let left = 
                //node.Left.ChildNodesAndTokens() 
                //|> Seq.map LineTransformer.ParseNodeOrToken
                //|> Seq.reduce (fun a b -> Expr.Sequential (SequencePointsAtSeq, true, a,b)) 

            let right = 
                //let r = 
                    node.Right.ChildNodesAndTokens() 
                    |> Seq.map LineTransformer.ParseNodeOrToken
                    |> sequential


            let l = LongIdentWithDots (node.Left.WithoutTrivia().ToFullString() |> toIdent, [range0])
            Expr.LongIdentSet (l, right)


        | SyntaxKind.AddAssignmentExpression -> 
            
            let isPlusEquals = 
                node.ChildTokens() |> Seq.exists (fun x -> x.Kind() = SyntaxKind.PlusEqualsToken)

            let right =
                if isPlusEquals then 
                    let paramList = 
                        node.Right.ChildNodes() 
                        |> Seq.filter (fun x -> x.Kind() = SyntaxKind.ParameterList) 
                        |> Seq.choose LineTransformer.ToCsharpSyntaxNode
                        |> Seq.map LineTransformer.ParseChsarpNode
                        |> Seq.last
                        //|> String.concat ""

                    let otherNodes = 
                        node.Right.ChildNodes()
                        |> Seq.filter (fun x -> x.Kind() <> SyntaxKind.ParameterList) 
                        |> Seq.choose LineTransformer.ToCsharpSyntaxNode
                        |> Seq.map LineTransformer.ParseChsarpNode
                        |> sequential

                    [paramList; otherNodes] |> sequential
                else 
                    node.Right.ChildNodesAndTokens() 
                    |> Seq.map LineTransformer.ParseNodeOrToken
                    |> sequential

            let left = 
                node.Left.ChildNodesAndTokens() 
                |> Seq.map LineTransformer.ParseNodeOrToken
                |> sequential

            // TODO is 
            //if isPlusEquals then 

            //    let (Line left) = left
            //    let (Line right) = right
            //    sprintf "%s.AddHandler(new EventHandler<_>(fun %s ))" left right |> Line
            //else

            //let (Line right) = right
            //let (Line left) = left

            Expr.Set (left, right)


        | _ -> node.WithoutTrivia().ToFullString() |> Expr.Ident

    static member ParseBinaryExpresson (node:BinaryExpressionSyntax):Expr = 

        let createLogicalExpression join = 

            let left = 
                match node.Left with 
                | :? BinaryExpressionSyntax as x -> LineTransformer.ParseBinaryExpresson x 
                | _ -> LineTransformer.ParseChsarpNode node.Left

            let right = 
                match node.Right with 
                | :? BinaryExpressionSyntax as x -> LineTransformer.ParseBinaryExpresson x 
                | _ -> LineTransformer.ParseChsarpNode node.Right

            Expr.App (ExprAtomicFlag.NonAtomic, false, 
                Expr.App(ExprAtomicFlag.NonAtomic, true, Expr.Ident join, left), right) 

        match node.Kind() with 
        | SyntaxKind.LogicalAndExpression -> 
            PrettyNaming.CompileOpName "&&" |> createLogicalExpression
        | SyntaxKind.LogicalOrExpression -> PrettyNaming.CompileOpName "||" |> createLogicalExpression 
        | SyntaxKind.NotEqualsExpression -> PrettyNaming.CompileOpName "!=" |> createLogicalExpression 
        | SyntaxKind.EqualsExpression -> PrettyNaming.CompileOpName "=" |> createLogicalExpression 
        //| _ -> 
            //printfn "Binary: %A" <| node.Kind();  
            //node.ToFullString() |> Line

    static member ParseToken (node:SyntaxToken) = 

        match node.Kind() with 
        | SyntaxKind.ExclamationToken -> "!" |> PrettyNaming.CompileOpName |> Expr.Ident
        | SyntaxKind.EqualsGreaterThanToken -> ">=" |> PrettyNaming.CompileOpName |> Expr.Ident
        | SyntaxKind.EqualsToken -> "=" |> PrettyNaming.CompileOpName |> Expr.Ident
        | SyntaxKind.GreaterThanToken -> ">" |> PrettyNaming.CompileOpName |> Expr.Ident
        | SyntaxKind.LessThanToken -> "<" |> PrettyNaming.CompileOpName |> Expr.Ident
        | SyntaxKind.LessThanEqualsToken -> "<=" |> PrettyNaming.CompileOpName |> Expr.Ident
        | SyntaxKind.SemicolonToken-> ";" |> PrettyNaming.CompileOpName |> Expr.Ident
        | SyntaxKind.NotEqualsExpression-> "!=" |> PrettyNaming.CompileOpName |> Expr.Ident
        | SyntaxKind.NumericLiteralToken -> 

            let s = node.Text
            let asInt = 
                match Int32.TryParse s with 
                | true, x -> Some x 
                | false,_ ->  None 

            let asInt64 = 
                match Int64.TryParse s with 
                | true, x -> Some x 
                | false,_ ->  None 

            let asfloat = 
                match Double.TryParse s with 
                | true, x -> Some x 
                | false,_ ->  None 

            match asInt, asInt64, asfloat with 
            | Some x, _, _ ->  Expr.Const <| SynConst.Int32 x
            | _, Some x, _ ->  Expr.Const <| SynConst.Int64 x
            | _, _, Some x ->  Expr.Const <| SynConst.Double x
            | _, _, _  ->  toLongIdent s

        | SyntaxKind.IdentifierToken -> node.WithoutTrivia().ToFullString() |> toLongIdent
        //| SyntaxKind.ReturnKeyword -> Expr.Const SynConst.Unit
        //| SyntaxKind.ForEachKeyword -> "for" |> toLongIdent

        | SyntaxKind.BoolKeyword -> "Bool" |> toLongIdent
        | SyntaxKind.ByteKeyword -> "Byte" |> toLongIdent
        | SyntaxKind.SByteKeyword -> "SByte" |> toLongIdent
        | SyntaxKind.ShortKeyword -> "Short" |> toLongIdent
        | SyntaxKind.UShortKeyword -> "UShort" |> toLongIdent
        | SyntaxKind.IntKeyword -> "Int" |> toLongIdent
        | SyntaxKind.UIntKeyword -> "UInt" |> toLongIdent
        | SyntaxKind.LongKeyword -> "Long" |> toLongIdent
        | SyntaxKind.ULongKeyword -> "ULong" |> toLongIdent
        | SyntaxKind.DoubleKeyword -> "Double" |> toLongIdent
        | SyntaxKind.FloatKeyword -> "Float" |> toLongIdent
        | SyntaxKind.DecimalKeyword -> "Decimal" |> toLongIdent
        | SyntaxKind.StringKeyword -> "String" |> toLongIdent
        | SyntaxKind.CharKeyword -> "Char" |> toLongIdent
        | SyntaxKind.VoidKeyword -> "Void" |> toLongIdent
        | SyntaxKind.ObjectKeyword -> "Object" |> toLongIdent
        | SyntaxKind.TypeOfKeyword -> "TypeOf" |> toLongIdent
        | SyntaxKind.SizeOfKeyword -> "SizeOf" |> toLongIdent
        | SyntaxKind.NullKeyword -> Expr.Null
        | SyntaxKind.TrueKeyword -> "True" |> toLongIdent
        | SyntaxKind.FalseKeyword -> "False" |> toLongIdent
        | SyntaxKind.IfKeyword -> "If" |> toLongIdent
        | SyntaxKind.ElseKeyword -> "Else" |> toLongIdent
        | SyntaxKind.WhileKeyword -> "While" |> toLongIdent
        | SyntaxKind.ForKeyword -> "For" |> toLongIdent
        | SyntaxKind.ForEachKeyword -> "ForEach" |> toLongIdent
        | SyntaxKind.DoKeyword -> "Do" |> toLongIdent
        | SyntaxKind.SwitchKeyword -> "Switch" |> toLongIdent
        | SyntaxKind.CaseKeyword -> "Case" |> toLongIdent
        | SyntaxKind.DefaultKeyword -> "Default" |> toLongIdent
        | SyntaxKind.TryKeyword -> "Try" |> toLongIdent
        | SyntaxKind.CatchKeyword -> "Catch" |> toLongIdent
        | SyntaxKind.FinallyKeyword -> "Finally" |> toLongIdent
        | SyntaxKind.LockKeyword -> "Lock" |> toLongIdent
        | SyntaxKind.GotoKeyword -> "Goto" |> toLongIdent
        | SyntaxKind.BreakKeyword -> "Break" |> toLongIdent
        | SyntaxKind.ContinueKeyword -> "Continue" |> toLongIdent
        | SyntaxKind.ReturnKeyword -> Expr.ReturnFromIf
        | SyntaxKind.ThrowKeyword -> "Throw" |> toLongIdent
        | SyntaxKind.PublicKeyword -> "Public" |> toLongIdent
        | SyntaxKind.PrivateKeyword -> "Private" |> toLongIdent
        | SyntaxKind.InternalKeyword -> "Internal" |> toLongIdent
        | SyntaxKind.ProtectedKeyword -> "Protected" |> toLongIdent
        | SyntaxKind.StaticKeyword -> "Static" |> toLongIdent
        | SyntaxKind.ReadOnlyKeyword -> "ReadOnly" |> toLongIdent
        | SyntaxKind.SealedKeyword -> "Sealed" |> toLongIdent
        | SyntaxKind.ConstKeyword -> "Const" |> toLongIdent
        | SyntaxKind.FixedKeyword -> "Fixed" |> toLongIdent
        | SyntaxKind.StackAllocKeyword -> "StackAlloc" |> toLongIdent
        | SyntaxKind.VolatileKeyword -> "Volatile" |> toLongIdent
        | SyntaxKind.NewKeyword -> "New" |> toLongIdent
        | SyntaxKind.OverrideKeyword -> "Override" |> toLongIdent
        | SyntaxKind.AbstractKeyword -> "Abstract" |> toLongIdent
        | SyntaxKind.VirtualKeyword -> "Virtual" |> toLongIdent
        | SyntaxKind.EventKeyword -> "Event" |> toLongIdent
        | SyntaxKind.ExternKeyword -> "Extern" |> toLongIdent
        | SyntaxKind.RefKeyword -> "Ref" |> toLongIdent
        | SyntaxKind.OutKeyword -> "Out" |> toLongIdent
        | SyntaxKind.InKeyword -> "In" |> toLongIdent
        | SyntaxKind.IsKeyword -> "Is" |> toLongIdent
        | SyntaxKind.AsKeyword -> "As" |> toLongIdent
        | SyntaxKind.ParamsKeyword -> "Params" |> toLongIdent
        | SyntaxKind.ArgListKeyword -> "ArgList" |> toLongIdent
        | SyntaxKind.MakeRefKeyword -> "MakeRef" |> toLongIdent
        | SyntaxKind.RefTypeKeyword -> "RefType" |> toLongIdent
        | SyntaxKind.RefValueKeyword -> "RefValue" |> toLongIdent
        | SyntaxKind.ThisKeyword -> "This" |> toLongIdent
        | SyntaxKind.BaseKeyword -> "Base" |> toLongIdent
        | SyntaxKind.NamespaceKeyword -> "Namespace" |> toLongIdent
        //| SyntaxKind.UsingKeyword -> "Using" |> toLongIdent
        | SyntaxKind.ClassKeyword -> "Class" |> toLongIdent
        | SyntaxKind.StructKeyword -> "Struct" |> toLongIdent
        | SyntaxKind.InterfaceKeyword -> "Interface" |> toLongIdent
        | SyntaxKind.EnumKeyword -> "Enum" |> toLongIdent
        | SyntaxKind.DelegateKeyword -> "Delegate" |> toLongIdent
        | SyntaxKind.CheckedKeyword -> "Checked" |> toLongIdent
        | SyntaxKind.UncheckedKeyword -> "Unchecked" |> toLongIdent
        | SyntaxKind.UnsafeKeyword -> "Unsafe" |> toLongIdent
        | SyntaxKind.OperatorKeyword -> "Operator" |> toLongIdent
        | SyntaxKind.ExplicitKeyword -> "Explicit" |> toLongIdent
        | SyntaxKind.ImplicitKeyword -> "Implicit" |> toLongIdent
        | SyntaxKind.YieldKeyword -> "Yield" |> toLongIdent
        | SyntaxKind.PartialKeyword -> "Partial" |> toLongIdent
        | SyntaxKind.AliasKeyword -> "Alias" |> toLongIdent
        | SyntaxKind.GlobalKeyword -> "Global" |> toLongIdent
        | SyntaxKind.AssemblyKeyword -> "Assembly" |> toLongIdent
        | SyntaxKind.ModuleKeyword -> "Module" |> toLongIdent
        | SyntaxKind.TypeKeyword -> "Type" |> toLongIdent
        | SyntaxKind.FieldKeyword -> "Field" |> toLongIdent
        | SyntaxKind.MethodKeyword -> "Method" |> toLongIdent
        | SyntaxKind.ParamKeyword -> "Param" |> toLongIdent
        | SyntaxKind.PropertyKeyword -> "Property" |> toLongIdent
        | SyntaxKind.TypeVarKeyword -> "TypeVar" |> toLongIdent
        | SyntaxKind.GetKeyword -> "Get" |> toLongIdent
        | SyntaxKind.SetKeyword -> "Set" |> toLongIdent
        | SyntaxKind.AddKeyword -> "Add" |> toLongIdent
        | SyntaxKind.RemoveKeyword -> "Remove" |> toLongIdent
        | SyntaxKind.WhereKeyword -> "Where" |> toLongIdent
        | SyntaxKind.FromKeyword -> "From" |> toLongIdent
        | SyntaxKind.GroupKeyword -> "Group" |> toLongIdent
        | SyntaxKind.JoinKeyword -> "Join" |> toLongIdent
        | SyntaxKind.IntoKeyword -> "Into" |> toLongIdent
        | SyntaxKind.LetKeyword -> "Let" |> toLongIdent
        | SyntaxKind.ByKeyword -> "By" |> toLongIdent
        | SyntaxKind.SelectKeyword -> "Select" |> toLongIdent
        | SyntaxKind.OrderByKeyword -> "OrderBy" |> toLongIdent
        | SyntaxKind.OnKeyword -> "On" |> toLongIdent
        | SyntaxKind.EqualsKeyword -> "Equals" |> toLongIdent
        | SyntaxKind.AscendingKeyword -> "Ascending" |> toLongIdent
        | SyntaxKind.DescendingKeyword -> "Descending" |> toLongIdent
        | SyntaxKind.NameOfKeyword -> "NameOf" |> toLongIdent
        | SyntaxKind.AsyncKeyword -> "Async" |> toLongIdent
        | SyntaxKind.AwaitKeyword -> "Await" |> toLongIdent
        | SyntaxKind.WhenKeyword -> "When" |> toLongIdent
        | SyntaxKind.ElifKeyword -> "Elif" |> toLongIdent
        | SyntaxKind.EndIfKeyword -> "EndIf" |> toLongIdent
        | SyntaxKind.RegionKeyword -> "Region" |> toLongIdent
        | SyntaxKind.EndRegionKeyword -> "EndRegion" |> toLongIdent
        | SyntaxKind.DefineKeyword -> "Define" |> toLongIdent
        | SyntaxKind.UndefKeyword -> "Undef" |> toLongIdent
        | SyntaxKind.WarningKeyword -> "Warning" |> toLongIdent
        | SyntaxKind.ErrorKeyword -> "Error" |> toLongIdent
        | SyntaxKind.LineKeyword -> "Line" |> toLongIdent
        | SyntaxKind.PragmaKeyword -> "Pragma" |> toLongIdent
        | SyntaxKind.HiddenKeyword -> "Hidden" |> toLongIdent
        | SyntaxKind.ChecksumKeyword -> "Checksum" |> toLongIdent
        | SyntaxKind.DisableKeyword -> "Disable" |> toLongIdent
        | SyntaxKind.RestoreKeyword -> "Restore" |> toLongIdent
        | SyntaxKind.ReferenceKeyword -> "Reference" |> toLongIdent
        | SyntaxKind.LoadKeyword -> "Load" |> toLongIdent
            //| SyntaxKind.CharacterLiteralToken -> Line ""
            //| _ -> node.ValueText |> Line

    static member ParseInterpolatedStringContentSyntax (node:InterpolatedStringContentSyntax) = 
        match node with 
        //| :? InterpolatedStringTextSyntax as x -> "-" + x.TextToken.Text |> Line
        | :? InterpolationSyntax as x -> x.Expression |> LineTransformer.ParseChsarpNode
        //| x -> x.ToFullString() |> Line

    static member ParseStatementSyntax (node:StatementSyntax) = 
        match node with 
        | :? BlockSyntax as x -> x.Statements  |> Seq.map LineTransformer.ParseStatementSyntax |> sequential
        | :? BreakStatementSyntax as x -> "BreakStatement" |> toLongIdent
        | :? CheckedStatementSyntax as x -> "CheckedStatement" |> toLongIdent
        | :? CommonForEachStatementSyntax as x -> "CommonForEachStatement" |> toLongIdent
        | :? ContinueStatementSyntax as x -> "ContinueStatement" |> toLongIdent
        | :? DoStatementSyntax as x -> "DoStatement" |> toLongIdent
        | :? EmptyStatementSyntax as x -> "EmptyStatement" |> toLongIdent
        | :? ExpressionStatementSyntax as x -> x.WithoutTrivia().ToFullString() |> toLongIdent // LineTransformer.ParseChsarpNode x.Expression
        | :? FixedStatementSyntax as x -> "FixedStatement" |> toLongIdent
        | :? ForStatementSyntax as x -> "ForStatement" |> toLongIdent
        | :? GotoStatementSyntax as x -> "GotoStatement" |> toLongIdent
        | :? IfStatementSyntax as x ->
            let condtion = LineTransformer.ParseChsarpNode x.Condition
            let statement = LineTransformer.ParseChsarpNode x.Statement
            let elseExpr = x.Else |>  Option.ofObj |> Option.map LineTransformer.ParseChsarpNode
            Expr.IfThenElse (condtion, statement, elseExpr, SequencePointInfoForBinding.SequencePointAtBinding range0, false)

        | :? LabeledStatementSyntax as x -> "LabeledStatement" |> toLongIdent
        | :? LocalDeclarationStatementSyntax as x -> 
            x.Declaration |> LineTransformer.ParseChsarpNode
        | :? LocalFunctionStatementSyntax as x -> "LocalFunctionStatement" |> toLongIdent
        | :? LockStatementSyntax as x -> "LockStatement" |> toLongIdent
        | :? ReturnStatementSyntax as x -> Expr.Const SynConst.Unit
        | :? SwitchStatementSyntax as x -> "SwitchStatement" |> toLongIdent
        | :? ThrowStatementSyntax as x -> "ThrowStatement" |> toLongIdent
        | :? TryStatementSyntax as x -> "TryStatement" |> toLongIdent
        | :? UnsafeStatementSyntax as x -> "UnsafeStatement" |> toLongIdent
        //| :? UsingStatementSyntax as x -> "UsingStatement" |> toLongIdent
        | :? WhileStatementSyntax as x -> "WhileStatement" |> toLongIdent
        | :? YieldStatementSyntax as x -> "YieldStatement" |> toLongIdent

    static member ParseChsarpNode (node:CSharpSyntaxNode):Expr = 

        //printfn "CSharpSyntaxNode"
        //node |> printfn "%A"
        //node.GetType() |> printfn "%A"
        //node.Kind() |> printfn "%A"
        //printfn "%s" <| node.ToString()
        //printfn "--\n"

        match node with 

        //| :? ArrowExpressionClauseSyntax as x -> LineTransformer.ParseChsarpNode x.Expression |> Line.prepend "-->"
        //| :? AssignmentExpressionSyntax as x -> LineTransformer.ParseAssignmentExpressionSyntax x

        //| :? AwaitExpressionSyntax as x -> Line.append (LineTransformer.ParseChsarpNode x.Expression) " |> Async.AwaitTask"
        //| :? ParenthesizedLambdaExpressionSyntax as x -> LineTransformer.ParseChsarpNode x.Body 
        //| :? LambdaExpressionSyntax as x -> LineTransformer.ParseChsarpNode x.Body 
        //| :? PrefixUnaryExpressionSyntax as x -> 
            //match x.Kind() with 
            //| SyntaxKind.LogicalNotExpression -> 
            //    Expr.App(ExprAtomicFlag.NonAtomic, false, PrettyNaming.CompileOpName "!" |> Expr.Ident, x.OperatorToken.ToFullString() |> Expr.Ident)

            //    //x.ToFullString().Replace("!", "not ") |> Line
            //| _ -> failwith "Unkown C# token: %s" (x.WithoutTrivia().ToFullString()) 

        //| :? AnonymousFunctionExpressionSyntax as x -> LineTransformer.ParseChsarpNode x.Body 
        //| :? ThrowExpressionSyntax as x -> LineTransformer.ParseChsarpNode x.Expression |> Line.prepend "raise " 
        //| :? CastExpressionSyntax as x -> Line.append (LineTransformer.ParseChsarpNode x.Expression) (" :?> " + x.Type.WithoutTrivia().ToFullString())
        //| :? InterpolatedStringContentSyntax as x -> x.WithoutTrivia().ToFullString().Replace("}", "").Replace("{","") |> Line
        //| :? InterpolatedStringExpressionSyntax as x -> 
        //    let builder = List.replicate (Seq.length x.Contents) "%A" |> String.concat " "
        //    let (Line values) = x.Contents |> Seq.map LineTransformer.ParseChsarpNode |> Line.concat " "
        //    ("sprintf \"" + builder + "\" " + values) |> Line

        //| :? InvocationExpressionSyntax as x -> 
            //let args =  
            //    x.ArgumentList.Arguments 
            //    |> Seq.map (fun x -> Expr.Ident <| x.ToFullString() )
            //    |> Seq.toList
            //    |> Expr.Tuple 

            //let name = x.Expression.WithoutTrivia().ToFullString() |> toIdent

            //Expr.App (ExprAtomicFlag.NonAtomic, false, 
                //Expr.LongIdent (false, LongIdentWithDots (name, [range0])), Expr.Paren args)

        //| :? VariableDeclaratorSyntax as x -> 

        //    let identifier = x.Identifier.ToFullString() |> Line
        //    let initlizer = LineTransformer.ParseChsarpNode x.Initializer.Value
        //    [identifier; initlizer] |> Line.concat " = "

        //| :? LocalDeclarationStatementSyntax as x -> 
        //    x.Declaration.Variables 
        //    |> Seq.map LineTransformer.ParseChsarpNode 
        //    |> Line.concat ""
        //    |> Line.prepend "let %s"

        //| :? ExpressionStatementSyntax as x -> 
        //| :? ReturnStatementSyntax as x -> 
        //    match x.Expression |> Option.ofObj with
        //    | Some x -> LineTransformer.ParseChsarpNode x
        //    | None ->  "return //TODO" |> Line



        //| :? ExpressionSyntax as x -> LineTransformer.ParseChsarpNode x. // TODO: This does not seem right. 
        //| :? LiteralExpressionSyntax as x -> x.Token |> LineTransformer.ParseToken

        | null -> Expr.Null
        | :? IdentifierNameSyntax as x -> 
            Expr.LongIdent (false, LongIdentWithDots (x.Identifier.WithoutTrivia().ToFullString() |> toIdent, [range0]))

        | :? AccessorDeclarationSyntax as x -> "AccessorDeclaration" |> toLongIdent
        | :? AccessorListSyntax as x -> "AccessorList" |> toLongIdent
        | :? AnonymousObjectMemberDeclaratorSyntax as x -> "AnonymousObjectMemberDeclarator" |> toLongIdent
        | :? ArgumentSyntax as x -> "Argument" |> toLongIdent
        | :? ArrayRankSpecifierSyntax as x -> "ArrayRankSpecifier" |> toLongIdent
        | :? ArrowExpressionClauseSyntax as x -> "ArrowExpressionClause" |> toLongIdent
        | :? AttributeArgumentListSyntax as x -> "AttributeArgumentList" |> toLongIdent
        | :? AttributeArgumentSyntax as x -> "AttributeArgument" |> toLongIdent
        | :? AttributeListSyntax as x -> "AttributeList" |> toLongIdent
        | :? AttributeSyntax as x -> "Attribute" |> toLongIdent
        | :? AttributeTargetSpecifierSyntax as x -> "AttributeTargetSpecifier" |> toLongIdent
        | :? BaseArgumentListSyntax as x -> "BaseArgumentList" |> toLongIdent
        | :? BaseCrefParameterListSyntax as x -> "BaseCrefParameterList" |> toLongIdent
        | :? BaseListSyntax as x -> "BaseList" |> toLongIdent
        | :? BaseParameterListSyntax as x -> "BaseParameterList" |> toLongIdent
        | :? BaseTypeSyntax as x -> "BaseType" |> toLongIdent
        | :? CatchClauseSyntax as x -> "CatchClause" |> toLongIdent
        | :? CatchDeclarationSyntax as x -> "CatchDeclaration" |> toLongIdent
        | :? CatchFilterClauseSyntax as x -> "CatchFilterClause" |> toLongIdent
        | :? CompilationUnitSyntax as x -> "CompilationUnit" |> toLongIdent
        | :? ConstructorInitializerSyntax as x -> "ConstructorInitializer" |> toLongIdent
        | :? CrefParameterSyntax as x -> "CrefParameter" |> toLongIdent
        | :? CrefSyntax as x -> "Cref" |> toLongIdent
        | :? ElseClauseSyntax as x -> "ElseClause" |> toLongIdent
        | :? EqualsValueClauseSyntax as x -> x.Value |> LineTransformer.ParseExpression
        | :? ExplicitInterfaceSpecifierSyntax as x -> "ExplicitInterfaceSpecifier" |> toLongIdent
        | :? ExpressionSyntax as x -> LineTransformer.ParseExpression x
        | :? ExternAliasDirectiveSyntax as x -> "ExternAliasDirective" |> toLongIdent
        | :? FinallyClauseSyntax as x -> "FinallyClause" |> toLongIdent
        | :? InterpolatedStringContentSyntax as x -> "InterpolatedStringContent" |> toLongIdent
        | :? InterpolationAlignmentClauseSyntax as x -> "InterpolationAlignmentClause" |> toLongIdent
        | :? InterpolationFormatClauseSyntax as x -> "InterpolationFormatClause" |> toLongIdent
        | :? JoinIntoClauseSyntax as x -> "JoinIntoClause" |> toLongIdent
        | :? MemberDeclarationSyntax as x -> "MemberDeclaration" |> toLongIdent
        | :? NameColonSyntax as x -> "NameColon" |> toLongIdent
        | :? NameEqualsSyntax as x -> "NameEquals" |> toLongIdent
        | :? OrderingSyntax as x -> "Ordering" |> toLongIdent
        | :? ParameterSyntax as x -> "Parameter" |> toLongIdent
        | :? PatternSyntax as x -> "Pattern" |> toLongIdent
        | :? QueryBodySyntax as x -> "QueryBody" |> toLongIdent
        | :? QueryClauseSyntax as x -> "QueryClause" |> toLongIdent
        | :? QueryContinuationSyntax as x -> "QueryContinuation" |> toLongIdent
        | :? SelectOrGroupClauseSyntax as x -> "SelectOrGroupClause" |> toLongIdent
        | :? StatementSyntax as x -> 
            x |> LineTransformer.ParseStatementSyntax
            //match 
            //    node.ChildNodes() 
            //    |> Seq.choose LineTransformer.ToCsharpSyntaxNode 
            //    |> Seq.map LineTransformer.ParseChsarpNode
            //    |> Seq.toList with 
            //| _::_::_ as xs -> 
            //    xs |> sequential
            //| x::[] -> x
            //| _ -> 
                //node.ChildTokens() 
                //|> Seq.map LineTransformer.ParseToken
                //|> Seq.toList 
                //|> sequential
        | :? StructuredTriviaSyntax as x -> "StructuredTrivia" |> toLongIdent
        | :? SwitchLabelSyntax as x -> "SwitchLabel" |> toLongIdent
        | :? SwitchSectionSyntax as x -> "SwitchSection" |> toLongIdent
        | :? TupleElementSyntax as x -> "TupleElement" |> toLongIdent
        | :? TypeArgumentListSyntax as x -> "TypeArgumentList" |> toLongIdent
        | :? TypeParameterConstraintClauseSyntax as x -> "TypeParameterConstraintClause" |> toLongIdent
        | :? TypeParameterConstraintSyntax as x -> "TypeParameterConstraint" |> toLongIdent
        | :? TypeParameterListSyntax as x -> "TypeParameterList" |> toLongIdent
        | :? TypeParameterSyntax as x -> "TypeParameter" |> toLongIdent
        //| :? UsingDirectiveSyntax as x -> 
            //x.Name
             //"UsingDirective" |> toLongIdent
        | :? VariableDeclarationSyntax as x -> 
     
            //x.TypeSyntax
            x.Variables 
            |> Seq.map LineTransformer.ParseChsarpNode 
            |> sequential
            
            //x.WithoutTrivia().ToFullString().Replace("var", "let") |> toLongIdent
        | :? VariableDeclaratorSyntax as x -> 

            let init = x.Initializer |> LineTransformer.ParseChsarpNode


            //Expr.Ident <| x.WithoutTrivia().ToFullString() // |> toIdent
            Expr.LetOrUse(
                false, false, 
                [LetBind(None, SynBindingKind.NormalBinding, false, true, [],
                    SynValData (
                        //Some {
                        //    MemberFlags.IsInstance = true
                        //    MemberFlags.IsDispatchSlot = false 
                        //    MemberFlags.IsOverrideOrExplicitImpl = false 
                        //    MemberFlags.IsFinal = false
                        //    MemberFlags.MemberKind = MemberKind.Member
                        //}
                        None, SynValInfo ([], SynArgInfo ([], false, None )), None), 
                    Pat.Named (Pat.Wild, Ident(x.Identifier.ValueText, range0), false, None), init)], Expr.InLetPlaceholder)

        | :? VariableDesignationSyntax as x -> "VariableDesignation" |> toLongIdent
        | :? WhenClauseSyntax as x -> "WhenClause" |> toLongIdent
        | :? XmlAttributeSyntax as x -> "XmlAttribute" |> toLongIdent
        | :? XmlElementEndTagSyntax as x -> "XmlElementEndTag" |> toLongIdent
        | :? XmlElementStartTagSyntax as x -> "XmlElementStartTag" |> toLongIdent
        | :? XmlNameSyntax as x -> "XmlName" |> toLongIdent
        | :? XmlNodeSyntax as x -> "XmlNode" |> toLongIdent
        | :? XmlPrefixSyntax as x -> "XmlPrefix" |> toLongIdent

        | x -> 
            x.WithoutTrivia().ToFullString() |> Expr.Ident 
            ////printfn "CSharpSyntaxNode"
            ////x |> printfn "%A"
            ////x.GetType() |> printfn "%A"
            ////x.Kind() |> printfn "%A"
            ////printfn "--\n"

            //x.WithoutTrivia().ToFullString() |> Line

    static member ParseExpression (node:ExpressionSyntax) = 
        match node with 
        //| :? AnonymousFunctionExpressionSyntax as x -> ()
        //| :? AnonymousObjectCreationExpressionSyntax as x -> ()
        //| :? ArrayCreationExpressionSyntax as x -> ()
        | :? AssignmentExpressionSyntax as x -> x |> LineTransformer.ParseAssignmentExpressionSyntax
        //| :? AwaitExpressionSyntax as x -> ()
        | :? BinaryExpressionSyntax as x -> x |> LineTransformer.ParseBinaryExpresson
        //| :? CastExpressionSyntax as x -> ()
        //| :? CheckedExpressionSyntax as x -> ()
        //| :? ConditionalAccessExpressionSyntax as x -> ()
        //| :? ConditionalExpressionSyntax as x -> ()
        //| :? DeclarationExpressionSyntax as x -> ()
        //| :? DefaultExpressionSyntax as x -> ()
        //| :? ElementAccessExpressionSyntax as x -> ()
        //| :? ElementBindingExpressionSyntax as x -> ()
        //| :? ImplicitArrayCreationExpressionSyntax as x -> ()
        //| :? ImplicitElementAccessSyntax as x -> ()
        //| :? ImplicitStackAllocArrayCreationExpressionSyntax as x -> ()
        //| :? InitializerExpressionSyntax as x -> ()
        //| :? InstanceExpressionSyntax as x -> ()
        //| :? InterpolatedStringExpressionSyntax as x -> ()
        | :? InvocationExpressionSyntax as x -> 
            let args =  
                x.ArgumentList.Arguments 
                |> Seq.map (fun x -> Expr.Ident <| x.ToFullString() )
                |> Seq.toList
                |> Expr.Tuple 

            let name = x.Expression.WithoutTrivia().ToFullString() |> toIdent

            Expr.App (ExprAtomicFlag.NonAtomic, false, 
                Expr.LongIdent (false, LongIdentWithDots (name, [range0])), Expr.Paren args)
        //| :? IsPatternExpressionSyntax as x -> ()
        | :? LiteralExpressionSyntax as x -> x.Token |> LineTransformer.ParseToken
        //| :? MakeRefExpressionSyntax as x -> ()
        | :? MemberAccessExpressionSyntax as x -> x.WithoutTrivia().ToFullString() |> toLongIdent
        //| :? MemberBindingExpressionSyntax as x -> ()
        //| :? ObjectCreationExpressionSyntax as x -> ()
        //| :? OmittedArraySizeExpressionSyntax as x -> ()
        //| :? ParenthesizedExpressionSyntax as x -> ()
        //| :? PostfixUnaryExpressionSyntax as x -> ()
        | :? PrefixUnaryExpressionSyntax as x -> 
            match x.Kind() with 
            | SyntaxKind.LogicalNotExpression -> 
                Expr.App(ExprAtomicFlag.NonAtomic, false, PrettyNaming.CompileOpName "!" |> Expr.Ident, x.OperatorToken.ToFullString() |> Expr.Ident)

                //x.ToFullString().Replace("!", "not ") |> Line
            | _ -> sprintf "Unkown C# token: %s" (x.WithoutTrivia().ToFullString()) |> failwith
        //| :? QueryExpressionSyntax as x -> ()
        //| :? RefExpressionSyntax as x -> ()
        //| :? RefTypeExpressionSyntax as x -> ()
        //| :? RefValueExpressionSyntax as x -> ()
        //| :? SizeOfExpressionSyntax as x -> ()
        //| :? StackAllocArrayCreationExpressionSyntax as x -> ()
        //| :? ThrowExpressionSyntax as x -> ()
        //| :? TupleExpressionSyntax as x -> ()
        //| :? TypeOfExpressionSyntax as x -> ()
        //| :? TypeSyntax as x -> ()

    static member ParseNodeOrToken(node:SyntaxNodeOrToken): Expr = 
   
        if node.IsToken then 
            node.AsToken() |> LineTransformer.ParseToken
        else 
            node.AsNode() 
            |> LineTransformer.ToCsharpSyntaxNode
            |> Option.map (fun x -> LineTransformer.ParseChsarpNode x)
            |> function 
                | Some x -> x
                | None -> failwith "VB is not supported"
    


type FileContentsDumper() = 
    inherit CSharpSyntaxWalker(SyntaxWalkerDepth.Token)
    
    member this.VisitNamespaceDeclaration (node:NamespaceDeclarationSyntax ) = 

        let classes = 
            node.ChildNodes().OfType<ClassDeclarationSyntax>()
            |> Seq.map this.VisitClassDeclaration
            |> Seq.toList

        let interfaces = 
            node.ChildNodes().OfType<InterfaceDeclarationSyntax>()
            |> Seq.map this.VisitInterfaceDeclaration
            |> Seq.toList
            
        {
            Namespace.Name = node.Name.WithoutTrivia().ToFullString()
            Namespace.Classes = classes
            Namespace.Interfaces = interfaces
        } 


    member this.VisitInterfaceDeclaration(node:InterfaceDeclarationSyntax) =    

        let members = 
            node.Members |> Seq.map (fun x -> x.ToFullString()) |> Seq.toList

        {
            Interface.Name = node.Identifier.WithoutTrivia().Text
            Methods = members
        }

    member this.VisitClassDeclaration(node:ClassDeclarationSyntax ) =

        let attrs = 
            node.AttributeLists
            |> Seq.map (fun x -> 
                x.Attributes |> Seq.map (fun x -> 
                    let args = 
                        x.ArgumentList
                        |> Option.ofObj 

                        |> Option.map (fun x -> x.ToFullString())
                        //|> Option.map (fun x -> x |> Seq.map (fun x -> 
                        //    {
                        //        Parameter.Name = x.NameColon.Name
                        //        Parameter.Type = x.Expression
                        //    }
                        //))

                    {
                        Attribute.Name = x.Name.WithoutTrivia().ToFullString()
                        Attribute.Parameters = args }
                    ))
            |> Seq.concat
            |> Seq.toList

        let typeParameters = 
            node.TypeParameterList 
            |> Option.ofObj 
            |> Option.map (fun x -> 
                x.Parameters 
                |> Seq.map (fun x -> x.Identifier.WithoutTrivia().Text) 
                |> Seq.toList )
            |> Option.toList
            |> List.concat

        let baseTypes = 
            node.BaseList 
            |> Option.ofObj 
            |> Option.bind (fun x -> 
                x.Types 
                |> Seq.map (fun x -> x.WithoutTrivia().ToFullString())
                |> Seq.toList
                |> function 
                | [] -> None
                | x::xs -> Some (x, xs) )

        let ctors = 
            node.ChildNodes().OfType<ConstructorDeclarationSyntax>()
            |> Seq.map this.VisitConstructorDeclaration
            |> Seq.toList

        let fields = 
            node.ChildNodes().OfType<FieldDeclarationSyntax>()
            |> Seq.map this.VisitFieldDeclaration
            |> Seq.concat
            |> Seq.toList

        let methods = 
            node.ChildNodes().OfType<MethodDeclarationSyntax>()
            |> Seq.map this.VisitMethodDeclaration
            |> Seq.toList

        let properties = 
            node.ChildNodes().OfType<PropertyDeclarationSyntax>()
            |> Seq.map this.VisitPropertyDeclaration
            |> Seq.toList
            
        {
            Name = { ClassName.Name = node.Identifier.ValueText; Generics = [] }
            ImplementInterfaces = baseTypes |> Option.map (snd) |> Option.toList |> List.concat
            BaseClass = baseTypes |> Option.map (fst)
            Constructors = ctors
            Fields = fields
            Methods = methods
            Properties = properties
            TypeParameters = typeParameters
            Attributes = attrs  }


    member this.VisitConstructorDeclaration (node:ConstructorDeclarationSyntax) = 
        {
            Ctor.Body = Line "" // TODO
                //node.Body.Statements 
                //|> Seq.map (fun x -> x.WithoutTrailingTrivia().WithoutLeadingTrivia().ToFullString().Replace(";", ""))
                //|> Seq.map Line
                //|> Seq.toList

            Ctor.Parameters = 
                node.ParameterList.Parameters 
                |> Seq.map (fun x -> 
                    {Parameter.Name = x.Identifier.WithoutTrivia().Text; Type = x.Type.WithoutTrivia().ToFullString() })
                |> Seq.toList

            SubclassArgs = 
                node.Initializer |> Option.ofObj 
                |> Option.bind (fun x -> Option.ofObj x.ArgumentList)
                |> Option.map (fun x -> x.Arguments)
                |> Option.map (fun x -> 
                    x |> Seq.map (fun x -> x.WithoutTrivia().ToFullString()) |> Seq.toList)
                |> Option.toList
                |> List.concat
        }

    member this.VisitMethodDeclaration(node:MethodDeclarationSyntax ) = 
        let isPrivate = 
            node.Modifiers |> Seq.exists (fun x -> x.Kind() = SyntaxKind.PrivateKeyword)

        {
            Method.Name = node.Identifier.WithoutTrivia().Text
            Method.IsVirtual = node.Modifiers |> Seq.exists (fun x -> x.Kind() = SyntaxKind.VirtualKeyword )
            Method.IsAsync = node.Modifiers |> Seq.exists (fun x -> x.Kind() = SyntaxKind.AsyncKeyword )
            Method.IsPrivate = isPrivate
            Method.IsOverride = node.Modifiers |> Seq.exists (fun x -> x.Kind() = SyntaxKind.OverrideKeyword )

            Method.ReturnType = node.ReturnType.WithoutTrivia().ToFullString()
            Method.Parameters = 
                node.ParameterList.Parameters 
                |> Seq.map (fun x -> 
                    {Parameter.Name = x.Identifier.WithoutTrivia().Text; Type = x.Type.WithoutTrivia().ToFullString() })
                |> Seq.toList
            Method.Body =
                node.Body |> Option.ofObj |> Option.map (fun x -> 
                    x.Statements 
                    |> Seq.map LineTransformer.ParseChsarpNode
                    |> sequential  )
                |> function 
                | Some x -> x
                | None -> Expr.Const SynConst.Unit

            Method.Accessibility = if isPrivate then Some SynAccess.Private else None
        }

    member this.VisitFieldDeclaration (node:FieldDeclarationSyntax): Field seq = 

        let isPublic = node.Modifiers |> Seq.exists (fun x -> x.Kind() = SyntaxKind.PublicKeyword)

        node.Declaration.Variables
        |> Seq.map (fun x -> 
            {
                Field.Name =  x.Identifier.WithoutTrivia().ToFullString()
                Field.Type = node.Declaration.Type.WithoutTrivia().ToFullString()
                Field.IsPublic = isPublic
                Field.Initilizer = x.Initializer |> Option.ofObj |> Option.map (fun x -> x.Value |> LineTransformer.ParseExpression )
            })


    member this.VisitPropertyDeclaration (node:PropertyDeclarationSyntax) = 

        let parseAccessorDeclaration (node:AccessorDeclarationSyntax) = 

            let expression = node.ExpressionBody |> Option.ofObj
            let statement = node.Body |> Option.ofObj 

            match expression, statement with 
            | None, None -> []
            | Some e, None -> [LineTransformer.ParseChsarpNode e]
            | None, Some s -> s.Statements |> Seq.map LineTransformer.ParseChsarpNode |> Seq.toList
            | Some e, Some s -> [LineTransformer.ParseChsarpNode e;] @ (s.Statements |> Seq.map LineTransformer.ParseChsarpNode |> Seq.toList)

        let processAccessorForAccessorType accessor = 
            node.AccessorList.Accessors 
            |> Seq.filter (fun x -> x.Kind() = accessor)
            |> Seq.map parseAccessorDeclaration
            |> Seq.toList
            |> List.concat
            |> function
            | _::_::_ as xs -> xs |> Seq.reduce (fun a b -> Expr.Sequential (SequencePointsAtSeq, true, a,b)) |> Some
            | x::[] -> x |> Some
            | _ -> None

        let getStatements = processAccessorForAccessorType SyntaxKind.GetAccessorDeclaration
        let setStatements =  processAccessorForAccessorType SyntaxKind.SetAccessorDeclaration

        let access = 
            match node.Modifiers |> Seq.map (fun x -> x.WithoutTrivia().ToFullString()) |> Seq.toList with 
            | ["internal"] -> Some SynAccess.Internal
            | ["private"] -> Some SynAccess.Private
            | _  -> None
            
        {
            Prop.Name = node.Identifier.WithoutTrivia().Text
            Type = node.Type.WithoutTrivia().ToFullString()
            Prop.Get = getStatements 
            Prop.Set = setStatements
            Access = access
        }

    member this.VisitUsingDirective (node:UsingDirectiveSyntax ) = 
        node.ChildNodes() |> ignore

        { Namespace = node.Name.WithoutTrivia().ToFullString() }


    member this.ParseSyntax tree (x: SyntaxNode) = 

        let result = 
            match x with
            | :? UsingDirectiveSyntax as x -> x |> this.VisitUsingDirective |> UsingStatement
            | :? NamespaceDeclarationSyntax as x -> x |> this.VisitNamespaceDeclaration |> Namespace
            | :? MethodDeclarationSyntax as x -> x |> this.VisitMethodDeclaration |> Method
            | :? InterfaceDeclarationSyntax as x -> x |> this.VisitInterfaceDeclaration |> Interface
            | :? ClassDeclarationSyntax as x -> x |> this.VisitClassDeclaration |> Class
            | :? FieldDeclarationSyntax as x -> x |> this.VisitFieldDeclaration |> Field
            | :? PropertyDeclarationSyntax as x -> x |> this.VisitPropertyDeclaration |> Prop
            | x -> printfn "Skipping element: %A" <| x.Kind(); Empty

        match tree with 
        | None -> result |> Some
        | Some tree -> 
            match tree, result with 
            | Empty, x -> x |> Some
            | File f, File _ -> failwith "Meging files, not implemented"
            | File f, Interface i -> failwith "Meging interface with file, not implemented"
            | File f, Class c -> failwith "Meging class with file, not implemented"
            | File f, Field field -> failwith "Meging files, not implemented"
            | File f, Prop p -> failwith "Meging files, not implemented"
            | File f, Method m -> failwith "Meging files, not implemented"
            | File f, Namespace ns -> 
                {f with Namespaces = ns :: f.Namespaces} |> File |> Some
            | File f, UsingStatement m -> { f with UsingStatements = m :: f.UsingStatements } |> File |> Some
            | UsingStatement i, UsingStatement i2 -> 
                {
                    UsingStatements = [i;i2]
                    Namespaces = []
                } |> File |> Some
            | UsingStatement i, Namespace ns -> 
                {
                    UsingStatements = [i]
                    Namespaces = [ns]
                } |> File |> Some
            | Field f, Field fPrime -> 
                seq {
                    yield! f
                    yield! fPrime
                } |> Field |> Some
            | _, _ -> sprintf "C# not supported: %A, %A" tree result |> failwith