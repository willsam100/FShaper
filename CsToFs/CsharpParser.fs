namespace CsToFs

open Microsoft.FSharp.Compiler.Ast
open Microsoft.CodeAnalysis
open System.Linq
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax
open CsToFs
open Microsoft.FSharp.Compiler

type LineTransformer() = 

    static member ToCsharpSyntaxNode (node:SyntaxNode) =
        match node with
        | :? CSharpSyntaxNode as x -> Some x
        | _ -> None

    static member ParseAssignmentExpressionSyntax (node:AssignmentExpressionSyntax) = 

        match node.Kind() with  
        | SyntaxKind.SimpleAssignmentExpression -> 
            let left = 
                node.Left.ChildNodesAndTokens() 
                |> Seq.map LineTransformer.ParseNodeOrToken
                |> Seq.toList
                |> Line.concat ""

            let right = 
                let r = 
                    node.Right.ChildNodesAndTokens() 
                    |> Seq.map LineTransformer.ParseNodeOrToken

                let rt = 
                    node.Right.ChildNodesAndTokens() 
                    |> Seq.map (fun x -> x.GetLeadingTrivia(), x.GetTrailingTrivia())
                Seq.zip rt r 
                |> Seq.map (fun ((lead, trail), (Line x)) -> lead.ToFullString() + x + trail.ToFullString())
                |> String.concat ""
                |> Line
                
            [left;right] |> Line.concat " <- "


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
                        node.Right.ChildNodesAndTokens() 
                        |> Seq.filter (fun x -> x.Kind() <> SyntaxKind.ParameterList) 
                        |> Seq.map LineTransformer.ParseNodeOrToken
                        |> Line.concat " "

                    [paramList; otherNodes] |> Line.concat " "
                else 
                    node.Right.ChildNodesAndTokens() 
                    |> Seq.map LineTransformer.ParseNodeOrToken
                    |> Line.concat " "

            let left = 
                node.Left.ChildNodesAndTokens() 
                |> Seq.map LineTransformer.ParseNodeOrToken
                |> Line.concat ""

            if isPlusEquals then 
                let (Line left) = left
                let (Line right) = right
                sprintf "%s.AddHandler(new EventHandler<_>(fun %s ))" left right |> Line
            else
                let (Line right) = right
                let (Line left) = left
                sprintf "%s %s" left right |> Line
        | _ -> node.WithoutTrivia().ToFullString() |> Line

    static member ParseBinaryExpresson (node:BinaryExpressionSyntax):Expr = 

        let createLogicalExpression isInfix join = 

            let left = 
                match node.Left with 
                | :? BinaryExpressionSyntax as x -> LineTransformer.ParseBinaryExpresson x 
                | _ -> LineTransformer.ParseChsarpNode node.Left

            let right = 
                match node.Right with 
                | :? BinaryExpressionSyntax as x -> LineTransformer.ParseBinaryExpresson x 
                | _ -> LineTransformer.ParseChsarpNode node.Right
                
            SynExpr.App (isInfix, left, right) 

        match node.Kind() with 
        | SyntaxKind.LogicalAndExpression -> 
            PrettyNaming.CompileOpName "&&" |> createLogicalExpression true
        | SyntaxKind.LogicalOrExpression -> PrettyNaming.CompileOpName "||" |> createLogicalExpression true
        | SyntaxKind.NotEqualsExpression -> PrettyNaming.CompileOpName "!" |> createLogicalExpression false
        | SyntaxKind.EqualsExpression -> PrettyNaming.CompileOpName "=" |> createLogicalExpression true
        | _ -> 
            printfn "Binary: %A" <| node.Kind();  
            node.ToFullString() |> Line

    static member ParseToken (node:SyntaxToken) = 

        match node.Kind() with 
        | SyntaxKind.ExclamationToken -> Line "not"
        | SyntaxKind.EqualsGreaterThanToken -> Line "->"
        | SyntaxKind.CharacterLiteralToken -> Line ""
        | _ -> node.ValueText |> Line

    static member ParseInterpolatedStringContentSyntax (node:InterpolatedStringContentSyntax) = 
        match node with 
        | :? InterpolatedStringTextSyntax as x -> "-" + x.TextToken.Text |> Line
        | :? InterpolationSyntax as x -> x.Expression |> LineTransformer.ParseChsarpNode
        | x -> x.ToFullString() |> Line

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
        | :? PrefixUnaryExpressionSyntax as x -> 
            match x.Kind() with 
            | SyntaxKind.LogicalNotExpression -> 
                App(false, Expr.Ident PrettyNaming.CompileOpName "!", x.OperatorToken.ToFullString() |> Expr.Ident )

                //x.ToFullString().Replace("!", "not ") |> Line
            | _ -> x.WithoutTrivia().ToFullString() |> Line

        //| :? AnonymousFunctionExpressionSyntax as x -> LineTransformer.ParseChsarpNode x.Body 
        //| :? ThrowExpressionSyntax as x -> LineTransformer.ParseChsarpNode x.Expression |> Line.prepend "raise " 
        //| :? CastExpressionSyntax as x -> Line.append (LineTransformer.ParseChsarpNode x.Expression) (" :?> " + x.Type.WithoutTrivia().ToFullString())
        //| :? InterpolatedStringContentSyntax as x -> x.WithoutTrivia().ToFullString().Replace("}", "").Replace("{","") |> Line
        //| :? InterpolatedStringExpressionSyntax as x -> 
        //    let builder = List.replicate (Seq.length x.Contents) "%A" |> String.concat " "
        //    let (Line values) = x.Contents |> Seq.map LineTransformer.ParseChsarpNode |> Line.concat " "
        //    ("sprintf \"" + builder + "\" " + values) |> Line

        //| :? InvocationExpressionSyntax as x -> x.WithoutTrivia().ToFullString() |> Line

        //| :? VariableDeclaratorSyntax as x -> 

        //    let identifier = x.Identifier.ToFullString() |> Line
        //    let initlizer = LineTransformer.ParseChsarpNode x.Initializer.Value
        //    [identifier; initlizer] |> Line.concat " = "

        //| :? LocalDeclarationStatementSyntax as x -> 
        //    x.Declaration.Variables 
        //    |> Seq.map LineTransformer.ParseChsarpNode 
        //    |> Line.concat ""
        //    |> Line.prepend "let %s"

        //| :? ExpressionStatementSyntax as x -> LineTransformer.ParseChsarpNode x.Expression
        //| :? ReturnStatementSyntax as x -> 
        //    match x.Expression |> Option.ofObj with
        //    | Some x -> LineTransformer.ParseChsarpNode x
        //    | None ->  "return //TODO" |> Line
        //| :? BlockSyntax as x -> x.Statements |> Seq.map LineTransformer.ParseChsarpNode |> Line.concat ";"
        //| :? IfStatementSyntax as x -> 

        //    let addIndentationToLines (Line lines) = 
        //        let indent = sprintf "%s    " (x.IfKeyword.LeadingTrivia.ToString())
        //        if lines.Contains "\n" then
        //            lines |> (fun s -> indent + s.Replace(";\n", "\n" + indent))
        //        else
        //            lines |> (fun s -> indent + s.Replace(";", "\n" + indent))

        //    let (Line condtion) = LineTransformer.ParseChsarpNode x.Condition
        //    let statement = LineTransformer.ParseChsarpNode x.Statement |> addIndentationToLines
            
        //    match Option.ofObj x.Else with 
        //    | Some elseBlock -> 
        //        let elseBlock = LineTransformer.ParseChsarpNode elseBlock |> addIndentationToLines
        //        sprintf "if %s then\n%s\nelse\n%s" condtion statement elseBlock |> Line
        //    | None -> 

        //        //Expr.IfThenElse (condtion, )

        //        sprintf "if %s then\n%s" condtion statement |> Line




        //| :? BinaryExpressionSyntax as x -> LineTransformer.ParseBinaryExpresson x
        //| :? StatementSyntax as x -> 
        //    node.ChildNodesAndTokens() 
        //    |> Seq.map LineTransformer.ParseNodeOrToken
        //    |> Line.concat ""
        ////| :? ExpressionSyntax as x -> LineTransformer.ParseChsarpNode x // TODO: This does not seem right. 
        //| null -> Line "null"
        //| x -> 
            ////printfn "CSharpSyntaxNode"
            ////x |> printfn "%A"
            ////x.GetType() |> printfn "%A"
            ////x.Kind() |> printfn "%A"
            ////printfn "--\n"

            //x.WithoutTrivia().ToFullString() |> Line

    static member ParseNodeOrToken(node:SyntaxNodeOrToken) = 
   
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
            Method.Body = Line "" // TODO
                //node.Body |> Option.ofObj |> Option.map (fun x -> 
                //    x.Statements 
                //    |> Seq.map LineTransformer.ParseChsarpNode
                //    //|> Seq.map (fun x -> 
                //        //FSharpExpr

                //        //)
                //    |> Seq.toList )
                //|> Option.toList
                //|> List.concat

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
                Field.Initilizer = x.Initializer |> Option.ofObj |> Option.map (fun x -> x.Value.WithoutTrivia().ToFullString())
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

        let getStatements = processAccessorForAccessorType SyntaxKind.GetAccessorDeclaration
        let setStatements =  processAccessorForAccessorType SyntaxKind.SetAccessorDeclaration

        {
            Prop.Name = node.Identifier.WithoutTrivia().Text
            Type = node.Type.WithoutTrivia().ToFullString()
            Prop.Get = getStatements |> Seq.head // TODO
            Prop.Set = setStatements |> Seq.head // TODO
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