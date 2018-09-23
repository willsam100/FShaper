// Learn more about F# at http://fsharp.org

open System
open System.Text
open Microsoft.CodeAnalysis;
open System.Collections.Generic
open System.Linq
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax
open Microsoft.CodeAnalysis.Text
open System.Linq

type Parameter = {
    Type:string
    Name:string
}

type Ctor = {
    Body:string list
    Parameters: Parameter list
    SubclassArgs: string list
}

type Method = {
    Name:string
    Parameters:Parameter list
    Body: string list
    ReturnType:string
    IsVirtual:bool
    IsAsync:bool
    IsPrivate: bool
    IsOverride:bool
}

type Prop = {
    Name:string
    Get: (string list)
    Set: (string list)
}

type ClassName = {
    Name:string
    Generics:string list
}

type Field = {
    IsPublic: bool // this syntax is not supported in F# 
    Name:string
    Type: string
    Initilizer:string option
}

type Attribute = {
    Name:string
    Parameters:string option
}

type Class = {
    Name:ClassName
    Constructors:Ctor list
    Fields: Field list
    Methods: Method list
    Properties: Prop list
    Attributes: Attribute list
    BaseClass: string option
    ImplementInterfaces: string list
    TypeParameters:string list
}

type Interface = {
    Name:string
    // TDOO Generics
    Methods:string list
}

type UsingStatement = {
    Namespace:string
}

type Namespace = {
    Name:string
    Interfaces: Interface list
    Classes: Class list
}



type File = {
    UsingStatements:UsingStatement list
    Namespaces:Namespace list
}

type FsharpSyntax = 
    | File of File
    | UsingStatement of UsingStatement
    | Namespace of Namespace
    | Interface of Interface
    | Class of Class
    | Field of Field seq
    | Prop of Prop
    | Method of Method
    | Empty

type FileContentsDumper() = 
    inherit CSharpSyntaxWalker(SyntaxWalkerDepth.Token)

    let rec parseExpressSyntax (node:ExpressionSyntax) = 
        //node.ChildNodes() |> Seq.iter (fun x -> 
        //    x |> printfn "%A"
        //    x.GetType() |> printfn "%A"
        //    x.Kind() |> printfn "%A")

        //node.ChildNodes() |> Seq.map (fun x -> 
            //match x with 
            //| :? AssignmentExpressionSyntax 

            //) 

        match node with 
        | :? InvocationExpressionSyntax as x -> sprintf "%s" <| x.WithoutTrivia().ToFullString() 
        | :? AssignmentExpressionSyntax as x -> 
            let result = x.WithoutTrivia().ToFullString()

            match x.Kind() with  
            | SyntaxKind.SimpleAssignmentExpression ->  result.Replace("=", "<-")
            | SyntaxKind.AddAssignmentExpression -> 

                let isPlusEquals = 
                    x.ChildTokens() |> Seq.exists (fun x -> x.Kind() = SyntaxKind.PlusEqualsToken)

                let right =
                    if isPlusEquals then 
                        let paramList = 
                            x.Right.ChildNodes() 
                            |> Seq.filter (fun x -> x.Kind() = SyntaxKind.ParameterList) 
                            |> Seq.map (fun x -> x.ChildNodes() |> Seq.last)
                            |> Seq.map (fun x -> x.WithoutTrivia().ToFullString())
                            |> String.concat ""

                        let otherNodes = 
                            x.Right.ChildNodes() 
                            |> Seq.filter (fun x -> x.Kind() <> SyntaxKind.ParameterList) 
                            |> Seq.map (fun x -> 
                                match x with 
                                | :? AssignmentExpressionSyntax as x -> parseExpressSyntax x
                                | _ -> x.WithoutTrivia().ToFullString()) 

                                |> String.concat ";"

                        sprintf "%s -> %s" paramList otherNodes
                    else 
                        x.Right.ChildNodes() 
                        |> Seq.map (fun x -> 
                            match x with 
                            | :? AssignmentExpressionSyntax as x -> parseExpressSyntax x
                            | _ -> x.WithoutTrivia().ToFullString()) 

                            |> String.concat ";"

                let left = 
                    x.Left.ChildNodes() |> Seq.map (fun x -> 
                        match x with 
                        | :? AssignmentExpressionSyntax as x -> parseExpressSyntax x
                        | _ -> x.WithoutTrivia().ToFullString()) |> String.concat "."

                if isPlusEquals then 
                    sprintf "%s.Add (fun %s )" left right
                else sprintf "%s <- %s" left right

                //result.Replace(" +=", ".Add (fun")
            | _ -> result
        | _ -> node.WithoutTrivia().ToFullString()

    let parseExpressionStatement (node:ExpressionStatementSyntax) = 
        
        //printfn "%A" <| node.Expression.Kind()
        //printfn "%A" <| node.Expression.GetType()
        parseExpressSyntax node.Expression



    let parseLocalDeclarationStatement (node:LocalDeclarationStatementSyntax) = 
        //match node.Declaration with 
        //| :? VariableDeclaratorSyntax as x -> parseVaribleDeclaration x
        //| x -> printfn "%A" <| x.GetType(); x.ToFullString()
        sprintf "let %s" <| node.Declaration.Variables.ToFullString()


    let parseStatements (node:StatementSyntax) = 
        if node.Kind() = SyntaxKind.ReturnStatement then 
            node.ChildTokens() |> printfn "%A"
            node.ToFullString().Replace("return ", "")
        else 
            match node with 
            | :? LocalDeclarationStatementSyntax as x -> parseLocalDeclarationStatement x
            | :? ExpressionStatementSyntax as x -> parseExpressionStatement x
            | x -> printfn "%A" <| x.GetType(); x.ToFullString()


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
            Ctor.Body = 
                node.Body.Statements 
                |> Seq.map (fun x -> x.WithoutTrailingTrivia().WithoutLeadingTrivia().ToFullString().Replace(";", ""))
                |> Seq.toList

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
        {
            Method.Name = node.Identifier.WithoutTrivia().Text
            Method.IsVirtual = node.Modifiers |> Seq.exists (fun x -> x.Kind() = SyntaxKind.VirtualKeyword )
            Method.IsAsync = node.Modifiers |> Seq.exists (fun x -> x.Kind() = SyntaxKind.AsyncKeyword )
            Method.IsPrivate = node.Modifiers |> Seq.exists (fun x -> x.Kind() = SyntaxKind.PrivateKeyword)
            Method.IsOverride = node.Modifiers |> Seq.exists (fun x -> x.Kind() = SyntaxKind.OverrideKeyword )

            Method.ReturnType = node.ReturnType.WithoutTrivia().ToFullString()
            Method.Parameters = 
                node.ParameterList.Parameters 
                |> Seq.map (fun x -> 
                    {Parameter.Name = x.Identifier.WithoutTrivia().Text; Type = x.Type.WithoutTrivia().ToFullString() })
                |> Seq.toList
            Method.Body = 
                node.Body.Statements 
                |> Seq.map parseStatements
                //|> Seq.map (fun x -> x.WithoutTrailingTrivia().WithoutLeadingTrivia().ToFullString().Replace(";", ""))
                |> Seq.toList
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
            node.Body 
            |> Option.ofObj 
            |> Option.map (fun x -> x.Statements |> Seq.map parseStatements |> Seq.toList)
            |> Option.toList
            |> List.concat 

        let getStatements = 
            node.AccessorList.Accessors 
            |> Seq.filter (fun x -> x.Kind() = SyntaxKind.GetAccessorDeclaration)
            |> Seq.map parseAccessorDeclaration
            |> Seq.toList
            |> List.concat

        printfn "Setters"
        let setStatements = 
            node.AccessorList.Accessors 
            |> Seq.filter (fun x -> x.Kind() = SyntaxKind.SetAccessorDeclaration)
            |> Seq.map parseAccessorDeclaration
            |> Seq.toList
            |> List.concat

        {
            Prop.Name = node.Identifier.WithoutTrivia().Text
            Prop.Get = getStatements
            Prop.Set = setStatements
        }

    //member this.VisitAttribute (node:AttributeSyntax) = 


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
            //| :? AttributeSyntax as x -> x |> this.VisitAtt |> Prop
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
            | File f, Namespace ns -> {f with Namespaces = ns :: f.Namespaces} |> File |> Some
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
            | _, _ -> sprintf "C# not supported: %A, %A" tree result |> failwith


module ProgramPrinter = 

    let private addIndents ind xs = xs |> List.map (fun x -> ind + x)
    let private addIndentsLvl2 ind xs = xs |> List.map (fun x -> ind + ind + x)
    let private addIndentsLvl3 ind xs = xs |> List.map (fun x -> ind + ind + ind + x)

    let printParameters (xs: Parameter list) = 
        match xs with 
        | [] -> "()"
        | xs -> 
            let args = xs |> List.map (fun x -> x.Name + ":" + x.Type) |> String.concat ","
            "(" + args + ")"


    let printMethod indent (m:Method) = 

        let args = m.Parameters |> printParameters
        
        let prefix = 
            match m.IsOverride, m.IsPrivate with 
            | true, _ -> "override this."
            | false, true -> "member private this."
            | false, false -> "member this."
            
        seq {
            yield indent + prefix + m.Name + args + " = "
            yield! (m.Body |> addIndentsLvl2 indent)
            yield ""
        }

    let printProp indent (p:Prop) = 

        match p.Get, p.Set with 
        | [], [] -> 
            seq {
                yield indent + "member val " + p.Name + " = null with get, set // TODO :: assuming both getter and setter"
                yield "" }
        | _, _  -> 
            let getter = 
                let prefix = indent + indent + "with get() = "
                match p.Get with 
                | [] -> Seq.empty
                | x::[] -> prefix + x |> Seq.singleton
                | xs -> 
                    seq {
                        yield prefix
                        yield! xs |> addIndentsLvl3 indent
                    }

            let setter = 
                let prefix = indent + indent + "and set(vale) = "
                match p.Set with 
                | [] -> Seq.empty
                | x::[] -> prefix  + x |> Seq.singleton
                | xs -> 
                    seq {
                        yield prefix 
                        yield! xs |> addIndentsLvl3 indent
                }

            seq {
                yield indent + "member this." + p.Name 
                yield! getter 
                yield! setter
                yield ""
            }

    let printField indent f = 
        let line = 
            match f.Initilizer with 
            | Some init -> indent + "let " + f.Name + " = " + init
            | None -> indent + "let " + f.Name + ":" + f.Type + " = null // TODO:: replace with default value for type"

        seq {
            if f.IsPublic then yield "// TODO:: C# declared this as public. F# does not support a public field"
            yield line } 

    let printAttribute (x:Attribute) = 
        let prefix = "[<" + x.Name
        let suffix =  ">]"
        match x.Parameters with 
        | Some ps -> prefix + ps + suffix
        | None -> prefix  + suffix
        
    let printClass indent (x:Class) = 
        if List.length x.Constructors > 1 then 
            printfn "Mulitple ctors for %s, this is not implemented" x.Name.Name

        let ctor = 
            match x.Constructors with 
            | [] -> None
            | xs -> xs |> List.maxBy (fun x -> x.Parameters |> List.length) |> Some

        let ctorArgs = ctor |> Option.map (fun x -> x.Parameters |> printParameters) |> Option.defaultValue "()"
        let methods = x.Methods |> List.map (printMethod indent) |> Seq.concat
        let props = x.Properties |> Seq.map (printProp indent) |> Seq.concat

        let attributes = x.Attributes |> Seq.map (printAttribute)

        let typeParameters = 
            match x.TypeParameters with 
            | [] -> ""
            | xs -> 
                let fTypePaarams = 
                    xs
                    |> List.map (fun x -> "'" + x)
                    |> String.concat ","

                "<" + fTypePaarams + ">"

        let subclass = 
            x.BaseClass 
            |> Option.bind (fun baseClass -> 
                ctor |> Option.map (fun ctor ->
                    let args = ctor.SubclassArgs |> String.concat","
                    indent + "inherit " + baseClass + "(" + args + ")" ))
            |> function
            | None -> seq {yield ""}
            | Some baseClass -> seq { yield baseClass; yield "" }

        let interfaces = 
            match x.ImplementInterfaces with 
            | [] -> Seq.empty
            | xs -> 
                xs |> Seq.map (fun i -> 
                    seq {
                        yield (indent + "interface " + i + " with")
                        yield ""
                    }) 
                |> Seq.concat

        let ctorBodies = 
            x.Constructors 
            |> List.map (fun ctor -> 

                if List.length ctor.Body >= 1 then 
                    seq {
                        yield indent +  "do"
                        yield! ctor.Body |> addIndentsLvl2 indent
                        yield "" }
                else Seq.empty)
            |> Seq.concat

        let fields = 
            x.Fields 
            |> List.map (printField indent)
            |> Seq.concat
            

        seq {
            yield! attributes
            yield ("type " + x.Name.Name + typeParameters + ctorArgs + " = ")
            yield! subclass
            yield! fields
            yield! ctorBodies
            yield! methods
            yield! props
            yield! interfaces
        }


    let printInterface indent (i:Interface) = 

        seq {
            yield "type " + i.Name + " ="
            yield! i.Methods
        }

    let printUsingStatement us = 
        "open " + us.Namespace

    let printNamespace indent usingStatements ns = 
        seq {
            yield "namespace " + ns.Name
            yield! usingStatements |> Seq.map (printUsingStatement)
            yield ""
            yield! ns.Interfaces |> Seq.map (printInterface indent) |> Seq.concat
            yield! ns.Classes |> Seq.map (printClass indent) |> Seq.concat
        }


    let printFile indent file = 
        seq {
            yield! file.Namespaces |> Seq.map (printNamespace indent file.UsingStatements) |> Seq.concat
            yield ""
        }

    let prettyPrint indent = function 
    | File f -> printFile indent f
    | Namespace ns -> printNamespace indent Seq.empty ns
    | UsingStatement us -> printUsingStatement us |> Seq.singleton
    | Interface i -> printInterface indent i
    | Class c -> printClass indent c
    | Field f -> f |> Seq.map (printField indent) |> Seq.concat
    | Prop p -> printProp indent p
    | Method m -> printMethod indent m
    | Empty -> Seq.empty
    
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


    let visitor = new FileContentsDumper()
    let indent = " " |> List.replicate 4 |> String.concat ""
    
    let tree = 
        System.Console.In.ReadToEnd()
        //mvvmCross
        |> SyntaxFactory.ParseSyntaxTree

    let t = tree.GetRoot()
    t.ChildNodes()
    |> Seq.fold (fun file node -> visitor.ParseSyntax file node) None
    |> Option.map (ProgramPrinter.prettyPrint indent >> String.concat "\n")
    |> Option.defaultValue "failed to parse"
    |> Console.WriteLine

    //visitor.Visit <| tree.GetRoot()

    //printfn "--\nParsed C# Code. F# below:\n--"
    //let lines = ProgramPrinter.prettyPrint visitor.GetFsharpProgram
    //lines |> Seq.iter (printfn "%s")

    0


