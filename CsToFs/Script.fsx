#r "/Users/sam.williams/.nuget/packages/microsoft.codeanalysis.csharp.workspaces/2.9.0/lib/netstandard1.3/Microsoft.CodeAnalysis.CSharp.Workspaces.dll"
#r "/Users/sam.williams/.nuget/packages/microsoft.codeanalysis.csharp/2.9.0/lib/netstandard1.3/Microsoft.CodeAnalysis.CSharp.dll"
#r "/Users/sam.williams/.nuget/packages/microsoft.codeanalysis.common/2.9.0/lib/netstandard1.3/Microsoft.CodeAnalysis.dll"

#load "/Users/sam.williams/projects/CsToFs/CsToFs/DomainTypes.fs"
#load "/Users/sam.williams/projects/CsToFs/CsToFs/CsharpParser.fs"
#load "/Users/sam.williams/projects/CsToFs/CsToFs/DomainPrinter.fs"

//#r "mscorlib.dll"
open System
open System.IO
open CsToFs
open Microsoft.CodeAnalysis
open System.Linq
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax

let run (text:string) = 
    let visitor = new FileContentsDumper()
    let indent = " " |> List.replicate 4 |> String.concat ""

    let tree = text|> SyntaxFactory.ParseSyntaxTree

    let t = tree.GetRoot()
    t.ChildNodes()
    |> Seq.fold (fun file node -> visitor.ParseSyntax file node) None
    |> Option.map (ProgramPrinter.prettyPrint indent >> String.concat "\n")
    |> Option.iter (printfn "%s")
    //|> Option.map (ProgramPrinter.prettyPrint indent >> String.concat "\n")
    //|> Option.defaultValue "failed to parse"
    //|> Console.WriteLine