module Tests.CodeFormatter

open FShaper.Core
open Fantomas
open System

let rec reduceIndent (x:string) =
    if x.Replace("\n", "").StartsWith "    " |> not then x.Trim() 
    else 
        let trim (x:string) =
            if x.StartsWith "    " then x.Substring 4
            else 
                if x.StartsWith "\n    " then "\n" + x.Substring 5 else x
        
        x.Split '\n'
        |> Array.map trim
        |> String.concat "\n"
        |> reduceIndent
        

let formatCode s source =
    let filename = "/home/user/Test.fsx"
    let ast =
        CodeFormatter.ParseAsync(
            filename,
            SourceOrigin.SourceString s,
            Converter.createParsingOptionsFromFile filename,
            Converter.sharedChecker.Value
        )
        |> Async.RunSynchronously
        |> Seq.head
        |> fst
        
    let sourceCode =
        if source then SourceOrigin.SourceString s |> Some else None
        
    CodeFormatter.FormatASTAsync(ast, DefaultNames.file, [], sourceCode, Converter.config)
    |> Async.RunSynchronously

let rec formatFsharpWithReplacement prefix (s:string) source: string =
    let s = reduceIndent s
    let tree = 
        formatCode s source 
        |> fun x ->
            if prefix = "" then x.Trim()
            else
                x.Replace(prefix, "")
                |> reduceIndent
                |> fun x -> x.Trim()
        
    printfn "\n-------------------------------Expecting [RAW]---------------------------------\n%s" s
    printfn "\n------------------------Expecting [Compiler formatted] ------------------------\n%s" tree
    
    tree
    
let formatFsharpWithClassWithPrefix input source =
    
    let input = reduceIndent input
    
    let input =
        input.Split "\n"
        |> Array.map (fun x -> "    " + x)
        |> String.concat "\n"
        
    let prefix = sprintf "type Klass() ="
    
    let inputWithClass  =
        let sep = if input.Replace(" ", "").StartsWith "\n" then "" else "\n"
        printfn "Starting:\n%A" <| input.Substring(0, 5)
        sprintf "%s%s%s" prefix sep input 

    let prefix = inputWithClass.Replace(input, "")
    printfn "Prefix:\n%A" prefix
    formatFsharpWithReplacement prefix inputWithClass source
    
let formatFsharpWithClass s = formatFsharpWithClassWithPrefix s false
let formatFsharpWithClassWithSource s = formatFsharpWithClassWithPrefix s true
    
let formatFsharp (s:string) = formatFsharpWithReplacement "" s false
let formatFsharpWithSource (s:string) = formatFsharpWithReplacement "" s true 

let simpleFormat (s:string) =
    let s' =  reduceIndent s
    printfn "\n-------------------------------Expecting [RAW]---------------------------------\n%s" s
    printfn "\n------------------------Expecting [Simple formatted] ------------------------\n%s" s'
    s'
    
let logConverted s = 
    s |> (fun x -> printfn "\n------------------------CONVERTED------------------------\n%s\n" x; x)