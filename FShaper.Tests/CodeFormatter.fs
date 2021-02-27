module Tests.CodeFormatter

open FSharper.Core
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
        

let formatCode s =
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
        
    CodeFormatter.FormatASTAsync(ast, DefaultNames.file, [], (None), Converter.config)
    |> Async.RunSynchronously

let rec formatFsharpWithReplacement prefix (s:string): string =
    let s = reduceIndent s
    let tree = 
        formatCode s 
        |> fun x ->
            if prefix = "" then x.Trim()
            else
                x.Replace(prefix, "")
                |> reduceIndent
                |> fun x -> x.Trim()
        
        
    printfn "\n-------------------------------Expecting [RAW]---------------------------------\n%s" s
    printfn "\n------------------------Expecting [Compiler formatted] ------------------------\n%s" tree
    
    tree
    
let formatFsharpWithClassWithPrefix prefix input =
    
    let input = reduceIndent input
    
    let input =
        input.Split "\n"
        |> Array.map (fun x -> "    " + x)
        |> String.concat "\n"
        
    let prefix = sprintf "type Klass() =%s" prefix
    
    let inputWithClass  =
        let sep = if input.Replace(" ", "").StartsWith "\n" then "" else "\n"
        printfn "Starting:\n%A" <| input.Substring(0, 5)
        sprintf "%s%s%s" prefix sep input 

    let prefix = inputWithClass.Replace(input, "")
    printfn "Prefix:\n%A" prefix
    formatFsharpWithReplacement prefix inputWithClass
    
let formatFsharpWithClass = formatFsharpWithClassWithPrefix ""    
    
let formatFsharp (s:string) = formatFsharpWithReplacement "" s

let simpleFormat (s:string) =
    let s' =  reduceIndent s
    printfn "\n-------------------------------Expecting [RAW]---------------------------------\n%s" s
    printfn "\n------------------------Expecting [Simple formatted] ------------------------\n%s" s'
    s'
    
    
    
let logConverted s = 
    s |> (fun x -> printfn "\n------------------------CONVERTED------------------------\n%s" x; x)