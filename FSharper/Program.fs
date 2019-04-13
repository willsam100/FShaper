// Learn more about F# at http://fsharp.org
open System
open Fantomas
open FSharper.Core
open Fantomas.FormatConfig
open System.Text.RegularExpressions
    
[<EntryPoint>]
let main argv =

    //let input = System.Console.In.ReadToEnd()
    //FSharper.Core.Converter.run input |> printfn "%s"


    // Used for debugging/development. To see the the F# syntax, add here. 
    // the syntax can then be used to idently how to construct it from the CSharp syntax
    let printFsharpTree = true
    let input = 
             """type Foo() = 
                    let f = sprintf "%s %s" "a" "b" """ // Add expected F# syntax here

    if printFsharpTree then 

        let placeholderFilename = "/home/user/Test.fsx"
        let tree = TreeOps.getUntypedTree(placeholderFilename, input)

        let regexRepleace (pattern:string, replace:string) (s:string) = 
            Regex.Replace(s, pattern, replace)

        let removeFilenameFromOutput tree = 
            tree
            |> regexRepleace("\n\s+/home/user/Test.fsx", "") 
            |> regexRepleace("/home/user/Test.fsx", "") 

        tree.ToString() |> removeFilenameFromOutput |> printfn "%s"

        CodeFormatter.FormatAST(tree, placeholderFilename, None, FormatConfig.Default) |> printfn "%s"

    0


