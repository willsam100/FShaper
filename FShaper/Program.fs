// Learn more about F# at http://fsharp.org
open System
open FSharp.Compiler.Text
open Fantomas
open FSharper.Core
open Fantomas.FormatConfig
open System.Text.RegularExpressions
open Microsoft.CodeAnalysis

[<EntryPoint>]
let main argv =

    let printFsharpTree = true

    if not printFsharpTree then 
        let input = System.Console.In.ReadToEnd()
        FSharper.Core.Converter.run input |> printfn "%s"

    // Used for debugging/development. To see the the F# syntax, add here. 
    // the syntax can then be used to identity how to construct it from the CSharp syntax
    else
        let input = 
                 """
                    type TipView<'T, 'Z>(s: string, i: int) =
                        inherit MvxContentPage<TipViewModel>(message)
                        do InitializeComponent()""" // Add expected F# syntax here

        let placeholderFilename = "/home/user/Test.fsx"
        let tree = TreeOps.getUntypedTree(placeholderFilename, SourceText.ofString input)

        let regexReplace (pattern:string, replace:string) (s:string) = 
            Regex.Replace(s, pattern, replace)

        let removeFilenameFromOutput tree = 
            tree
            |> regexReplace("\n\s+/home/user/Test.fsx", "") 
            |> regexReplace("/home/user/Test.fsx", "") 

        tree.ToString() |> removeFilenameFromOutput |> printfn "%s"

        CodeFormatter.FormatASTAsync(tree, placeholderFilename, [], input |> SourceOrigin.SourceString |> Some, {FormatConfig.Default with StrictMode = false}) |> Async.RunSynchronously |> printfn "%s"

    0


