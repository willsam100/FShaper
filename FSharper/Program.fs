// Learn more about F# at http://fsharp.org
open System
open Fantomas
open FSharper.Core
open Fantomas.FormatConfig
open System.Text.RegularExpressions

type Main( [<ParamArray>] foobar: string [] ) = 
    member this.Foo() = ()

[<EntryPoint>]
let main argv =

    let printFsharpTree = true

    if not printFsharpTree then 
        let input = System.Console.In.ReadToEnd()
        FSharper.Core.Converter.run input |> printfn "%s"

    // Used for debugging/development. To see the the F# syntax, add here. 
    // the syntax can then be used to idently how to construct it from the CSharp syntax
    else
        let input = 
                 """
                    open System

                    [<assembly:ResolutionGroupName ("MyCompany")>] do()
                    [<assembly:ExportEffect (typeof<BackgroundColorEffect>, "BackgroundColorEffect")>] do()

                    type Service = 
                        inherit global.Foo.Bar.MainActivity() = 
                        member For = 42""" // Add expected F# syntax here

        let placeholderFilename = "/home/user/Test.fsx"
        let tree = TreeOps.getUntypedTree(placeholderFilename, input)

        let regexRepleace (pattern:string, replace:string) (s:string) = 
            Regex.Replace(s, pattern, replace)

        let removeFilenameFromOutput tree = 
            tree
            |> regexRepleace("\n\s+/home/user/Test.fsx", "") 
            |> regexRepleace("/home/user/Test.fsx", "") 

        tree.ToString() |> removeFilenameFromOutput |> printfn "%s"

        CodeFormatter.FormatAST(tree, placeholderFilename, Some input, {FormatConfig.Default with StrictMode = false}) |> printfn "%s"

    0


