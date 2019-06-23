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
                 """type Foo() =
                     member this.Index(scraper: HzzoHtmlScraper): Task<ActionResult> =
                        async {
                                let mutable startTime = DateTime.Now
                                let! meds = scraper.Run() |> Async.AwaitTask
                                let mutable totalTime = startTime - DateTime.Now 
                                return Ok(sprintf "Done! Handler duration: %O" (totalTime.Duration())
                                                + Environment.NewLine + Environment.NewLine
                                                + (TypeSyntax.Join(Environment.NewLine, meds.Select(fun x -> x.FileName))))
                         } |> Async.StartAsTask """ // Add expected F# syntax here

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


