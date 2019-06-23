namespace Tests

open NUnit.Framework
open FSharper.Core
open FsUnit
open System

[<TestFixture>]
type AsyncAwaitTests () =

    let formatFsharp (s:string) = 

        let indent = "                "
        s.Split ("\n") |> Array.map (fun x -> if x.StartsWith indent then x.Substring indent.Length else x) |> String.concat "\n"
        |> (fun s -> 
            s
                .Replace("\n    \n", "\n\n")
                .Replace("\n            \n", "\n\n")
                .Trim() )


    [<Test>]
    member this.``async no await writes as normal method`` () = 
        let csharp = 
             """public async void Foo ()
                {
                    return;
                }"""
    
        let fsharp = 
             """member this.Foo() = ()"""
                   
        csharp |> Converter.run 
        |> (fun x -> printfn "%s" x; x)
        |> should equal (formatFsharp fsharp)

    [<Test>]
    member this.``async with await writes as async computation method`` () = 
        let csharp = 
             """public async void Foo ()
                {
                    await Bar.WriteAsync();
                }"""
    
        let fsharp = 
             """member this.Foo() = async { do! Bar.WriteAsync() |> Async.AwaitTask } |> Async.StartAsTask"""
                   
        csharp |> Converter.run 
        |> (fun x -> printfn "%s" x; x)
        |> should equal (formatFsharp fsharp)

    [<Test>]
    member this.``await writes as async with let! in async computation`` () = 
        let csharp = 
             """public async void Foo ()
                {
                    var x = await Bar.WriteAsync();
                    return x;
                }"""
    
        let fsharp = 
             """member this.Foo() = async { return Bar.WriteAsync() |> Async.AwaitTask } |> Async.StartAsTask"""
                   
        csharp |> Converter.run 
        |> (fun x -> printfn "%s" x; x)
        |> should equal (formatFsharp fsharp)

    [<Test>]
    member this.``await writes as async with multiple let!`` () = 
        let csharp = 
             """public async void Foo ()
                {
                    var x = await Bar.GetX();
                    var y = await Baz.GetY();
                    return x + y;
                }"""
    
        let fsharp = 
             """member this.Foo() = async { let! x = Bar.GetX() |> Async.AwaitTask
                                            let! y = Baz.GetY() |> Async.AwaitTask
                                            return x + y } |> Async.StartAsTask"""
                   
        csharp |> Converter.run 
        |> (fun x -> printfn "%s" x; x)
        |> should equal (formatFsharp fsharp)

    [<Test>]
    member this.``return keyword is added with complex final statement`` () = 
        let csharp = 
             """public async Task<ActionResult> Index([FromServices] HzzoHtmlScraper scraper)
                {
                    var startTime = DateTime.Now;
                    var meds = await scraper.Run();
                    var totalTime = startTime - DateTime.Now;

                    return Ok(
                        $"Done! Handler duration: {totalTime.Duration()}" +
                        Environment.NewLine +
                        Environment.NewLine +
                        string.Join(Environment.NewLine, meds.Select(x => x.FileName))
                    );
                }"""
    
        let fsharp = 
             """member this.Index(scraper: HzzoHtmlScraper): Task<ActionResult> =
                    async {
                        let mutable startTime = DateTime.Now
                        let! meds = scraper.Run() |> Async.AwaitTask
                        let mutable totalTime = startTime - DateTime.Now
                        return Ok
                                   (sprintf "Done! Handler duration: %O" (totalTime.Duration()) + Environment.NewLine
                                    + Environment.NewLine + TypeSyntax.Join(Environment.NewLine, meds.Select(fun x -> x.FileName)))
                    }
                    |> Async.StartAsTask"""
                   
        csharp |> Converter.run 
        |> (fun x -> printfn "%s" x; x)
        |> should equal (formatFsharp fsharp)
