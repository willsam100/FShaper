namespace Tests
open NUnit.Framework
open FShaper.Core
open FsUnit
open System
open CodeFormatter

[<TestFixture>]
type AsyncAwaitTests () =

    [<Test>]
    member this.``async no await writes as normal method`` () = 
        let csharp = 
             """
                public async void Foo ()
                {
                    return;
                }"""
    
        let fsharp = 
             """
                member this.Foo() = ()"""
                   
        csharp
        |> reduceIndent
        |> Converter.run 
        |> logConverted
        |> should equal (formatFsharpWithClass fsharp)

    [<Test>]
    member this.``async with await writes as async computation method`` () = 
        let csharp = 
             """
                public async void Foo ()
                {
                    await Bar.WriteAsync();
                }"""
    
        let fsharp = 
             """
                member this.Foo() = async { do! Bar.WriteAsync() |> Async.AwaitTask } |> Async.StartAsTask"""
                   
        csharp
        |> reduceIndent
        |> Converter.run 
        |> logConverted
        |> should equal (formatFsharpWithClass fsharp)

    [<Test>]
    member this.``await writes as async with let! in async computation`` () = 
        let csharp = 
             """
                public async void Foo ()
                {
                    var x = await Bar.WriteAsync();
                    return x;
                }"""
    
        let fsharp = 
             """
                member this.Foo() = async { return Bar.WriteAsync() |> Async.AwaitTask } |> Async.StartAsTask"""
                   
        csharp
        |> reduceIndent
        |> Converter.run 
        |> logConverted
        |> should equal (formatFsharpWithClass fsharp)

    [<Test>]
    member this.``await writes as async with multiple let!`` () = 
        let csharp = 
             """
                public async void Foo ()
                {
                    var x = await Bar.GetX();
                    var y = await Baz.GetY();
                    return x + y;
                }"""
    
        let fsharp = 
             """
                member this.Foo() = async { let! x = Bar.GetX() |> Async.AwaitTask
                                            let! y = Baz.GetY() |> Async.AwaitTask
                                            return x + y } |> Async.StartAsTask"""
                   
        csharp
        |> reduceIndent
        |> Converter.run 
        |> logConverted
        |> should equal (formatFsharpWithClass fsharp)

    [<Test>]
    member this.``return keyword is added with complex final statement`` () = 
        let csharp = 
             """
                public async Task<ActionResult> Index([FromServices] HeelHtmlScraper scraper)
                {
                    var startTime = DateTime.Now;
                    var meds = await scraper.Run();
                    var totalTime = startTime - DateTime.Now;

                    return Ok(
                        $"Done! Handler duration: {totalTime.Duration()}" +
                        Environment.NewLine +
                        Environment.NewLine +
                        String.Join(Environment.NewLine, meds.Select(x => x.FileName))
                    );
                }"""
    
        let fsharp = 
             """
                member this.Index(scraper: HeelHtmlScraper): Task<ActionResult> =
                    async {
                        let mutable startTime = DateTime.Now
                        let! meds = scraper.Run() |> Async.AwaitTask
                        let mutable totalTime = startTime - DateTime.Now
                        return Ok
                                   (sprintf "Done! Handler duration: %O" (totalTime.Duration()) + Environment.NewLine
                                    + Environment.NewLine + String.Join(Environment.NewLine, meds.Select((fun x -> x.FileName))))
                    }
                    |> Async.StartAsTask"""
                   
        csharp
        |> reduceIndent
        |> Converter.run 
        |> logConverted
        |> should equal (formatFsharpWithClass fsharp)

    [<Test>]
    member this.``can convert if statements without else and additional statements in async method`` () = 
        let csharp = 
             """
                protected async void OnGetViewControlAsync (CancellationToken token, DocumentViewContent view)
        		{
        			if (globalOptions == null) {
        				OnConfigurationZoomLevelChanged (null, EventArgs.Empty);
        			}

        			// Content providers can provide additional content
        			NotifyContentChanged ();
        			await Load (false);
        		}"""

        let fsharp = 
             """
                member this.OnGetViewControlAsync(token: CancellationToken, view: DocumentViewContent) =
                    async {
                        if globalOptions = null then
                            OnConfigurationZoomLevelChanged(null, EventArgs.Empty)
                        
                        // Content providers can provide additional content
                        NotifyContentChanged()
                        do! Load(false) |> Async.AwaitTask
                    }
                    |> Async.StartAsTask"""
                   
        csharp
        |> reduceIndent
        |> Converter.run 
        |> logConverted
        |> should equal (formatFsharpWithClassWithSource fsharp)


    [<Test>]
    member this.``return keyword is added with if statement`` () = 
        let csharp = 
             """
                public async Task<ActionResult<TodoItem>> GetTodoItem(long id)
                {
                    var todoItem = await _context.TodoItems.FindAsync(id);

                    if (todoItem == null)
                    {
                        return NotFound();
                    }

                    return todoItem;
                }"""
    
        let fsharp = 
             """
                member this.GetTodoItem(id: int64): Task<ActionResult<TodoItem>> =
                    async {
                        let! todoItem = _context.TodoItems.FindAsync(id) |> Async.AwaitTask
                        return if todoItem = null then NotFound()
                               else todoItem
                    }
                    |> Async.StartAsTask"""
                   
        csharp
        |> reduceIndent
        |> Converter.run 
        |> logConverted
        |> should equal (formatFsharpWithClass fsharp)

    // At first this test seemed like a good idea. It turns out that is would be nothing more than `find and replace` since 
    // there isn't a way to know the foo() returns a task. For this reason the test is left commented out. 
    // The resulting code should still compile under F# (and possible deadlock because it's bad in C# and F#). 
    // [<Test>]
    // member this.``C result is translated to Async.RunSynchronously`` () =
    //     let csharp = 
    //          """public int Foo()
    //             {
    //                 return foo().Result;
    //             }"""
    
    //     let fsharp = 
    //          """member this.Foo(): int =
    //                 foo() |> Async.RunSynchronously"""
                   
    //     csharp |> Converter.run 
    //     |> (fun x -> printfn "%s" x; x)
    //     |> should equal (formatFsharpWithClass fsharp)
