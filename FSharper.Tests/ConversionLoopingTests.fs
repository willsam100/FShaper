namespace Tests

open NUnit.Framework
open FSharper.Core
open FsUnit
open System

[<TestFixture>]
type LoopngTests () =

    let formatFsharp (s:string) = 

        let indent = "                "
        s.Split ("\n") |> Array.map (fun x -> if x.StartsWith indent then x.Substring indent.Length else x) |> String.concat "\n"
        |> (fun s -> 
            s
                .Replace("\n    \n", "\n\n")
                .Replace("\n            \n", "\n\n")
                .Trim() )

    [<Test>]
    member this.``standard incrementing for loop i, i < i++`` () = 
        let csharp = 
             """for (int n = 0; n < 10; n++) 
                {
                    Console.WriteLine($"{n}");
                }"""

        let fsharp = 
             """for n = 0 to 10 do
                        Console.WriteLine(sprintf "%O" (n))"""
                   
        csharp |> Converter.run 
        |> (fun x -> printfn "%s" x; x)
        |> should equal (formatFsharp fsharp)

    [<Test>]
    member this.``standard decrementing for loop i, i < i--`` () = 
        let csharp = 
             """for (int n = 10; n > 0; n--) 
                {
                    Console.WriteLine($"{n}");
                }"""

        let fsharp = 
             """for n = 10 downto 1 do
                        Console.WriteLine(sprintf "%O" (n))"""
                   
        csharp |> Converter.run 
        |> (fun x -> printfn "%s" x; x)
        |> should equal (formatFsharp fsharp)

    [<Test>]
    member this.``weird for loop`` () = 
        let csharp = 
             """void Foo()
                {
                    int i, j;
                    long[] c = new long[100];
                    for (c[i = 1] = 1L; i < n; c[0] = -c[0], i++)
                    {
                        Console.WriteLine($"{i}");
                    }
                }"""

        let fsharp = 
             """member this.Foo() =
                    let mutable i = Unchecked.defaultof<int>
                    let mutable j = Unchecked.defaultof<int>
                    let mutable c = Array.zeroCreate<int64> (100)
                    i <- 1
                    c.[i] <- 1L
                    for i = 1 to n do
                        Console.WriteLine(sprintf "%O" (i))
                        c.[0] <- -c.[0]"""
                   
        csharp |> Converter.run 
        |> (fun x -> printfn "%s" x; x)
        |> should equal (formatFsharp fsharp)

    [<Test>]
    member this.``weird double for loop`` () = 
        let csharp = 
             """void coef(int n)
                {
                    int i, j;

                    if (n < 0 || n > 63) System.Environment.Exit(0);// gracefully deal with range issue

                    for (c[i = 0] = 1L; i < n; c[0] = -c[0], i++)
                        for (c[1 + (j = i)] = 1L; j > 0; j--)
                            c[j] = c[j - 1] - c[j];
                }"""

        let fsharp = 
             """member this.coef (n: int) =
                    let mutable i = Unchecked.defaultof<int>
                    let mutable j = Unchecked.defaultof<int>
                    if n < 0 || n > 63 then System.Environment.Exit(0)
                    else
                        i <- 0
                        c.[i] <- 1L
                        for i = 0 to n do
                            j <- i
                            c.[1 + j] <- 1L
                            for j = i downto 1 do
                                c.[j] <- c.[j - 1] - c.[j]
                            c.[0] <- -c.[0]"""
                   
        csharp |> Converter.run 
        |> (fun x -> printfn "%s" x; x)
        |> should equal (formatFsharp fsharp)

    [<Test>]
    member this.``convert ushort cast`` () = 
        let csharp = 
             """public void Main() {
                    for (ushort ctr = (ushort)'a'; ctr <= (ushort) 'z'; ctr++)
                        sb.Append(Convert.ToChar(ctr), 4); 
                }"""
    
        let fsharp = 
             """member this.Main() =
                    for ctr = (int 'a') to (int 'z') do
                        sb.Append(Convert.ToChar(ctr), 4)"""

        csharp |> Converter.runWithConfig false 
        |> (fun x -> printfn "%s" x; x)
        |> should equal (formatFsharp fsharp)

    [<Test>]
    member this.``simple while loop`` () = 
        let csharp = 
             """void Loop()
                {
                    int i = 10;
                    while (i >= 1)
                    {
                        i--;
                        Console.WriteLine($"{i}");
                    }
                }"""

        let fsharp = 
             """member this.Loop() =
                    let mutable i = 10
                    while i >= 1 do
                        i <- i - 1
                        Console.WriteLine(sprintf "%O" (i))"""
                   
        csharp |> Converter.run 
        |> (fun x -> printfn "%s" x; x)
        |> should equal (formatFsharp fsharp)

    [<Test>]
    member this.``simple while loop with preincrement`` () = 
        let csharp = 
             """void Loop()
                {
                    while (--i >= 1)
                        Console.WriteLine($"{i}");
                }"""

        let fsharp = 
             """member this.Loop() =
                    i <- i - 1
                    while i >= 1 do
                        Console.WriteLine(sprintf "%O" (i))
                        i <- i - 1"""
                   
        csharp |> Converter.run 
        |> (fun x -> printfn "%s" x; x)
        |> should equal (formatFsharp fsharp)

    [<Test>]
    member this.``simple while loop with postincrement`` () = 
        let csharp = 
             """void Loop()
                {
                    while (i-- >= 1)
                        Console.WriteLine($"{i}");
                }"""

        let fsharp = 
             """member this.Loop() =
                    while i >= 1 do
                        i <- i - 1
                        Console.WriteLine(sprintf "%O" (i))"""
                   
        csharp |> Converter.run 
        |> (fun x -> printfn "%s" x; x)
        |> should equal (formatFsharp fsharp)

    [<Test>]
    member this.``for loop with long ident for condition and preincrement`` () = 
        let csharp = 
             """void Foo() 
                {
                    for(int i = 0; i < word.Length; ++i)
                    {
                        Console.WriteLine($"{i}");
                    }
                }"""

        let fsharp = 
             """member this.Foo() =
                    for i = 0 to word.Length do
                        Console.WriteLine(sprintf "%O" (i))"""
                   
        csharp |> Converter.run 
        |> (fun x -> printfn "%s" x; x)
        |> should equal (formatFsharp fsharp)
