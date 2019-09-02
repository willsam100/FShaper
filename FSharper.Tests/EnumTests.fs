namespace Tests

open NUnit.Framework
open FSharper.Core
open FsUnit
open System

[<TestFixture>]
type EnumTests () =

    let formatFsharp (s:string) = 

        let indent = "                "
        s.Split ("\n") |> Array.map (fun x -> if x.StartsWith indent then x.Substring indent.Length else x) |> String.concat "\n"
        |> (fun s -> 
            s
                .Replace("\n    \n", "\n\n")
                .Replace("\n            \n", "\n\n")
                .Trim() )


    [<Test>]
    member this.``Simple enum maps directly`` () = 
        let csharp = 
             """enum EnumTest { 
                    None = 0,
                    First = 1
                }"""
    
        let fsharp = 
             """type EnumTest =
                    | None = 0
                    | First = 1"""
                   
        csharp |> Converter.run 
        |> (fun x -> printfn "%s" x; x)
        |> should equal (formatFsharp fsharp)

    [<Test>]
    member this.``Enum with non-sequential values`` () = 
        let csharp = 
                """enum EnumTest { 
                    None = 0,
                    Ten = 10,
                    Twenty = 20
                }"""
        
        let fsharp = 
                """type EnumTest =
                    | None = 0
                    | Ten = 10
                    | Twenty = 20"""
                       
        csharp |> Converter.run 
        |> (fun x -> printfn "%s" x; x)
        |> should equal (formatFsharp fsharp)