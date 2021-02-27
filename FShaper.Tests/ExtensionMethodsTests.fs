namespace Tests

open NUnit.Framework
open FSharper.Core
open FsUnit
open System

[<TestFixture>]
type ExtensionMethodTests () =

    let formatFsharp (s:string) = 

        let indent = "                "
        s.Split ("\n") |> Array.map (fun x -> if x.StartsWith indent then x.Substring indent.Length else x) |> String.concat "\n"
        |> (fun s -> 
            s
                .Replace("\n    \n", "\n\n")
                .Replace("\n            \n", "\n\n")
                .Trim() )


    [<Test>]
    member this.``Extension method is converted correclty`` () = 
        let csharp = 
             """using AngleSharp;
                using Microsoft.Extensions.DependencyInjection;

                namespace MedsProcessor.Scraper
                {
                    public static class ServiceCollectionExtensions
                    {
                        public static IServiceCollection AddAngleSharp(this IServiceCollection services) =>
                            services.AddSingleton(BrowsingContext.New(
                                AngleSharp.Configuration.Default.WithDefaultLoader()));
                    }
                }"""
    
        let fsharp = 
             """namespace MedsProcessor.Scraper

                open AngleSharp
                open Microsoft.Extensions.DependencyInjection

                type ServiceCollectionExtensions() =
                    static member AddAngleSharp(services: IServiceCollection): IServiceCollection =
                        services.AddSingleton(BrowsingContext.New(AngleSharp.Configuration.Default.WithDefaultLoader()))"""
                   
        csharp |> Converter.run 
        |> (fun x -> printfn "%s" x; x)
        |> should equal (formatFsharp fsharp)