namespace Tests
open CodeFormatter
open NUnit.Framework
open FShaper.Core
open FsUnit
open Swensen.Unquote.Assertions

[<TestFixture>]
type CommentTests () =        
        
    [<Test>]
    member this.``using statements with comment convert to open statements`` () = 
        let csharp = 
             """
                // This is a comment
                using MvvmCross.Forms.Views;
                using TipCalc.Core.ViewModels;"""

        let fsharp = 
            """
                // This is a comment
                open MvvmCross.Forms.Views
                open TipCalc.Core.ViewModels"""
                   
        csharp
        |> reduceIndent
        |> Converter.run 
        |> logConverted
        |> should equal (formatFsharpWithSource fsharp)
        
    [<Test>]
    member this.``using statements with two comments convert to open statements`` () = 
        let csharp = 
             """
                // This is a comment
                using MvvmCross.Forms.Views;
                // a comment each
                using TipCalc.Core.ViewModels;"""

        let fsharp = 
            """
                // This is a comment
                open MvvmCross.Forms.Views
                // a comment each
                open TipCalc.Core.ViewModels"""
                   
        csharp
        |> reduceIndent
        |> Converter.run 
        |> logConverted
        |> should equal (formatFsharpWithSource fsharp)
        
        
    [<Test>]
    member this.``class with comments converts`` () = 
        let csharp = 
             """
                // This is a comment
                public class MyClass
                {
                    public void DoNothing()
                    {
                    }
                }"""

        let fsharp = 
            """
                // This is a comment
                type MyClass() =
                    member this.DoNothing() = ()"""
                   
        csharp
        |> reduceIndent
        |> Converter.run 
        |> logConverted
        |> should equal (formatFsharpWithSource fsharp)
        
        
    [<Test>]
    member this.``method with comments converts`` () = 
        let csharp = 
             """
                // This is a comment
                public void DoNothing()
                {
                }"""

        let fsharp = 
            """
                // This is a comment
                member this.DoNothing() = ()"""
                   
        csharp
        |> reduceIndent
        |> Converter.run 
        |> logConverted
        |> should equal (formatFsharpWithClassWithSource fsharp)
        
        
    [<Test>]
    member this.``assignment with comments converts`` () = 
        let csharp = 
             """
                public void DoNothing()
                {
                    // Comment one
                    var v = 42;
                    // Comment two
                    v = 007;
                }"""

        let fsharp = 
            """
                member this.DoNothing() =
                    // Comment one
                    let mutable v = 42
                    // Comment two
                    v <- 7
                    """
                   
        csharp
        |> reduceIndent
        |> Converter.run 
        |> logConverted
        |> should equal (formatFsharpWithClassWithSource fsharp)
        
    [<Test>]
    member this.``assignment with multiline comments converts`` () = 
        let csharp = 
             """
                public void DoNothing()
                {
                    // Comment one
                    // Comment two
                    var v = 42;
                    v = 007;
                }"""

        let fsharp = 
            """
                member this.DoNothing() =
                    // Comment one
                    // Comment two
                    let mutable v = 42
                    v <- 7
                    """
                   
        csharp
        |> reduceIndent
        |> Converter.run 
        |> logConverted
        |> should equal (formatFsharpWithClassWithSource fsharp)
        
        
    [<Test>]
    member this.``method with XML Comment`` () = 
        let csharp = 
             """
                /// <summary>   
                /// Divides an integer by another and returns the result.
                /// </summary>
                /// <returns>
                /// The division of two integers.
                /// </returns>
                /// <exception cref="System.DivideByZeroException">Thrown when a division by zero occurs.</exception>
                public static int Divide(int a, int b)
                {
                    return a / b;
                }"""

        let fsharp = 
            """
                /// <summary>
                /// Divides an integer by another and returns the result.
                /// </summary>
                /// <returns>
                /// The division of two integers.
                /// </returns>
                /// <exception cref="System.DivideByZeroException">Thrown when a division by zero occurs.</exception>
                static member Divide(a: int, b: int): int = a / b"""
                   
        csharp
        |> reduceIndent
        |> Converter.run 
        |> logConverted
        |> should equal (formatFsharpWithClassWithSource fsharp)
        
    [<Test>]
    member this.``convert a C# block comment to and F# block comment`` () = 
        let csharp = 
             """
                /*
                    The main Math class
                    Contains all methods for performing basic math functions
                */
                public class Math
                {
                    public static double Add(double a, double b)
                    {
                        return a + b;
                    }
                }"""

        let fsharp = 
            """
                (*
                    The main Math class
                    Contains all methods for performing basic math functions
                *)
                type Math() =
                    static member Add(a: double, b: double): double = a + b"""
                   
        csharp
        |> reduceIndent
        |> Converter.run 
        |> logConverted
        |> should equal (formatFsharpWithSource fsharp)
        
        
    [<Test>]
    member this.``try catch on a statement carries over comments`` () = 
        let csharp = 
             """
                static public async Task UploadBlob(string accountName, string containerName, string blobName, string blobContents)
                {
                    try
                    {
                        // Create the container if it does not exist.
                        await containerClient.CreateIfNotExistsAsync();

                        // Upload text to a new block blob.
                        byte[] byteArray = Encoding.ASCII.GetBytes(blobContents);

                        using (MemoryStream stream = new MemoryStream(byteArray))
                        {
                            await containerClient.UploadBlobAsync(blobName, stream);
                        }
                    }
                    catch (Exception e)
                    {
                        throw e;
                    }
                }"""
    
        let fsharp = 
             """
                static member UploadBlob(accountName: string, containerName: string, blobName: string, blobContents: string): Task =
                    try
                        // Create the container if it does not exist.
                        do! containerClient.CreateIfNotExistsAsync()
                        // Upload text to a new block blob.
                        let mutable byteArray = Encoding.ASCII.GetBytes(blobContents)
                        use stream = new MemoryStream(byteArray)
                        do! containerClient.UploadBlobAsync(blobName, stream)
                    with :? Exception as e -> raise(e)"""

        csharp
        |> reduceIndent
        |> Converter.run 
        |> logConverted
        |> should equal (formatFsharpWithClassWithSource fsharp)
        
    [<Test>]
    member this.``match cases works with comments over try catch`` () = 
        let csharp = 
             """
                public void UploadBlob()
                {
                    try
                    {
                        // Create the container if it does not exist.
                        containerClient.CreateIfNotExistsAsync();
                    }
                    catch (Exception e)
                    {
                        throw e;
                    }
                }"""
    
        let fsharp = 
             """
                member this.UploadBlob() =
                    try
                        // Create the container if it does not exist.
                        containerClient.CreateIfNotExistsAsync();
                    with :? Exception as e -> raise(e)"""

        csharp
        |> reduceIndent
        |> Converter.run 
        |> logConverted
        |> should equal (formatFsharpWithClassWithSource fsharp)
        
        

