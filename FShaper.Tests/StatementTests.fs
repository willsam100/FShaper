namespace Tests
open CodeFormatter
open NUnit.Framework
open FShaper.Core
open FsUnit
open Swensen.Unquote.Assertions

[<TestFixture>]
type StatementTests () =        

    [<Test>]
    member this.``using statements convert to open statements`` () = 
        let csharp = 
             """
                using MvvmCross.Forms.Views;
                using TipCalc.Core.ViewModels;"""

        let fsharp = 
            """
                open MvvmCross.Forms.Views
                open TipCalc.Core.ViewModels"""
                   
        csharp
        |> reduceIndent
        |> Converter.run 
        |> logConverted
        |> should equal (formatFsharp fsharp)

    [<Test>]
    member this.``array initialization`` () = 
        let csharp = 
             """
                long[] c = new long[100];"""

        let fsharp = 
            """
                let mutable c = Array.zeroCreate<int64>(100)
                ()"""
                   
        csharp
        |> reduceIndent
        |> Converter.run 
        |> logConverted
        |> should equal (formatFsharp fsharp)

    [<Test>]
    member this.``array init with values`` () = 
        let csharp = 
             """
                string[] values = { "12", "31.", "5.8:32:16", "12:12:15.95", ".12"};"""

        let fsharp = 
             """
                let mutable values =
                    [| "12"
                       "31."
                       "5.8:32:16"
                       "12:12:15.95"
                       ".12" |]

                ()"""
                   
        csharp
        |> reduceIndent
        |> Converter.run 
        |> logConverted
        |> should equal (formatFsharp fsharp)        


    [<Test>]
    member this.``array update with nested assignment`` () = 
        let csharp = 
             """
                c[i = n] -= 1;"""

        let fsharp = 
             """
                i <- n
                c.[i] <- c.[i] - 1"""
                   
        csharp
        |> reduceIndent
        |> Converter.run 
        |> logConverted
        |> should equal (formatFsharp fsharp)
        
    [<Test>]
    member this.``list initialization`` () = 
        let csharp = 
             """
                List<int> ListGeneric = new List<int> { 5, 9, 1, 4 };"""
    
        let fsharp = 
             """
                let mutable ListGeneric =
                    new List<int>(
                        [| 5
                           9
                           1
                           4 |]
                    )

                ()"""
        csharp
        |> reduceIndent
        |> Converter.runWithConfig false
        |> logConverted
        |> should equal (formatFsharp fsharp)   
        
    [<Test>]
    member this.``match is converts to pattern match`` () = 
        let csharp = 
             """
                if (value is DateTime)
                    writer.WriteValue(((DateTime)value)); 
                else
                    base.WriteJson(writer, value, serializer);
                """

        let fsharp = 
             """
                if value :? DateTime then writer.WriteValue((value :?> DateTime))
                else base.WriteJson(writer, value, serializer)"""

        test <@ csharp |> Converter.run = formatFsharp fsharp @>
        
    [<Test>]
    member this.``single line`` () = 
        let csharp = 
            """
                AppCenter.Start("cf32a57f-60c4-4L42-aac1-a84148c29b0d", typeof(Push));"""

        let fsharp = 
            """
                AppCenter.Start("cf32a57f-60c4-4L42-aac1-a84148c29b0d", typeof<Push>)"""
                   
        csharp
        |> reduceIndent
        |> Converter.run 
        |> logConverted
        |> should equal (formatFsharp fsharp)
        


    [<Test>]
    member this.``single statements`` () = 
        let csharp = 
             """
                var credentials = new StoredProfileAWSCredentials(profileName);
                var s3Client = new AmazonS3Client(credentials, RegionEndpoint.USWest2);"""
    
        let fsharp = 
             """
                let mutable credentials = new StoredProfileAWSCredentials(profileName)
                let mutable s3Client = new AmazonS3Client(credentials, RegionEndpoint.USWest2)
                ()"""

        csharp
        |> reduceIndent
        |> Converter.runWithConfig false
        |> logConverted
        |> should equal (formatFsharp fsharp)
        

    [<Test>]
    member this.``console print line`` () = 
        let csharp = 
             """
                Console.Write($"{row[i]}\t");"""
    
        let fsharp = 
             """
                Console.Write(sprintf "%O\t" (row.[i]))"""

        csharp
        |> reduceIndent
        |> Converter.runWithConfig false
        |> logConverted
        |> should equal (formatFsharp fsharp)
        
        
    [<Test>]
    member this.``convert generic array`` () = 
        let csharp = 
             """
                var countSelectColumn = transformedData.GetColumn<float[]>(
                transformedData.Schema[columnName]);"""
    
        let fsharp = 
             """
                let mutable countSelectColumn = transformedData.GetColumn<float32 []>(transformedData.Schema.[columnName])
                ()"""

        csharp
        |> reduceIndent
        |> Converter.runWithConfig false
        |> logConverted
        |> should equal (formatFsharp fsharp)
        
        
        