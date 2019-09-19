#r "paket:
nuget FSharp.Core 4.6.0.0
nuget Fake.Core.Target
nuget Fake.DotNet.Cli //"

#load ".fake/build.fsx/intellisense.fsx"
open Fake.Core
open Fake.DotNet
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing.Operators
open Fake.Core.TargetOperators

Target.initEnvironment()

let solutionFile = "FSharper.sln"
let fshaperProject = "FSharper.Core/FSharper.Core.fsproj"
let fshaperTestsProject = "FSharper.Tests/FSharper.Tests.fsproj"

// On OSX there is a bug running the tests. Requires this version of dotnet to run correctly.
let Release_2_1_505 (option: DotNet.CliInstallOptions) =
    { option with
        InstallerOptions = (fun io ->
            { io with
                Branch = "release/2.1"
            })
        Channel = None
        Version = DotNet.CliVersion.Version "2.1.505"
    }

// // Lazily install DotNet SDK in the correct version if not available
let install = lazy DotNet.install Release_2_1_505

// // Set general properties without arguments
let inline dotnetSimple arg = DotNet.Options.lift install.Value arg

let inline withWorkDir wd =
    DotNet.Options.lift install.Value
    >> DotNet.Options.withWorkingDirectory wd

Target.create "Clean" (fun _ ->
    !! "**/bin"
    ++ "**/obj"
    |> Shell.cleanDirs 
)

Target.create "Restore" (fun _ -> DotNet.restore dotnetSimple solutionFile )
Target.create "Build" (fun _ -> DotNet.build dotnetSimple solutionFile )
Target.create "Test" (fun _ -> DotNet.test dotnetSimple fshaperTestsProject)

Target.create "All" ignore

"Clean"
  ==> "Restore"
  ==> "Build"
  ==> "Test"
  ==> "All"

Target.runOrDefault "Test"
