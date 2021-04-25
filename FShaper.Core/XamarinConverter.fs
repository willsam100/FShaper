module FShaper.Core.XamarinConverter

open System
open System.IO 
open System.Text.RegularExpressions

module Android =

    let updateAndroidProjFile path = 
        File.ReadAllLines path 
        |> Array.toList
        |> List.filter (fun x -> x.Contains "Resource.Designer." |> not)
        |> List.map (fun x -> x.Replace("Xamarin.Android.CSharp.targets", "Xamarin.Android.FSharp.targets" ))
        |> List.collect (fun x -> 
            let indent = x |> Seq.takeWhile Char.IsWhiteSpace |> Seq.toArray |> String
            if x.Contains "Mono.Android" then 
                [x; indent + """<Reference Include="Microsoft.CSharp" />"""]
            elif x.StartsWith "<Project " then 
                [x; indent + """<Import Project="$(MSBuildExtensionsPath)\$(MSBuildToolsVersion)\Microsoft.Common.props" Condition="Exists('$(MSBuildExtensionsPath)\$(MSBuildToolsVersion)\Microsoft.Common.props')" />"""]
            elif x.Contains """Reference Include="System.Xml" />""" then 
                [
                    x 
                    indent + """<Reference Include="Xamarin.Android.FSharp.ResourceProvider.Runtime">"""
                    indent + """  <HintPath>..\packages\Xamarin.Android.FSharp.ResourceProvider.1.0.0.28\lib\monoandroid81\Xamarin.Android.FSharp.ResourceProvider.Runtime.dll</HintPath>"""
                    indent + """</Reference>"""]                

            else [x])
        |> fun x -> File.WriteAllLines (path, x)   

    let updateProjectJsonFile androidProj = 
        printfn "%s" <| Path.GetDirectoryName androidProj
        let projectJson = Path.Combine(Path.GetDirectoryName androidProj, "package.json")
        if File.Exists projectJson then 
            File.ReadAllLines projectJson
            |> Array.toList
            |> List.collect (fun x -> 
                if x.Contains "<packages>" then 
                    [x; """  <package id="Xamarin.Android.FSharp.ResourceProvider" version="1.0.0.28" />"""]
                else [x] )
            |> fun x -> File.WriteAllLines (projectJson, x)            
        else 
            [
                """<?xml version="1.0" encoding="utf-8"?>"""
                """<packages>"""
                """  <package id="Xamarin.Android.FSharp.ResourceProvider" version="1.0.0.28" />"""
                """</packages>"""
            ] |> fun x -> File.WriteAllLines (projectJson, x)

    let getAndroidProjFile (projs: string list) = 
        projs 
        |> List.filter (fun x -> x.ToLower().Contains "android")
        |> function 
            | [] -> None
            | [x] -> Some x
            | _ -> None

    let removeCsResourceFile androidProj = 
        let resource = Path.Combine(Path.GetDirectoryName androidProj, "Resources", "Resource.designer.fs")
        if File.Exists resource then 
            File.Delete(resource)

        let resource = Path.Combine(Path.GetDirectoryName androidProj, "Resources", "Resource.designer.cs")
        if File.Exists resource then 
            File.Delete(resource)
        

let getSlnFile rootDir = 
    Directory.GetFiles rootDir 
    |> Array.filter (fun x -> Path.GetExtension x = ".sln")
    |> Array.toList

let renameContentsSoultionFile path = 
    File.ReadAllLines path 
    |> Array.map (fun x -> x.Replace(".csproj", ".fsproj") )
    |> (fun output -> File.WriteAllLines(path, output))

let rec findFiles filter path: string list = 
    let nestedFiles = 
        Directory.GetDirectories path
        |> Array.filter (fun x -> Path.GetFileName(x).[0] <> '.') // Filter out hidden files
        |> Array.toList
        |> List.collect (findFiles filter)

    let localFiles = 
        Directory.GetFiles path
        |> Array.filter filter
        |> Array.toList

    localFiles @ nestedFiles

let isNotUwp (x:string) = x.ToLower().Contains "uwp" |> not
let csToFs (x:string) = x.Replace(".cs", ".fs")
let findFsFiles = findFiles (fun x -> Path.GetExtension x = ".fs")

let move (f: string -> string) (x: string) = 
    let name = Path.GetFileName x
    printfn "Renaming: %s -> %s" name (f name)
    File.Move (x, f x)

let renameProjFiles = 
    let findProjFiles = findFiles (fun x -> Path.GetExtension x = ".csproj")
    findProjFiles >> List.filter isNotUwp >> List.iter (move csToFs)
        
let renameCsFiles = 
    let csharpFiles = findFiles (fun x -> x.EndsWith ".cs")
    csharpFiles >> List.filter isNotUwp  >> List.iter (move csToFs)

let renameContentsFsprojFiles = 
    let findProjFiles = findFiles (fun x -> Path.GetExtension x = ".fsproj")
    
    let replaceProjFileContentsToFsFiles (file:string) = 
        File.ReadAllLines file
        |> Array.map csToFs
        |> (fun lines -> File.WriteAllLines (file, lines))

    findProjFiles >> List.iter replaceProjFileContentsToFsFiles

let getCoreProj = 
    let findProjFiles = findFiles (fun x -> Path.GetExtension x = ".fsproj")
    findProjFiles >> List.filter (fun path -> 
        let path = path.ToLower() 
        (path.Contains "ios" || path.Contains "android") |> not)


let updateCoreProj coreProj = 
    let fsFiles = findFsFiles (Path.GetDirectoryName coreProj)
    let xaml = findFiles (fun x -> x.EndsWith ".xaml") (Path.GetDirectoryName coreProj)
    
    let xmalPairToXml (xaml, xamlFs) = 
        [
            yield sprintf """<EmbeddedResource Include="%s" />""" <| Path.GetFileName xaml
            yield sprintf """<Compile Include="%s">""" <| Path.GetFileName xamlFs
            yield sprintf """  <DependentUpon>%s</DependentUpon>""" <| Path.GetFileName xaml
            yield  "</Compile>"     
        ]

    let fsXmlCompile fsFile = 
        sprintf """<Compile Include="%s" />""" <| Path.GetFileName fsFile


    let asItemGroup files = 
        let files = files |> List.map (fun x -> "  " + x)
        [
            yield "<ItemGroup>"
            yield! files
            yield "</ItemGroup>"
        ] |> List.map (fun x -> "  " + x)
    
    let xamlPaired (xaml: string list) (fsFiles: string list) = 
        let xamlfs = fsFiles |> List.filter (fun x -> x.Contains "xaml")

        xamlfs
        |> List.choose (fun x -> 
            let name = (Path.GetFileNameWithoutExtension x).Replace(".xaml", "")
            xaml 
            |> List.tryFind (fun y -> y.Contains name) 
            |> Option.map (fun xaml -> xaml, x)
            )

    let filesToItemGroup xaml (fsFiles: string list) = 

        let fsFiles = 
            fsFiles
            |> Converter.orderFiles

        let xamlFiles = xamlPaired xaml fsFiles 
        let fsFiles = fsFiles |> List.filter (fun x -> xamlFiles |> List.map snd |> List.contains x |> not)

        [
            yield! fsFiles |> List.map fsXmlCompile
            yield! xamlFiles |> List.collect xmalPairToXml 
        ] |> asItemGroup 

    let lines = 
        File.ReadAllLines coreProj
        |> Array.toList

    let firstProperty = lines |> List.findIndex (fun x -> x.Contains "</PropertyGroup>")

    lines 
    |> List.mapi  (fun i x -> 
        if i = firstProperty then 
            [
                yield x
                yield! filesToItemGroup xaml fsFiles
            ]
        else [x] )
    |> List.collect id
    |> fun x -> File.WriteAllLines (coreProj, x)

let convertFileEndingsAndReferences rootPath = 
    let updateSolutionFile () = 
        match rootPath |> getSlnFile with 
        | [] -> printfn "No Solution file found"
        | [x] -> renameContentsSoultionFile x
        | _ -> printfn "Muliple solution files found. Aborting since this case is unknown."

    let updateAndroidProj () = 
        rootPath 
        |> findFiles (fun x -> Path.GetExtension x = ".fsproj") 
        |> Android.getAndroidProjFile
        |> function
        | Some androidProj -> 
            Android.updateAndroidProjFile androidProj
            Android.updateProjectJsonFile androidProj
            Android.removeCsResourceFile androidProj

        | None -> 
            printfn "No (or multiple) Android proj file found."        

    updateSolutionFile()
    renameProjFiles rootPath
    renameContentsFsprojFiles rootPath
    renameCsFiles rootPath
    updateAndroidProj ()
    getCoreProj rootPath |> List.iter updateCoreProj


let path = "/Users/sam.williams/xamarin-forms-samples/GetStarted/FirstApp/"

let convertXamarinApp path = 
    convertFileEndingsAndReferences path

    findFsFiles path
    |> List.iter (fun x -> 
        printfn "Converting to F#: %s" <| Path.GetFileName x
        let input  = File.ReadAllText x
        let output = Converter.run input
        File.WriteAllText(x, output)
    )


