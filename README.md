# FShaper
FShaper - C# to F# without the repetition and pain.

## Purpose 

There should be very little 'cost' (measured in time) changing language to F#, while observing significant benefits.

F# and c# interop is very good. In most cases, wrap the C# code in a project and call from F# (or vice versa). If the C# code is being removed/replaced, then it is better to rewrite to an FP style. 

This project is for the remaining C#. Where there is an observable cost moving F# with little benefit. 

Some domains do not allow for an FP style particularly those around UI such as Xamairn (iOS, Android, WPF, etc). Native APIs in Xamarin will always be OOP as they are controlled by Apple and Google. 

Documentation/blog posts is another area where there is a lot of C# code, and to quickly test out the idea, the developer must first translate the code to F# (if a C# project was used, simple copy/paste would be all that is required). 

FShaper reduces the time to do this translation, and improves the code quality slightly in the process. 

## Usage 

copy C# code, pipe into the program 

on a Mac `pbpaste | dotnet run | pbcopy`<br>
FShaper can be installed a dotnet global tool. 

`cd FShaper`<br>
`./build.sh && ./install.sh`<br>
`pbpaste | fshaper | pbcopy`<br>


## Removing/Minimizing the pain

This project uses the untyped abstract syntax tree of Roslyn. Because of that it is not possible to generate perfect F# code. Addtionally, to support the goals above (blog posts) valid C# code is not expected would not compile (convert a method, that does not have a class or the required nuget packages declared). 

A best effort must be taken to solve most problems, or produce and F# output that is close enough. 

## Work to be done - current state alpha

approx confidence converting a random piece of code - 30%

To ensure this project is a success, the hardest conversions needed to be solved first. There are a couple still remaining. See the issues for remaining tasks. 

Once it is clear that the most difficult challenges have been solved, completing the Roslyn api to F# syntax remains which should be most straight forward, which will raise the confidence substantially. 

## Contributing

Check if you problem has been raised as and issue fist. if not raise and issue, for simple fixes a supporting PR would be great. 

### Building locally

Install FAKE:
    dotnet tool install -g fake-cli
    
then run the fake script, it will clean, build and execute tests

    cd FShaper
    fake build

## End Goal 
To have this project embedded in either/all:<br>
Visual Studio <br>
Visual Studio Code (with Ionide)<br>
Web tool<br>
As browser plugin that automatically converts C# code to F# <br>

