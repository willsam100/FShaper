# FShaper
FShaper - C# to F# without the repetition and pain.

## Purpose 

There should be very little 'cost' (measured in time) changing language to F#, while obversing significant benefits.

F# and c# interop is very good. In most cases, wrap the C# code in a project and call from F# (or vice versa). If the C# code is being removed/replaced, then it is better to rewrite to an FP style. 

This project is for the remaining C#. Where there is an observable cost moving F# with little beneift. 

Some domains do not allow for an FP style particularly those around UI such as Xamairn (iOS, Android, WPF, etc). Native APIs in Xamarin will always be OOP as they are controlled by Apple and Google. 

Documentation/blog posts is another area where there is a lot of C# code, and to quickly test out the idea, the developer must first translaste the code to F# (if a C# project was used, simple copy/paste would be all that is required). 

FSharper reduces the time to do this translation, and improves the code quality slighly in the process. 

## Removing/Minimising the pain

This project uses the untyped abstract syntax tree of Roslyn. Because of that it is not possible to generate perfect F# code. Addtionally, to support the goals above (blog posts) valid C# code is not expected would not compile (convert a method, that does not have a class or the required nuget packages declared). 

A best effort must be taken to solve most problems, or produce and F# output that is close enough. 

## Work to be done - current state alpha

approx confidence converting a random peice of code - 30%

To ensuer this project is a success, the hardest conversions needed to be solved first. There are a couple still remaining. See the issues for remaining tasks. 

Once it is clear that the most difficult chanllenges have been solved, completing the Roslyn api to F# syntax remains which should be most straigt forward, which will raise the confidence substanially. 

## Contributing

Check if you problem has been raised as and issue fist. if not raise and issue, for simple fixes a supporting PR woudl be great. 

## End Goal 
To have this project embedded in either/all:
Visual Studio 
Visual Stuido Code (with Ionide)
Web tool
As browser plugin that automatically converts C# code to F# 

