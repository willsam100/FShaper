rm -rf nupkg
rm -rf .bin/
rm -rf .obj/
dotnet publish && dotnet pack -c release -O nupkg
