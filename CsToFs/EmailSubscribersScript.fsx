#r "/Users/sam.williams/.nuget/packages/newtonsoft.json/11.0.2/lib/net45/Newtonsoft.Json.dll"

open Newtonsoft.Json
open System.Collections.Generic

let s = """ [{"Blaine Holt": 415609, "Celso12ssssssss Dolend": 78691}] """

let main () = 

    let driverLookup driver (dict:IDictionary<string,int>) = 
        dict.[driver]

    let r = JsonConvert.DeserializeObject<seq<IDictionary<string,int>>> s
    let drivers = Seq.head r
    let driverNames = drivers.Keys

    let blaine = driverLookup "Blaine Holt" drivers

    driverNames, blaine




open System
open System.IO

let captalize (xs:string) = 
    match xs |> Seq.toList with 
    | x::xs -> sprintf "%c%s" (Char.ToUpper(x)) (xs |> List.map Char.ToLower |> List.map string |> String.concat "")
    | x -> x |> List.map string |> String.concat ""

let cleanName (a:string) = 
    a.Split(' ') |> Array.map captalize |> Array.toList

let firstAndLast xs = match xs with [a;b;c;d;e] -> Some (a,e) | _ -> printfn "Error, not enough elements"; None

let writeToFile (lines: (string * string * string) list) = 
    let file = "/Users/sam.williams/Desktop/subs.csv"

    let data = 
        lines
        |> List.map (fun (a,b,c) -> 
            match a,b,c with 
            |  "", "", "" -> ""
            |  a, b, "" -> sprintf "%s,%s,\"\"" a b 
            |  a, b, c -> sprintf "%s,%s,%s" a b c )

    File.WriteAllLines (file, data)

let processLines (lines: string[]) = 

    let header = "Email", "First Name", "Last Name"

    let result = 
        lines 
        |> Array.skip 1
        |> Array.map (fun x -> 
            x.Split(',')
            |> Array.toList 
            |> firstAndLast) 
        |> Array.toList
        |> List.choose id
        |> List.map (fun (x,y) -> x, cleanName y)
        |> List.map (fun (x,y) -> 
            match y with 
            | [a;b;c] -> printfn "Special case"; x, (a + b), c
            | [a;b] -> x, a,b
            | [a] -> x, a, ""
            | [] -> "", "","" 
            | y::xs -> 
                printfn "Lots of cases here: %A" xs; 
                x, y, (List.last xs) )

    header :: result



