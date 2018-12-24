module ProgramPrinter
open CsToFs

let private addIndents ind xs = xs |> List.map (fun x -> ind + x)
let private addIndentsLvl2 ind xs = xs |> List.map (fun (Line x) -> ind + ind + x)
let private addIndentsLvl3 ind xs = xs |> List.map (fun (Line x) -> ind + ind + ind + x)

let printParameters (xs: Parameter list) = 
    match xs with 
    | [] -> "()"
    | xs -> 
        let args = xs |> List.map (fun x -> x.Name + ":" + x.Type) |> String.concat ","
        "(" + args + ")"


let printMethod indent (m:Method) = 

    let args = m.Parameters |> printParameters
    
    let prefix = 
        match m.IsOverride, m.IsPrivate with 
        | true, _ -> "override this."
        | false, true -> "member private this."
        | false, false -> "member this."
        
    seq {
        yield indent + prefix + m.Name + args + " = "
        yield! (m.Body |> addIndentsLvl2 indent)
        yield ""
    }

let printProp indent (p:Prop) = 

    match p.Get, p.Set with 
    | [], [] -> 
        seq {
            yield indent + "member val " + p.Name + ":" + p.Type + " = null with get, set // TODO :: assuming both getter and setter"
            yield "" }
    | _, _  -> 
        let getter = 
            let prefix = indent + indent + "with get() = "
            match p.Get with 
            | [] -> Seq.empty
            | (Line x)::[] -> prefix + x |> Seq.singleton
            | xs -> 
                seq {
                    yield prefix
                    yield! xs |> addIndentsLvl3 indent
                }

        let setter = 
            let prefix = indent + indent + "and set(value) = "
            match p.Set with 
            | [] -> Seq.empty
            | (Line x)::[] -> prefix  + x |> Seq.singleton
            | xs -> 
                seq {
                    yield prefix 
                    yield! xs |> addIndentsLvl3 indent
            }

        seq {
            yield indent + "member this." + p.Name 
            yield! getter 
            yield! setter
            yield ""
        }

let printField indent f = 
    let line = 
        match f.Initilizer with 
        | Some init -> indent + "let " + f.Name + " = " + init
        | None -> indent + "let " + f.Name + ":" + f.Type + " = null // TODO:: replace with default value for type"

    seq {
        if f.IsPublic then yield "// TODO:: C# declared this as public. F# does not support a public field"
        yield line } 

let printAttribute (x:Attribute) = 
    let prefix = "[<" + x.Name
    let suffix =  ">]"
    match x.Parameters with 
    | Some ps -> prefix + ps + suffix
    | None -> prefix  + suffix
    
let printClass indent (x:Class) = 
    if List.length x.Constructors > 1 then 
        printfn "Mulitple ctors for %s, this is not implemented" x.Name.Name

    let ctor = 
        match x.Constructors with 
        | [] -> None
        | xs -> xs |> List.maxBy (fun x -> x.Parameters |> List.length) |> Some

    let ctorArgs = ctor |> Option.map (fun x -> x.Parameters |> printParameters) |> Option.defaultValue "()"
    let methods = x.Methods |> List.map (printMethod indent) |> Seq.concat
    let props = x.Properties |> Seq.map (printProp indent) |> Seq.concat

    let attributes = x.Attributes |> Seq.map (printAttribute)

    let typeParameters = 
        match x.TypeParameters with 
        | [] -> ""
        | xs -> 
            let fTypePaarams = 
                xs
                |> List.map (fun x -> "'" + x)
                |> String.concat ","

            "<" + fTypePaarams + ">"

    let subclass = 
        x.BaseClass 
        |> Option.map (fun baseClass -> 
            let args = 
                ctor 
                |> Option.map (fun ctor -> ctor.SubclassArgs |> String.concat"," )
                |> Option.map (fun args -> "(" + args + ")")
                |> Option.defaultValue "()"
            indent + "inherit " + baseClass + args )
        |> function
        | None -> seq {yield ""}
        | Some baseClass -> seq { yield baseClass; yield "" }

    let interfaces = 
        match x.ImplementInterfaces with 
        | [] -> Seq.empty
        | xs -> 
            xs |> Seq.map (fun i -> 
                seq {
                    yield (indent + "interface " + i + " with")
                    yield ""
                }) 
            |> Seq.concat

    let ctorBodies = 
        x.Constructors 
        |> List.map (fun ctor -> 

            if List.length ctor.Body >= 1 then 
                seq {
                    yield indent +  "do"
                    yield! ctor.Body |> addIndentsLvl2 indent
                    yield "" }
            else Seq.empty)
        |> Seq.concat

    let fields = 
        x.Fields 
        |> List.map (printField indent)
        |> Seq.concat
        

    seq {
        yield! attributes
        yield ("type " + x.Name.Name + typeParameters + ctorArgs + " = ")
        yield! subclass
        yield! fields
        yield! ctorBodies
        yield! methods
        yield! props
        yield! interfaces
    }


let printInterface indent (i:Interface) = 

    seq {
        yield "type " + i.Name + " ="
        yield! i.Methods
    }

let printUsingStatement us = 
    "open " + us.Namespace

let printNamespace indent usingStatements ns = 
    seq {
        yield "namespace " + ns.Name
        yield! usingStatements |> Seq.map (printUsingStatement)
        yield ""
        yield! ns.Interfaces |> Seq.map (printInterface indent) |> Seq.concat
        yield! ns.Classes |> Seq.map (printClass indent) |> Seq.concat
    }


let printFile indent file = 
    seq {
        yield! file.Namespaces |> Seq.map (printNamespace indent file.UsingStatements) |> Seq.concat
        yield ""
    }

let prettyPrint indent = function 
| File f -> printFile indent f
| Namespace ns -> printNamespace indent Seq.empty ns
| UsingStatement us -> printUsingStatement us |> Seq.singleton
| Interface i -> printInterface indent i
| Class c -> printClass indent c
| Field f -> f |> Seq.map (printField indent) |> Seq.concat
| Prop p -> printProp indent p
| Method m -> printMethod indent m
| Empty -> Seq.empty