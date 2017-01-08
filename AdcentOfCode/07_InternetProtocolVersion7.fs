module InternetProtocolVersion7

let private addressRegex = System.Text.RegularExpressions.Regex("(([^\[\]]\w+[^\[\]])|([\[\]]\w+[\[\]]))")

type AddressPart =
    | NoBrackets of string
    | Brackets of string

let parseAddress addressString =
    let groupCollectionToSequence (mc:System.Text.RegularExpressions.MatchCollection) = seq {
        for i = 0 to (mc.Count - 1) do
            let gc = mc.[i].Groups
            for j = 0 to (gc.Count - 1) do
                let value = gc.[j].Value
                if value <> "" then
                    yield value
    }

    let parsePart (p:string) =
        if p.[0] = '[' then
            Brackets (p.[1..(p.Length - 1)])
        else
            NoBrackets p

    let matches = addressRegex.Matches(addressString)
    matches |> groupCollectionToSequence |> Seq.map parsePart
 
let rec hasAbba (addressPart:string) =
    let isAbba (s:string) = s.[0] = s.[3] && s.[1] = s.[2] && s.[0] <> s.[1]
    if addressPart.Length < 4 then
        false
    else if isAbba addressPart then
        true
    else 
        hasAbba addressPart.[1..]

let private isValidAddress parts =
    let inBrackets part =
        match part with
        | Brackets _ -> true
        | _ -> false
    
    let inBrackets, notInBrackets = parts |> partition inBrackets

    inBrackets |> Seq.forall (fun (Brackets s) -> hasAbba s |> not) &&
    notInBrackets |> Seq.exists (fun (NoBrackets s) -> hasAbba s)

let main input =
    let result = input 
                    |> System.IO.File.ReadAllLines
                    |> Array.filter (parseAddress >> isValidAddress)
                    |> (fun a -> a.Length) |> (sprintf "%A")

    [
        result
    ]

    // [
        
    //         //|> Array.map (sprintf "%A")
    //         //|> Array.map (parseAddress >> isValidAddress) 
    //         // |> Array.filter id
    //         // |> (fun a -> sprintf "%A" a.Length)     
    // ]