module InternetProtocolVersion7

let private addressRegex = System.Text.RegularExpressions.Regex("(([^\[\]]\w+[^\[\]])|([\[\]]\w+[\[\]]))")

let parseAddress addressString =
    let groupCollectionToSequence (mc:System.Text.RegularExpressions.MatchCollection) = seq {
        for i = 0 to (mc.Count - 1) do
            let gc = mc.[i].Groups
            for j = 0 to (gc.Count - 1) do
                let value = gc.[j].Value
                if value <> "" then
                    yield value
    }

    let matches = addressRegex.Matches(addressString)
    matches |> groupCollectionToSequence

let rec hasAbba (addressPart:string) =
    let isAbba (s:string) = s.[0] = s.[3] && s.[1] = s.[2] && s.[0] <> s.[1]
    if addressPart.Length < 4 then
        false
    else if isAbba addressPart then
        true
    else 
        hasAbba addressPart.[1..]

let private isValidAddress (p0, p1, p2) =
    match (hasAbba p0 || hasAbba p2, hasAbba p1) with
    | (true, false) -> true
    | _ -> false

let main input =
    input 
            |> System.IO.File.ReadAllLines
            |> Array.map parseAddress
            |> Array.map (sprintf "%A")
            |> Array.toList
    
    // [
        
    //         //|> Array.map (sprintf "%A")
    //         //|> Array.map (parseAddress >> isValidAddress) 
    //         // |> Array.filter id
    //         // |> (fun a -> sprintf "%A" a.Length)     
    // ]