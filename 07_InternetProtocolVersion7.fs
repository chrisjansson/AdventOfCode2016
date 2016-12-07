module InternetProtocolVersion7

let private addressRegex = System.Text.RegularExpressions.Regex("^(.*?)\[(.*?)\](.*?)$")

let parseAddress addressString =
    let m = addressRegex.Match(addressString)
    (m.Groups.[1].Value, m.Groups.[2].Value, m.Groups.[3].Value)

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
    [
        input 
            |> System.IO.File.ReadAllLines
            |> Array.map (parseAddress >> isValidAddress) 
            |> Array.filter id
            |> (fun a -> sprintf "%A" a.Length)     
    ]