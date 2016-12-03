let splitLines (s:string) = s.Split([| System.Environment.NewLine |], System.StringSplitOptions.RemoveEmptyEntries)

let split (s:string) = s.Split([|' '|], System.StringSplitOptions.RemoveEmptyEntries)

let trim (s:string) = s.Trim()

let parseTriangleSides = Array.map (trim >> int)

let isValidTriangle sides =
    match sides with
    | [| s1; s2; s3 |] -> (s1 + s2) > s3 && (s2 + s3) > s1 && (s3 + s1) > s2
    | _ -> failwith "Not a triangle"

[<EntryPoint>]
let main argv =
    let result = argv.[0] |> splitLines
                            |> Array.map split
                            |> Array.map parseTriangleSides
                            |> Array.map isValidTriangle
                            |> Array.filter (fun isValid -> isValid)
    printfn "%A" result.Length
    0
