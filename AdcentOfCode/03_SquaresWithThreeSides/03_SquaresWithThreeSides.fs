module SquaresWithTreeSides

let splitLines (s:string) = s.Split([| System.Environment.NewLine |], System.StringSplitOptions.RemoveEmptyEntries)

let split (s:string) = s.Split([|' '|], System.StringSplitOptions.RemoveEmptyEntries)

let trim (s:string) = s.Trim()

let parseTriangleSides = Array.map (trim >> int)

let isValidTriangle sides =
    match sides with
    | [| s1; s2; s3 |] -> (s1 + s2) > s3 && (s2 + s3) > s1 && (s3 + s1) > s2
    | _ -> failwith "Not a triangle"

let main input =
    let result = input |> splitLines
                            |> Array.map split
                            |> Array.map parseTriangleSides
                            |> Array.map isValidTriangle
                            |> Array.filter (fun isValid -> isValid)
    [
        sprintf "%A" result.Length
    ]
