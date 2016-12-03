let splitLines (s:string) =
    s.Split([| System.Environment.NewLine |], System.StringSplitOptions.RemoveEmptyEntries)

let parseButtonSequence sequence =
    let parseMovement m =
        match m with
        | 'U' -> (0, -1)
        | 'D' -> (0, 1)
        | 'L' -> (-1, 0)
        | 'R' -> (1, 0)
        | _ -> failwith "unrecognized movement"

    sequence |> Seq.map parseMovement
let clamp min max value =
    if value < min then
        min
    else if value > max then
        max
    else 
        value
let clampTuple (minx, miny) (maxx, maxy) (x, y) =
    (clamp minx maxx x, clamp miny maxy y)
let add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

let reduceButtonSequence s start =
    let fold s m =
        add s m |> clampTuple (0, 0) (2, 2)
    s |> Seq.fold fold start

let reduceButtonSequences s =
    let fold buttons buttonSequence =
        let pos = List.head buttons
        let newPos = reduceButtonSequence buttonSequence pos
        newPos :: buttons
    let initialState = [(1,1)] 
    s |> Seq.fold fold initialState |> Seq.rev 

let translateToButton (x,y) =
    (y * 3 + x) + 1

[<EntryPoint>]
let main argv =
    
    let result = argv.[0] |> splitLines
                        |> Array.map parseButtonSequence
                        |> reduceButtonSequences
                        |> Seq.map translateToButton
    
    for k in result do
        printf "%A" k
    0
