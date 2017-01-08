module BathroomSecurity

let keypad = [
        [| '1'; '2'; '3'; |]
        [| '4'; '5'; '6'; |]
        [| '7'; '8'; '9'; |]
    ]

let keypad2 = [
        [| '-'; '-'; '1'; '-'; '-' |]
        [| '-'; '2'; '3'; '4'; '-' |]
        [| '5'; '6'; '7'; '8'; '9' |]
        [| '-'; 'A'; 'B'; 'C'; '-' |]
        [| '-'; '-'; 'D'; '-'; '-' |]
    ]

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

let clampTuple keypad old (x, y) =
    let maxx = (Array2D.length1 keypad) - 1
    let maxy = (Array2D.length2 keypad) - 1
    let cx, cy = (clamp 0 maxx x), (clamp 0 maxy y)
    if keypad.[cy, cx] = '-' then
        old
    else 
        cx, cy
let add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

let reduceButtonSequence keypad s start =
    let fold s m =
        add s m |> clampTuple keypad s
    s |> Seq.fold fold start

let reduceButtonSequences keypad s =
    let fold buttons buttonSequence =
        let pos = List.head buttons
        let newPos = reduceButtonSequence keypad buttonSequence pos
        newPos :: buttons
    let initialState = [(1,1)] 
    s |> Seq.fold fold initialState |> Seq.rev 

let translateToButton (keypad:_[,]) (x,y) =
    keypad.[y, x]

let main input =

    let result = input |> splitLines
                        |> Array.map parseButtonSequence
                        |> reduceButtonSequences (array2D keypad)
                        |> Seq.skip 1
                        |> Seq.map (translateToButton (array2D keypad))
                        |> (fun s -> System.String.Concat(Array.ofSeq s))

    let result2 = input |> splitLines
                        |> Array.map parseButtonSequence
                        |> reduceButtonSequences (array2D keypad2)
                        |> Seq.skip 1
                        |> Seq.map (translateToButton (array2D keypad2))
                        |> (fun s -> System.String.Concat(Array.ofSeq s))
    
    [
        sprintf "%A" result
        sprintf "%A" result2
    ]

