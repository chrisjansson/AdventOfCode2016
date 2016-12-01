let rotate (direction, distance) d =
    let directions = [| (0, 1) ; (1, 0); (0, -1); (-1, 0) |]
    let index = Array.findIndex (fun dir -> dir = d) directions
    directions.[(index + direction + directions.Length) % (directions.Length)]

let scale s (x, y)  =
    (x * s, y * s)

let add (x1, y1) (x2, y2) =
    (x1 + x2, y1 + y2)

let move (_, distance) (d, l) =
    add l (scale distance d)

let reduce (d, l) m  =
    let d = rotate m d
    let l = move m (d, l)
    (d, l)

let calculateManhattanDistance (x, y) =
    (abs x) + (abs y)

let trim (s:string) =
    s.Trim()

[<EntryPoint>]
let main argv = 

    let parseMovement (movementInput:string) =
        match (movementInput.[0], movementInput.[1..]) with
        | direction, distance ->
            let d = int32 distance
            match direction with
            | 'L' -> (-1, d)
            | 'R' -> (1, d)
            | _ -> failwith "Cannot parse movement"
        | _ -> failwith "Cannot parse movement"

    let startPosition = (0,1), (0,0)

    let result = argv.[0].Split([| ',' |]) 
                        |> Array.map (trim >> parseMovement)
                        |> Array.fold reduce startPosition
                        |> snd
                        |> calculateManhattanDistance
    0
