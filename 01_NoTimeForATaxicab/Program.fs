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

let split c (s:string) =
    s.Split([| c |])

type PathingState = 
    {
        FirstDoubleLocation : (int * int) option
        Locations : (int * int) list
        State : ((int * int) * (int * int))
    }
let interpolate (x1, y1) (x2, y2) =
    let interpolateComponent p0 p1 =
        if p1 > p0 then
            [p0..p1]
        else
            [p1..p0] |> List.rev

    if x1 <> x2 then
        interpolateComponent x1 x2
            |> List.map (fun x -> (x, y1))
    else
        interpolateComponent y1 y2
            |> List.map (fun y -> (x1, y))

let pathingReducer s m =
        let reduceLocal (locations, doubleVisit) newLocation =
            let firstDoubleVisit = 
                    match doubleVisit with
                    | Some l ->
                        Some l
                    | None -> 
                        if List.contains newLocation locations then
                            Some newLocation
                        else
                            None
            (locations @ [ newLocation ], firstDoubleVisit)
    
        let newDir, newLoc = reduce s.State m
        let _, currentLocation = s.State
        let newLocations = interpolate currentLocation newLoc |> List.skip 1
        
        let ls, dl = newLocations |> List.fold reduceLocal (s.Locations, s.FirstDoubleLocation)
        { FirstDoubleLocation = dl; Locations = ls; State = (newDir, newLoc) }


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
    let initialState = {
            FirstDoubleLocation = None
            Locations = [ snd startPosition ]
            State = startPosition
        }

    let result = argv.[0]
                    |> split ','
                    |> Array.map (trim >> parseMovement)
                    |> Array.fold pathingReducer initialState

    printfn "Distance: %A" (result.State |> snd |> calculateManhattanDistance)
    printfn "First doubly visied location: %A" result.FirstDoubleLocation
    printfn "Distance to first doubly visited location: %A" (result.FirstDoubleLocation.Value |> calculateManhattanDistance)
    0
