[<EntryPoint>]
let main argv =
    let puzzleSolvers = 
        Map.empty.
            Add("01", NoTimeForATaxicab.main).
            Add("02", BathroomSecurity.main).
            Add("03", SquaresWithTreeSides.main).
            Add("04", SecurityThroughObscurity.main).
            Add("05", HowAboutaNiceGameofChess.main).
            Add("06", SignalsAndNoise.main).
            Add("07", InternetProtocolVersion7.main)

    let solver = puzzleSolvers.[argv.[0]]
    for line in (argv.[1] |> solver) do
        printfn "%s" line
    0
