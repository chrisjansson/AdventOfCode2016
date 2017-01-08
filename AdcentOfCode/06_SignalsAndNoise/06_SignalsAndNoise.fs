module SignalsAndNoise

let private initialColumnState = 
    ['a'..'z'] 
        |> List.map (fun c -> (c, 0))
        |> Map.ofList

let private incrementCountForCharacter ((s:Map<char, int>),c) =
    let value = s.[c]
    s.Add(c, value + 1)

let private sumCode characterCount code = Seq.zip characterCount code |> Seq.map incrementCountForCharacter

let private getCharacterPerPositions sorter characterCounts =
    let getFirstCharacter position = position 
                                    |> Map.toSeq 
                                    |> sorter
                                    |> Seq.take 1 
                                    |> Seq.exactlyOne
    characterCounts |> Seq.map getFirstCharacter  

let main input =
    let state = Seq.init 8 (fun _ -> initialColumnState)
    let result1 = input
                    |> splitLines
                    |> Array.map trim
                    |> Array.fold sumCode state 
                    |> getCharacterPerPositions (fun s -> s |> Seq.sortByDescending (fun (c, count) -> count))
                    |> Seq.map (fst >> string)
                    |> Seq.fold (+) ""
    let result2 = input
                    |> splitLines
                    |> Array.map trim
                    |> Array.fold sumCode state 
                    |> getCharacterPerPositions (fun s -> s |> Seq.sortBy (fun (c, count) -> count))
                    |> Seq.map (fst >> string)
                    |> Seq.fold (+) ""
    [
        result1
        result2
    ]