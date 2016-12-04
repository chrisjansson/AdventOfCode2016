module SecurityThroughObscurity

let splitLines (s:string) = s.Split([| System.Environment.NewLine |], System.StringSplitOptions.RemoveEmptyEntries)

let split (c:char) (s:string) = s.Split([|c|], System.StringSplitOptions.RemoveEmptyEntries)

let parseRoom roomString =
    match split '[' roomString with
    | [| roomPart; checksumPart |] ->
         let roomSplit = split '-' roomPart
         let roomLetters = roomSplit.[0..(roomSplit.Length - 2)]
         let roomNumber = roomSplit.[roomSplit.Length - 1] |> int
         let [| checksum |] = split ']' checksumPart
         (roomLetters, roomNumber, checksum)
    | _ -> failwith "Invalid room string"

let isValidRoom (roomLetters : string seq, _, checksum : string) = 
    System.String.Concat(roomLetters)
        |> Seq.groupBy id
        |> Seq.sortBy (fun (k, _) -> k)
        |> Seq.sortByDescending (fun (_,seq) -> Seq.length seq)
        |> Seq.zip checksum
        |> Seq.take 5
        |> Seq.forall (fun (checksumChar, (k,_)) -> checksumChar = k)

let rot c times =
    let alphabet = [| 'a'..'z' |]
    let pos = Array.findIndex (fun l -> c = l) alphabet
    alphabet.[(pos + times) % alphabet.Length]

let rotateString (s:string) times =
    System.String.Join("", (s |> Seq.map (fun c -> rot c times) |> Seq.toArray))

let rotateRoomName parts times = 
    System.String.Join("-", parts |> Seq.map (fun s -> rotateString s times)) + (sprintf "-%A" times)

let main input =
    let result = input 
                    |> splitLines
                    |> Array.map parseRoom
                    |> Array.filter isValidRoom
                    |> Array.map (fun (roomName, number, _) -> number)
                    |> Array.sum 

    let result2 = input 
                    |> splitLines
                    |> Array.map parseRoom
                    |> Array.filter isValidRoom
                    |> Array.map (fun (roomName, number, _) -> rotateRoomName roomName number)
                    |> Array.toList
    result2 @ [ (sprintf "%A" result) ]
