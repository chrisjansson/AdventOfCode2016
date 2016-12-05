module HowAboutaNiceGameofChess

let private md5 = System.Security.Cryptography.MD5.Create()

let inline private  hash (s:string) =
    let buf = System.Text.Encoding.ASCII.GetBytes(s)
    md5.ComputeHash(buf)

let inline private isInteresting (bytes:byte array) =
    bytes.[0] = 0uy && bytes.[1] = 0uy && (bytes.[2] &&& 0xF0uy) = 0uy 

// let calculateHash s = seq {
//         let mutable count = 0
//         while count < 8 do
//             for i = 0 to System.Int32.MaxValue do
//                 let hashed = (s + i.ToString(System.Globalization.CultureInfo.InvariantCulture)) |> hash
//                 if (hashed |> isInteresting) then
//                     yield (i, hashed)
//                     count <- count + 1
//     }

let calculateHash s = 
    let rec calculateHashPrivate (i, c, acc) =
        if c >= 8 then
            (i, c, acc)
        else
            let hashed = (s + i.ToString()) |> hash
            if (hashed |> isInteresting) then
                calculateHashPrivate (i+1, c+1, hashed :: acc)
            else
                calculateHashPrivate (i+1, c, acc)

    calculateHashPrivate (0, 0, [])


let main input =


    let result = input 
                    |> calculateHash 
                    |> (fun (_, _, hashes) -> hashes)
                    |> List.map (fun hash -> (hash.[2]).ToString("X1"))
                    |> List.rev
                    |> List.fold (+) ""

    [
        sprintf "%A" result
    ]