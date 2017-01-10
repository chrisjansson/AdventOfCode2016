module Parser

type Parser<'T> = Parser of (string -> Result<'T * string, string>)

let run p s =
    let (Parser fn) = p
    fn s
let pChar char =
    let innerFn s =
        if System.String.IsNullOrEmpty(s) then
            Error ""
        else if s.[0] = char then
            let remaining = s.[1..]
            Ok (char, remaining)
        else
            Error s
    Parser innerFn
    
let returnP x =
    let inner s =
        Ok (x, s)
    Parser (inner)

let mapP f p =
    let inner s =
        match run p s with
        | Ok (res, r) -> Ok(f res, r)
        | Error m -> Error m
    Parser inner

let applyP pf pa =
    let pb s =
        match (run pf s) with
        | Ok(f, rem1) ->
            match (run pa rem1) with
            | Ok(a, rem2) ->
                Ok (f a, rem2)
            | Error e -> Error e
        | Error e -> Error e
    Parser pb

let lift2 f pa =
    applyP (applyP (returnP f) pa)

let rec inSequence parsers =
    let append i l = i :: l  
    let appendP = lift2 append
    let rec inner parsers acc =
        match parsers with
        | [] -> acc
        | head::tail ->
            let acc2 = appendP head acc
            inner tail acc2
    inner (parsers |> List.rev) (returnP [])

let pString (s:string) =
    Seq.map pChar s
        |> Seq.toList
        |> inSequence
        |> mapP string

module Combinators = 
    let composeAnd p0 p1 =
        let inner s =
            match run p0 s with
            | Ok (result1, remaining1) ->
                match run p1 remaining1 with
                | Ok (result2, remaining2) ->
                    Ok ((result1, result2), remaining2)
                | Error m -> Error m
            | Error m -> Error m
        Parser inner
        
    let composeAndLeft p0 p1 = 
        let inner s =
            match run (composeAnd p0 p1) s with
            | Ok ((r1, r2), remaining) -> Ok (r1, remaining)
            | Error m -> Error m
        Parser inner    

    let composeAndRight p0 p1 =
        let inner s =
                match run (composeAnd p0 p1) s with
                | Ok ((r1, r2), remaining) -> Ok (r2, remaining)
                | Error m -> Error m
        Parser inner

    let composeOr p0 p1 =
        let inner s =   
            match run p0 s with
            | Error _ -> 
                run p1 s
            | r -> r
        Parser inner
    
    let composeOrList (parsers:Parser<_> list) =
        let rec inner acc parsers =
            match parsers with
            | [] -> acc
            | parser :: tail -> inner (composeOr acc parser) tail
        inner (List.head parsers) (List.tail parsers)