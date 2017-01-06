module Parser

open Xunit

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

[<Fact>]
let ``Parses single character``() =
    let result = run (pChar 'a') "abc"
    let expected = Ok ('a', "bc")
    Assert.Equal(expected, result)


[<Fact>]
let ``Fails parsing non matching charater``() =
    let result = run (pChar 'x') "abc"
    let expected = Error "abc"
    Assert.Equal(expected, result)


[<Fact>]
let ``Fails parsing empty string``() =
    let result = run (pChar 'x') ""
    let expected = Error ("")
    Assert.Equal(expected, result)

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
        

    module Tests =
        
        let private parseA = pChar 'a'
        let private parseB = pChar 'b'

        [<Fact>]
        let ``Combines the result of two parsers``() =
            let parser = composeAnd parseA parseB
            let result = run parser "abc"
            let expected = Ok (('a', 'b'), "c")
            Assert.Equal(expected, result)

        [<Fact>]
        let ``Fails when the first parser fails`` () =
            let parser = composeAnd parseA parseB
            let result = run parser "dbc"
            let expected = Error "dbc"
            Assert.Equal(expected, result)

        [<Fact>]
        let ``Fails when the second parser fails`` () =
            let parser = composeAnd parseA parseB
            let result = run parser "adc"
            let expected = Error "dc"
            Assert.Equal(expected, result)

        [<Fact>]
        let ``Picks the first result when it succeeds`` () =
            let parser = composeOr parseA parseB
            let result = run parser "abc"
            let expected = Ok ('a', "bc")
            Assert.Equal(expected, result)

        [<Fact>]
        let ``Picks the second result when the first fails and the second succeeds`` () =
            let parser = composeOr parseA parseB
            let result = run parser "bcd"
            let expected = Ok ('b', "cd")
            Assert.Equal(expected, result)

        [<Fact>]
        let ``Fails when both fails`` () =
            let parser = composeOr parseA parseB
            let result = run parser "cde"
            let expected = Error "cde"
            Assert.Equal(expected, result)

        [<Theory>]
        [<InlineData("a42", 'a', "42")>]
        [<InlineData("b42", 'b', "42")>]
        [<InlineData("c42", 'c', "42")>]
        let ``Parses a or b or c`` (input:string, parsed:char, remaining:string) =
            let parseC = pChar 'c'
            let parser = composeOrList [ parseA; parseB; parseC ]
            let result = run parser input
            let expected = Ok (parsed, remaining)
            Assert.Equal(expected, result)

        [<Fact>]
        let ``Maps parser result with supplied function`` () =
            let parser = composeAnd parseA parseB
            let mapper (a:char, b:char) = [a;b]
            let result = run ((mapP mapper) parser) "abc"
            let expected = Ok(['a';'b'], "c")
            Assert.Equal(expected, result)

        [<Fact>]
        let ``Retuns constant regardless of input`` () =
            let parser = returnP 123
            let result = run parser "woot"
            let expected = Ok (123, "woot")
            Assert.Equal(expected, result)

        [<Fact>]
        let ``Apply elevated function to elevated value`` () =
            let add3 x = x + 3
            let parserToApply = returnP add3
            let parser = applyP parserToApply
            let parserValueToApply = mapP (string>>int) (pChar '5')
            let result = run (parser parserValueToApply) "5abc"
            let expected = Ok(8, "abc")
            Assert.Equal(expected, result)

        [<Fact>]
        let ``Follows applicative functor law f(x) elevated equals elevated x applied to elevated f`` () =
            let f x = x + 7
            let x = 5
            let y = f x
            let p1 = returnP y

            let pF = returnP f
            let pX = returnP x
            let p2 = (applyP pF) pX

            let res1 = run p1 ""
            let res2 = run p2 ""
            Assert.Equal(res1, res2)

        [<Fact>]
        let ``Lift2 lifts + operator``() =
            let addP = lift2 (+)
            let result = run (addP (returnP 1) (returnP 3)) ""
            let expected = Ok(4, "")
            Assert.Equal(expected, result)

        [<Fact>]
        let ``Parses characters in sequence``() =
            let p = inSequence [ parseA; parseB; (pChar 'c') ]
            let result = run p "abcd"
            printfn "%A" result
            let expected = Ok(['a';'b';'c'], "d")
            Assert.Equal(expected, result)