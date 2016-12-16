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

module Operators =

    let composeAnd p0 p1 s =
        match run p0 s with
        | Ok (result1, remaining1) ->
            match run p1 remaining1 with
            | Ok (result2, remaining2) ->
                Ok ((result1, result2), remaining2)
            | Error m -> Error m
        | Error m -> Error m

    [<Fact>]
    let ``Combines the result of two parsers``() =
        let p0 = pChar 'a'
        let p1 = pChar 'b'
        let parser = composeAnd p0 p1
        let result = parser "abc"
        let expected = Ok (('a', 'b'), "c")
        Assert.Equal(expected, result)

    [<Fact>]
    let ``Fails when the first parser fails`` () =
        let p0 = pChar 'a'
        let p1 = pChar 'b'
        let parser = composeAnd p0 p1
        let result = parser "dbc"
        let expected = Error "dbc"
        Assert.Equal(expected, result)

    [<Fact>]
    let ``Fails when the second parser fails`` () =
        let p0 = pChar 'a'
        let p1 = pChar 'b'
        let parser = composeAnd p0 p1
        let result = parser "adc"
        let expected = Error "dc"
        Assert.Equal(expected, result)

    let composeOr (p0:Parser<'T>) (p1:Parser<'T>) s =
        match run p0 s with
        | Error _ -> 
            run p1 s
        | r -> r

    [<Fact>]
    let ``Picks the first result when it succeeds`` () =
        let p0 = pChar 'a'
        let p1 = pChar 'b'
        let parser = composeOr p0 p1
        let result = parser "abc"
        let expected = Ok ('a', "bc")
        Assert.Equal(expected, result)


    [<Fact>]
    let ``Picks the second result when the first fails and the second succeeds`` () =
        let p0 = pChar 'a'
        let p1 = pChar 'b'
        let parser = composeOr p0 p1
        let result = parser "bcd"
        let expected = Ok ('b', "cd")
        Assert.Equal(expected, result)

    [<Fact>]
    let ``Fails when both fails`` () =
        let p0 = pChar 'a'
        let p1 = pChar 'b'
        let parser = composeOr p0 p1
        let result = parser "cde"
        let expected = Error "cde"
        Assert.Equal(expected, result)