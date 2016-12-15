module Parser

open Xunit

let pChar char (s:string) =
    if System.String.IsNullOrEmpty(s) then
        Error ""
    else if s.[0] = char then
        let remaining = s.[1..]
        Ok (char, remaining)
    else
        Error s
    

[<Fact>]
let ``Parses single character``() =
    let result = pChar 'a' "abc"
    let expected = Ok ('a', "bc")
    Assert.Equal(expected, result)


[<Fact>]
let ``Fails parsing non matching charater``() =
    let result = pChar 'x' "abc"
    let expected = Error "abc"
    Assert.Equal(expected, result)


[<Fact>]
let ``Fails parsing empty string``() =
    let result = pChar 'x' ""
    let expected = Error ("")
    Assert.Equal(expected, result)

module Operators =

    let compose p0 p1 s =
        match p0 s with
        | Ok (result, remaining) ->
            match p1 remaining with
            | Ok (result2, remaining2) ->
                Ok ((result, result2), remaining2)
            | Error m -> Error m
        | Error m -> Error m

    [<Fact>]
    let ``Combines the result of two parsers``() =
        let p0 = pChar 'a'
        let p1 = pChar 'b'
        let parser = compose p0 p1
        let result = parser "abc"
        let expected = Ok (('a', 'b'), "c")
        Assert.Equal(expected, result)

    [<Fact>]
    let ``Fails when the first parser fails`` () =
        let p0 = pChar 'a'
        let p1 = pChar 'b'
        let parser = compose p0 p1
        let result = parser "dbc"
        let expected = Error "dbc"
        Assert.Equal(expected, result)

    [<Fact>]
    let ``Fails when the second parser fails`` () =
        let p0 = pChar 'a'
        let p1 = pChar 'b'
        let parser = compose p0 p1
        let result = parser "adc"
        let expected = Error "dc"
        Assert.Equal(expected, result)