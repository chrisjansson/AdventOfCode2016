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