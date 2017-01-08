module Tests

open Xunit
open Parser
open Parser.Combinators

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
    
    let expected = Ok(['a';'b';'c'], "d")
    Assert.Equal(expected, result)

[<Fact>]
let ``Parses matching string``() =
    let parser = pString "Hello world"
    let result = run parser "Hello world abc"
    printfn "%A" result
    let expected = Ok("Hello world", " abc")
    Assert.Equal(expected, result)

[<Fact>]
let ``Does not parse non matching string``() =
    let parser = pString "Hello world"
    let result = run parser "Hell world"
    let expected = Error("Hell world")
    Assert.Equal(expected, result)