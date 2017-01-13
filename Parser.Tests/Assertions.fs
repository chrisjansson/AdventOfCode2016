module Assertions

open Xunit
open Xunit.Sdk

let assertEqual expected actual =
        if expected = actual then
            ()        
        else
            let message = sprintf "\nExpected:\n%A\n\nActual:\n%A\n\n" expected actual
            raise (new XunitException(message))

let (@=) expected actual = assertEqual expected actual