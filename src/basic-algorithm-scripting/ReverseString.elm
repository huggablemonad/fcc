module ReverseString
  ( main, reverseString
  ) where

{-| # Reverse a String
----------------------

Reverse the provided string.

This is a certification challenge.

<https://www.freecodecamp.com/challenges/reverse-a-string>

# Core functions
@docs main, reverseString

-}

import Graphics.Element exposing (Element)
import List
import String
import ElmTest exposing (..)


{-| Main entry point.

This just runs the tests.
-}
main : Element
main = elementRunner testSuite


{-| Reverse the provided string.

    reverseString "hello" == "olleh"
    reverseString "Howdy" == "ydwoH"
    reverseString "Greetings from Earth" == "htraE morf sgniteerG"
-}
reverseString : String -> String
reverseString string = String.reverse string


{-| Test suite for `ReverseString`. -}
testSuite : Test
testSuite = suite "ReverseString" [testReverseString]


{-| Test suite for `reverseString`. -}
testReverseString : Test
testReverseString =
  let f (str, expected) =
        let name = "input: " ++ toString str
            actual = reverseString str
        in test name <| assertEqual expected actual
      inputs = [ ("hello", "olleh")
               , ("Howdy", "ydwoH")
               , ("Greetings from Earth", "htraE morf sgniteerG")
               ]
      tests = List.map f inputs
  in suite "reverseString" tests
