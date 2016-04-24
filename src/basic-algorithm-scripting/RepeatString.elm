module RepeatString
  ( main, repeatStringNumTimes
  ) where

{-| # Repeat a string repeat a string
-------------------------------------

Repeat a given string (first argument) `num` times (second argument). Return an
empty string if `num` is a negative number.

This is a certification challenge.

<https://www.freecodecamp.com/challenges/repeat-a-string-repeat-a-string>

# Core functions
@docs main, repeatStringNumTimes

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


{-| Repeat a given string (first argument) `num` times (second argument).
Return an empty string if `num` is a negative number.

    repeatStringNumTimes "*" 3 == "***"
    repeatStringNumTimes "abc" 3 == "abcabcabc"
    repeatStringNumTimes "abc" -2 == ""
    repeatStringNumTimes "abc" 0 == ""

The challenge doesn't say what to return if `num` is `0`, but both Elm's
`String.repeat` and Haskell's `Data.List.replicate` return an empty string.
-}
repeatStringNumTimes : String -> Int -> String
repeatStringNumTimes string num = String.repeat num string


{-| Test suite for `RepeatString`. -}
testSuite : Test
testSuite = suite "RepeatString" [testRepeatStringNumTimes]


{-| Test suite for `repeatStringNumTimes`. -}
testRepeatStringNumTimes : Test
testRepeatStringNumTimes =
  let f (str, num, expected) =
        let name = "input: " ++ toString str ++ " " ++ toString num
            actual = repeatStringNumTimes str num
        in test name <| assertEqual expected actual
      inputs = [ ("*", 3, "***")
               , ("abc", 3, "abcabcabc")
               , ("abc", 4, "abcabcabcabc")
               , ("abc", 1, "abc")
               , ("*", 8, "********")
               , ("abc", -2, "")
               ]
      tests = List.map f inputs
  in suite "repeatStringNumTimes" tests
