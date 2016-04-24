module ConfirmEnding
  ( main, confirmEnding
  ) where

{-| # Confirm the Ending
------------------------

Check if a string (first argument, `str`) ends with the given target string
(second argument, `target`).

This is a certification challenge.

<https://www.freecodecamp.com/challenges/confirm-the-ending>

# Core functions
@docs main, confirmEnding

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


{-| Return true if a string (first argument, `str`) ends with the given target
string (second argument, `target`).

    confirmEnding "Bastian" "n" == True
    confirmEnding "He has to give me a new name" "na" == False
-}
confirmEnding : String -> String -> Bool
confirmEnding string target = String.endsWith target string


{-| Test suite for `ConfirmEnding`. -}
testSuite : Test
testSuite = suite "ConfirmEnding" [testConfirmEnding]


{-| Test suite for `confirmEnding`. -}
testConfirmEnding : Test
testConfirmEnding =
  let f (str, target, expected) =
        let name = "input: " ++ toString str ++ " " ++ toString target
            actual = confirmEnding str target
        in test name <| assertEqual expected actual
      inputs = [ ("Bastian", "n", True)
               , ("Connor", "n", False)
               , ("Walking on water and developing software from a specification are easy if both are frozen", "specification", False)
               , ("He has to give me a new name", "name", True)
               , ("He has to give me a new name", "me", True)
               , ("He has to give me a new name", "na", False)
               , ("If you want to save our world, you must hurry. We dont know how much longer we can withstand the nothing", "mountain", False)
               ]
      tests = List.map f inputs
  in suite "confirmEnding" tests
