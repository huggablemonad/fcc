module SlasherFlick
  ( main, slasher
  ) where

{-| # Slasher Flick
-------------------

Return the remaining elements of an array after chopping off `n` elements from
the head.

The head means the beginning of the array, or the zeroth index.

This is a certification challenge.

<https://www.freecodecamp.com/challenges/slasher-flick>

# Core functions
@docs main, slasher

-}

import Graphics.Element exposing (Element)
import List
import ElmTest exposing (..)


{-| Main entry point.

This just runs the tests.
-}
main : Element
main = elementRunner testSuite


{-| Return the remaining elements of an array after chopping off `n` elements
from the head.

    slasher [1, 2, 3] 2 == 3
    slasher [1, 2, 3] 0 == [1, 2, 3]
    slasher [1, 2, 3] 9 == []
    slasher ["burgers", "fries", "shake"] 1 == ["fries", "shake"]
-}
slasher : List a -> Int -> List a
slasher xs n = List.drop n xs


{-| Test suite for `SlasherFlick`. -}
testSuite : Test
testSuite = suite "SlasherFlick" [testSlasher]


{-| Test suite for `slasher`. -}
testSlasher : Test
testSlasher =
  let f (str, n, expected) =
        let name = "input: " ++ toString str
            actual = slasher str n
        in test name <| assertEqual expected actual
      strInputs = [(["burgers", "fries", "shake"], 1, ["fries", "shake"])]
      strTests = List.map f strInputs
      inputs = [ ([1, 2, 3], 2, [3])
               , ([1, 2, 3], 0, [1, 2, 3])
               , ([1, 2, 3], 9, [])
               , ([1, 2, 3], 4, [])
               ]
      tests = List.map f inputs
  in suite "slasher" <| strTests ++ tests
