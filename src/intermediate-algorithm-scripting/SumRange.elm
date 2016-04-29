module SumRange
  ( main, sumAll
  ) where

{-| # Sum All Numbers in a Range
--------------------------------

We'll pass you an array of two numbers. Return the sum of those two numbers and
all numbers between them.

The lowest number will not always come first.

This is a certification challenge.

<https://www.freecodecamp.com/challenges/sum-all-numbers-in-a-range>

# Core functions
@docs main, sumAll

-}

import Graphics.Element exposing (Element)
import List
import List.Extra exposing ((!!))
import Maybe
import ElmTest exposing (..)


{-| Main entry point.

This just runs the tests.
-}
main : Element
main = elementRunner testSuite


{-| Return the sum of the two numbers in the array and all numbers between
them.

    sumAll [1, 4] == 10
    sumAll [4, 1] == 10
    sumAll [5, 10] == 45
    sumAll [10, 5] == 45

Return `0` if the length of the array is not `2`.
-}
sumAll : List Int -> Int
sumAll xs =
  if List.length xs /= 2 then
    0

  else
    let x = Maybe.withDefault 0 <| xs !! 0
        y = Maybe.withDefault 0 <| xs !! 1
        range = if x >= y then [y + 1..x - 1] else [x + 1..y - 1]
    in x + y + List.sum range


{-| Test suite for `SumRange`. -}
testSuite : Test
testSuite = suite "SumRange" [testSumAll]


{-| Test suite for `sumAll`. -}
testSumAll : Test
testSumAll =
  let f (xs, expected) =
        let name = "input: " ++ toString xs
            actual = sumAll xs
        in test name <| assertEqual expected actual
      inputs = [ ([1, 4], 10)
               , ([4, 1], 10)
               , ([5, 10], 45)
               , ([10, 5], 45)
               ]
      tests = List.map f inputs
  in suite "sumAll" tests
