module Belong
  ( main, getIndexToIns
  ) where

{-| # Where do I belong
-----------------------

Return the lowest index at which a value (second argument) should be inserted
into an array (first argument) once it has been sorted.

For example, `getIndexToIns [1,2,3,4] 1.5` should return `1` because it is
greater than `1` (index 0), but less than `2` (index 1).

Likewise, `getIndexToIns [20,3,5] 19` should return `2` because once the array
has been sorted it will look like `[3,5,20]` and `19` is less than `20` (index
2) and greater than `5` (index 1).

This is a certification challenge.

<https://www.freecodecamp.com/challenges/where-do-i-belong>

# Core functions
@docs main, getIndexToIns

-}

import Graphics.Element exposing (Element)
import List
import List.Extra
import Maybe
import ElmTest exposing (..)


{-| Main entry point.

This just runs the tests.
-}
main : Element
main = elementRunner testSuite


{-| Return the lowest index at which a value (second argument) should be
inserted into an array (first argument) once it has been sorted.

    getIndexToIns ["jkl", "abc", "xyz"] "def" == 1
    getIndexToIns [1.0, 2.0, 3.0, 4.0] 1.5 == 1
    getIndexToIns [3, 10, 5] 3 == 0
    getIndexToIns [2, 5, 10] 15 == 3
-}
getIndexToIns : List comparable -> comparable -> Int
getIndexToIns xs x =
  List.sort xs
    |> List.Extra.findIndex (\y -> y >= x)
    |> Maybe.withDefault (List.length xs)


{-| Test suite for `Belong`. -}
testSuite : Test
testSuite = suite "Belong" [testGetIndexToIns]


{-| Test suite for `getIndexToIns`. -}
testGetIndexToIns : Test
testGetIndexToIns =
  let f (xs, x, expected) =
        let name = "input: " ++ toString xs ++ " " ++ toString x
            actual = getIndexToIns xs x
        in test name <| assertEqual expected actual
      strInputs = [(["jkl", "abc", "xyz"], "def", 1)]
      strTests = List.map f strInputs
      floatInputs = [([1.0, 2.0, 3.0, 4.0], 1.5, 1)]
      floatTests = List.map f floatInputs
      inputs = [ ([10, 20, 30, 40, 50], 35, 3)
               , ([10, 20, 30, 40, 50], 30, 2)
               , ([40, 60], 50, 1)
               , ([3, 10, 5], 3, 0)
               , ([5, 3, 20, 3], 5, 2)
               , ([2, 20, 10], 19, 2)
               , ([2, 5, 10], 15, 3)
               ]
      tests = List.map f inputs
  in suite "getIndexToIns" <| strTests ++ floatTests ++ tests
