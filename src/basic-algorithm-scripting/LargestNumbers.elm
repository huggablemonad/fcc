module LargestNumbers
  ( main, largestOfFour
  ) where

{-| # Return Largest Numbers in Arrays
--------------------------------------

Return an array consisting of the largest number from each provided sub-array.
For simplicity, the provided array will contain exactly 4 sub-arrays.

This is a certification challenge.

<https://www.freecodecamp.com/challenges/return-largest-numbers-in-arrays>

# Core functions
@docs main, largestOfFour

-}

import Graphics.Element exposing (Element)
import List
import Maybe
import ElmTest exposing (..)


{-| Main entry point.

This just runs the tests.
-}
main : Element
main = elementRunner testSuite


{-| Return an array consisting of the largest number from each provided
sub-array.

    largestOfFour [[13, 27, 18, 26], [4, 5, 1, 3], [32, 35, 37, 39], [1000, 1001, 857, 1]] == [27,5,39,1001]
    largestOfFour [[4, 9, 1, 3], [13, 35, 18, 26], [32, 35, 97, 39], [1000000, 1001, 857, 1]] == [9, 35, 97, 1000000]

This function returns `0` if given invalid input:

    largestOfFour [[], [], [], []] == [0, 0, 0, 0]
    largestOfFour [[1, 2, 3, 4], [], [5, 6, 7, 8], []] = [4, 0, 8, 0]
-}
largestOfFour : List (List Int) -> List Int
largestOfFour list = List.map (Maybe.withDefault 0 << List.maximum) list


{-| Test suite for `LargestNumbers`. -}
testSuite : Test
testSuite = suite "LargestNumbers" [testLargestOfFour]


{-| Test suite for `largestOfFour`. -}
testLargestOfFour : Test
testLargestOfFour =
  let f (str, expected) =
        let name = "input: " ++ toString str
            actual = largestOfFour str
        in test name <| assertEqual expected actual
      inputs = [ ( [ [4, 5, 1, 3]
                   , [13, 27, 18, 26]
                   , [32, 35, 37, 39]
                   , [1000, 1001, 857, 1]
                   ]
                 , [5, 27, 39, 1001]
                 )
               , ( [ [13, 27, 18, 26]
                   , [4, 5, 1, 3]
                   , [32, 35, 37, 39]
                   , [1000, 1001, 857, 1]
                   ]
                 , [27,5,39,1001]
                 )
               , ( [ [4, 9, 1, 3]
                   , [13, 35, 18, 26]
                   , [32, 35, 97, 39]
                   , [1000000, 1001, 857, 1]
                   ]
                 , [9, 35, 97, 1000000]
                 )
               ]
      tests = List.map f inputs
  in suite "largestOfFour" tests
