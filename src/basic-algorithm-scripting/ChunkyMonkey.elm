module ChunkyMonkey
  ( main, chunkArrayInGroups
  ) where

{-| # Chunky Monkey
-------------------

Write a function that splits an array (first argument) into groups the length
of `size` (second argument) and returns them as a two-dimensional array.

This is a certification challenge.

<https://www.freecodecamp.com/challenges/chunky-monkey>

# Core functions
@docs main, chunkArrayInGroups

-}

import Graphics.Element exposing (Element)
import List
import List.Split
import ElmTest exposing (..)


{-| Main entry point.

This just runs the tests.
-}
main : Element
main = elementRunner testSuite


{-| Split an array (first argument) into groups the length of `size` (second
argument) and return them as a two-dimensional array.

    chunkArrayInGroups ["a", "b", "c", "d"] 2 == [["a", "b"], ["c", "d"]]
    chunkArrayInGroups [0, 1, 2, 3, 4, 5, 6] 3 == [[0, 1, 2], [3, 4, 5], [6]]
-}
chunkArrayInGroups : List a -> Int -> List (List a)
chunkArrayInGroups xs n = List.Split.chunksOfLeft n xs


{-| Test suite for `ChunkyMonkey`. -}
testSuite : Test
testSuite = suite "ChunkyMonkey" [testChunkArrayInGroups]


{-| Test suite for `chunkArrayInGroups`. -}
testChunkArrayInGroups : Test
testChunkArrayInGroups =
  let f (xs, n, expected) =
        let name = "input: " ++ toString xs ++ " " ++ toString n
            actual = chunkArrayInGroups xs n
        in test name <| assertEqual expected actual
      strInputs = [(["a", "b", "c", "d"], 2, [["a", "b"], ["c", "d"]])]
      strTests = List.map f strInputs
      inputs = [ ([0, 1, 2, 3, 4, 5], 3, [[0, 1, 2], [3, 4, 5]])
               , ([0, 1, 2, 3, 4, 5], 2, [[0, 1], [2, 3], [4, 5]])
               , ([0, 1, 2, 3, 4, 5], 4, [[0, 1, 2, 3], [4, 5]])
               , ([0, 1, 2, 3, 4, 5, 6], 3, [[0, 1, 2], [3, 4, 5], [6]])
               , ([0, 1, 2, 3, 4, 5, 6, 7, 8], 4, [[0, 1, 2, 3], [4, 5, 6, 7], [8]])
               ]
      tests = List.map f inputs
  in suite "chunkArrayInGroups" <| strTests ++ tests
