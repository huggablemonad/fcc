module LongestWord
  ( main, findLongestWord
  ) where

{-| # Find the Longest Word in a String
---------------------------------------

Return the length of the longest word in the provided sentence.

Your response should be a number.

This is a certification challenge.

<https://www.freecodecamp.com/challenges/find-the-longest-word-in-a-string>

# Core functions
@docs main, findLongestWord

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


{-| Return the length of the longest word in the provided sentence.

    findLongestWord "May the force be with you" == 5
    findLongestWord "Google do a barrel roll" == 6
    findLongestWord "" == 0
-}
findLongestWord : String -> Int
findLongestWord string =
  let longestWord =
        String.words string
          |> List.map String.length
          |> List.maximum
  in case longestWord of
       Just len ->
         len

       Nothing ->
         0


{-| Test suite for `LongestWord`. -}
testSuite : Test
testSuite = suite "LongestWord" [testfindLongestWord]


{-| Test suite for `findLongestWord`. -}
testfindLongestWord : Test
testfindLongestWord =
  let f (str, expected) =
        let name = "input: " ++ toString str
            actual = findLongestWord str
        in test name <| assertEqual expected actual
      inputs = [ ("The quick brown fox jumped over the lazy dog", 6)
               , ("May the force be with you", 5)
               , ("Google do a barrel roll", 6)
               , ("What is the average airspeed velocity of an unladen swallow", 8)
               , ("What if we try a super-long word such as otorhinolaryngology", 19)
               ]
      tests = List.map f inputs
  in suite "findLongestWord" tests
