module TitleCase
  ( main, titleCase
  ) where

{-| # Title Case a Sentence
---------------------------

Return the provided string with the first letter of each word capitalized. Make
sure the rest of the word is in lower case.

For the purpose of this exercise, you should also capitalize connecting words
like "the" and "of".

This is a certification challenge.

<https://www.freecodecamp.com/challenges/title-case-a-sentence>

# Core functions
@docs main, titleCase

-}

import Graphics.Element exposing (Element)
import List
import String
import String.Extra
import ElmTest exposing (..)


{-| Main entry point.

This just runs the tests.
-}
main : Element
main = elementRunner testSuite


{-| Return the provided string with the first letter of each word capitalized,
and the rest of the word in lower case.

    titleCase "I'm a little tea pot" == "I'm A Little Tea Pot"
    titleCase "sHoRt AnD sToUt" == "Short And Stout"
    titleCase "HERE IS MY HANDLE HERE IS MY SPOUT" == "Here Is My Handle Here Is My Spout"
-}
titleCase : String -> String
titleCase string =
  String.words string
    |> List.map (String.Extra.capitalize True << String.toLower)
    |> String.join " "


{-| Test suite for `TitleCase`. -}
testSuite : Test
testSuite = suite "TitleCase" [testTitleCase]


{-| Test suite for `titleCase`. -}
testTitleCase : Test
testTitleCase =
  let f (str, expected) =
        let name = "input: " ++ toString str
            actual = titleCase str
        in test name <| assertEqual expected actual
      inputs = [ ("I'm a little tea pot", "I'm A Little Tea Pot")
               , ("sHoRt AnD sToUt", "Short And Stout")
               , ("HERE IS MY HANDLE HERE IS MY SPOUT", "Here Is My Handle Here Is My Spout")
               ]
      tests = List.map f inputs
  in suite "titleCase" tests
