module SearchAndReplace
  ( main, myReplace
  ) where

{-| # Search and Replace
------------------------

Perform a search and replace on the sentence using the arguments provided and
return the new sentence.

First argument is the sentence to perform the search and replace on.

Second argument is the word that you will be replacing (before).

Third argument is what you will be replacing the second argument with (after).

NOTE: Preserve the case of the original word when you are replacing it. For
example if you mean to replace the word "Book" with the word "dog", it should
be replaced as "Dog"

This is a certification challenge.

<https://www.freecodecamp.com/challenges/search-and-replace>

# Core functions
@docs main, myReplace

-}

import Char
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


{-| Perform a search and replace on the sentence using the arguments provided
and return the new sentence.

    myReplace "Let us go to the store" "store" "mall" == "Let us go to the mall"
    myReplace "His name is Tom" "Tom" "john" == "His name is John"
-}
myReplace : String -> String -> String -> String
myReplace string before after =
  let replacement =
        case String.uncons before of
          Just (x, _) ->
            if Char.isUpper x then
              String.Extra.capitalize True after

            else
              after

          Nothing ->
            ""
  in String.words string
       |> List.map (\w -> if w == before then replacement else w)
       |> String.join " "


{-| Test suite for `SearchAndReplace`. -}
testSuite : Test
testSuite = suite "SearchAndReplace" [testMyReplace]


{-| Test suite for `myReplace`. -}
testMyReplace : Test
testMyReplace =
  let f (str, before, after, expected) =
        let name = "input: " ++ toString str ++ " " ++ toString before ++ " " ++ toString after
            actual = myReplace str before after
        in test name <| assertEqual expected actual
      inputs = [ ("Let us go to the store", "store", "mall", "Let us go to the mall")
               , ("He is Sleeping on the couch", "Sleeping", "sitting", "He is Sitting on the couch")
               , ("This has a spellngi error", "spellngi", "spelling", "This has a spelling error")
               , ("His name is Tom", "Tom", "john", "His name is John")
               , ("Let us get back to more Coding", "Coding", "algorithms", "Let us get back to more Algorithms")
               ]
      tests = List.map f inputs
  in suite "myReplace" tests
