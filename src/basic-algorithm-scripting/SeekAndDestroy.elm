module SeekAndDestroy
  ( main, destroyer
  ) where

{-| # Seek and Destroy
----------------------

You will be provided with an initial array (the first argument in the destroyer
function), followed by one or more arguments. Remove all elements from the
initial array that are of the same value as these arguments.

This is a certification challenge.

<https://www.freecodecamp.com/challenges/seek-and-destroy>

# Core functions
@docs main, destroyer

-}

import Graphics.Element exposing (Element)
import List
import ElmTest exposing (..)


{-| Main entry point.

This just runs the tests.
-}
main : Element
main = elementRunner testSuite


{-| Remove all elements from the initial array that are present in the second
array.

    destroyer [1, 2, 3, 5, 1, 2, 3] [2, 3] == [1, 5, 1]
    destroyer [2, 3, 2, 3] [2, 3] == []
    destroyer ["tree", "hamburger"] ["tree"] == ["hamburger"]
-}
destroyer : List a -> List a -> List a
destroyer xs ys = List.foldl (\y acc -> List.filter (\x -> x /= y) acc) xs ys


{-| Test suite for `SeekAndDestroy`. -}
testSuite : Test
testSuite = suite "SeekAndDestroy" [testDestroyer]


{-| Test suite for `destroyer`. -}
testDestroyer : Test
testDestroyer =
  let f (xs, ys, expected) =
        let name = "input: " ++ toString xs ++ " " ++ toString ys
            actual = destroyer xs ys
        in test name <| assertEqual expected actual
      strInputs = [(["tree", "hamburger"], ["tree"], ["hamburger"])]
      strTests = List.map f strInputs
      inputs = [ ([1, 2, 3, 1, 2, 3], [2, 3], [1, 1])
               , ([1, 2, 3, 5, 1, 2, 3], [2, 3], [1, 5, 1])
               , ([3, 5, 1, 2, 2], [2, 3, 5], [1])
               , ([2, 3, 2, 3], [2, 3], [])
               ]
      tests = List.map f inputs
  in suite "destroyer" <| strTests ++ tests
