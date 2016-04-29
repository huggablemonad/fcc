module DiffArrays
  ( main, diffArray
  ) where

{-| # Diff Two Arrays
---------------------

Compare two arrays and return a new array with any items only found in one of
the two given arrays, but not both. In other words, return the symmetric
difference of the two arrays.

This is a certification challenge.

<https://www.freecodecamp.com/challenges/diff-two-arrays>

# Core functions
@docs main, diffArray

-}

import Graphics.Element exposing (Element)
import List
import ElmTest exposing (..)


{-| Main entry point.

This just runs the tests.
-}
main : Element
main = elementRunner testSuite


{-| Return the symmetric difference of the two arrays.

    diffArray ["andesite", "dead shrub"] ["andesite", "dead shrub"] == []
    diffArray [], ["calf", "piglet"] == ["calf", "piglet"]
    diffArray [1, 2, 3, 5], [1, 2, 3, 4, 5] == [4]

This function assumes that the input arrays contain distinct elements.
-}
diffArray : List a -> List a -> List a
diffArray xs ys =
  if List.isEmpty xs then
    ys

  else if List.isEmpty ys then
    xs

  else
    symmetricDiff (xs ++ ys) []


{-| Helper function for diffArray. This is where most of the work takes place.
-}
symmetricDiff : List a -> List a -> List a
symmetricDiff xs acc =
  case xs of
    [] ->
      List.reverse acc

    y :: _ ->
      let (r, s) = List.partition (\z -> z == y) xs
          acc' = if List.length r == 1 then y :: acc else acc
      in symmetricDiff s acc'


{-| Test suite for `DiffArrays`. -}
testSuite : Test
testSuite = suite "DiffArrays" [testDiffArray]


{-| Test suite for `diffArray`. -}
testDiffArray : Test
testDiffArray =
  let f (xs, ys, expected) =
        let name = "input: " ++ toString xs ++ " " ++ toString ys
            actual = diffArray xs ys
        in test name <| assertEqual expected actual
      strInputs = [ ( ["diorite", "andesite", "grass", "dirt", "pink wool", "dead shrub"]
                    , ["diorite", "andesite", "grass", "dirt", "dead shrub"]
                    , ["pink wool"]
                    )
                  , ( ["andesite", "grass", "dirt", "pink wool", "dead shrub"]
                    , ["diorite", "andesite", "grass", "dirt", "dead shrub"]
                    , ["pink wool", "diorite"]
                    )
                  , ( ["andesite", "grass", "dirt", "dead shrub"]
                    , ["andesite", "grass", "dirt", "dead shrub"]
                    , []
                    )
                  , ( ["calf", "piglet"]
                    , ["calf"]
                    , ["piglet"]
                    )
                  , ( []
                    , ["snuffleupagus", "cookie monster", "elmo"]
                    , ["snuffleupagus", "cookie monster", "elmo"]
                    )
                  , ( ["calf", "piglet"]
                    , ["filly"]
                    , ["calf", "piglet", "filly"]
                    )
                  ]
      strTests = List.map f strInputs
      inputs = [ ([1, 2, 3, 5], [1, 2, 3, 4, 5], [4])
               , ([1, 3], [7], [1, 3, 7])
               ]
      tests = List.map f inputs
  in suite "diffArray" <| strTests ++ tests
