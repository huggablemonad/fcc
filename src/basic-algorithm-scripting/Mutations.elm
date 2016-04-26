module Mutations
  ( main, mutation
  ) where

{-| # Mutations
---------------

Return true if the string in the first element of the array contains all of the
letters of the string in the second element of the array.

For example, `["hello", "Hello"]`, should return true because all of the
letters in the second string are present in the first, ignoring case.

The arguments `["hello", "hey"]` should return false because the string "hello"
does not contain a "y".

Lastly, `["Alien", "line"]`, should return true because all of the letters in
"line" are present in "Alien".

This is a certification challenge.

<https://www.freecodecamp.com/challenges/mutations>

# Core functions
@docs main, mutation

-}

import Graphics.Element exposing (Element, show)
import List
import Set
import String
import ElmTest exposing (..)


{-| Main entry point.

This just runs the tests.
-}
main : Element
main = elementRunner testSuite


{-| Return true if the string in the first element of the array contains all of
the letters of the string in the second element of the array.

    mutation ["hello", "Hello"] = True
    mutation ["hello", "hey"] = False
    mutation ["Alien", "line"] = True

Return false if the length of the input list is not 2.
-}
mutation : List String -> Bool
mutation input =
  case input of
    parent :: child :: [] ->
      parent `contains` child

    _ -> False


{-| Return true if the first string contains all of the letters of the second
string. This function is the core of `mutation`.
-}
contains : String -> String -> Bool
contains parent child =
  let parentSet = toSet parent
      childSet = toSet child
      intersection = parentSet `Set.intersect` childSet
  in Set.size intersection == Set.size childSet


{-| Convert the string into a set. The string is lowercased first before it's
turned into a set.
-}
toSet : String -> Set.Set Char
toSet str =
  String.toLower str
    |> String.toList
    |> Set.fromList


{-| Test suite for `Mutations`. -}
testSuite : Test
testSuite = suite "Mutations" [testMutation]


{-| Test suite for `Mutation`. -}
testMutation : Test
testMutation =
  let f (xs, expected) =
        let name = "input: " ++ toString xs
            actual = mutation xs
        in test name <| assertEqual expected actual
      inputs = [ (["hello", "hey"], False)
               , (["hello", "Hello"], True)
               , (["zyxwvutsrqponmlkjihgfedcba", "qrstu"], True)
               , (["Mary", "Army"], True)
               , (["Mary", "Aarmy"], True)
               , (["Alien", "line"], True)
               , (["floor", "for"], True)
               , (["hello", "neo"], False)
               ]
      tests = List.map f inputs
  in suite "mutation" tests
