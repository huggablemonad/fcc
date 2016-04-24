module TruncateString
  ( main, truncateString
  ) where

{-| # Truncate a string
-----------------------

Truncate a string (first argument) if it is longer than the given maximum
string length (second argument). Return the truncated string with a `...`
ending.

Note that inserting the three dots to the end will add to the string length.

However, if the given maximum string length `num` is less than or equal to 3,
then the addition of the three dots does not add to the string length in
determining the truncated string.

This is a certification challenge.

<https://www.freecodecamp.com/challenges/truncate-a-string>

# Core functions
@docs main, truncateString

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


{-| Truncate a string (first argument) if it is longer than the given maximum
string length (second argument). Return the truncated string with a `...`
ending.

    truncateString "hello" 3 == "hel..."
    truncateString "hello world" 8 == "hello..."
    truncateString "hello beautiful world" 21 == "hello beautiful world"
    truncateString "hello" 6 == "hello"

This function is undefined for negative string lengths. It currently returns
`"..."`
-}
truncateString : String -> Int -> String
truncateString string num =
  if num >= String.length string then
    string

  else if num <= 3 then
    String.left num string ++ "..."

  else
    String.left (num - 3) string ++ "..."


{-| Test suite for `TruncateString`. -}
testSuite : Test
testSuite = suite "TruncateString" [testTruncateString]


{-| Test suite for `truncateString`. -}
testTruncateString : Test
testTruncateString =
  let f (str, num, expected) =
        let name = "input: " ++ toString str
            actual = truncateString str num
        in test name <| assertEqual expected actual
      inputs = [ ("A-tisket a-tasket A green and yellow basket", 11, "A-tisket...")
               , ("Peter Piper picked a peck of pickled peppers", 14, "Peter Piper...")
               , ("A-tisket a-tasket A green and yellow basket", 43, "A-tisket a-tasket A green and yellow basket")
               , ("A-tisket a-tasket A green and yellow basket", 45, "A-tisket a-tasket A green and yellow basket")
               , ("A-", 1, "A...")
               , ("Absolutely Longer", 2, "Ab...")
               , ("A-", 3, "A-")
               , ("A-", -1, "...")
               ]
      tests = List.map f inputs
  in suite "truncateString" tests
