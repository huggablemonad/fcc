module Palindromes
  ( main, palindrome
  ) where

{-| # Check for Palindromes
---------------------------

Return `true` if the given string is a palindrome. Otherwise, return `false`.

A `palindrome` is a word or sentence that's spelled the same way both forward
and backward, ignoring punctuation, case, and spacing.

**Note**

You'll need to remove **all non-alphanumeric characters** (punctuation, spaces
and symbols) and turn everything lower case in order to check for palindromes.

We'll pass strings with varying formats, such as `"racecar"`, `"RaceCar"`, and
`"race CAR"` among others.

This is a certification challenge.

<https://www.freecodecamp.com/challenges/check-for-palindromes>

# Core functions
@docs main, palindrome

-}

import Char
import Graphics.Element exposing (Element)
import List
import String
import ElmTest exposing (..)


{-| Main entry point.

This just runs the tests.
-}
main : Element
main = elementRunner testSuite


{-| Return true if the given string is a palindrome.

    palindrome "eye" == True
    palindrome "A man, a plan, a canal. Panama" == True
    palindrome "1 eye for of 1 eye." == False
    palindrome "0_0 (: /-\ :) 0-0" == True
-}
palindrome : String -> Bool
palindrome string =
  let pred c = Char.isLower c || Char.isDigit c
      alphaNumStr = String.toLower string |> String.filter pred
  in alphaNumStr == String.reverse alphaNumStr


{-| Test suite for `Palindromes`. -}
testSuite : Test
testSuite = suite "Palindromes" [testPalindrome]


{-| Test suite for `palindrome`. -}
testPalindrome : Test
testPalindrome =
  let f (str, expected) =
        let name = "input: " ++ toString str
            actual = palindrome str
        in test name <| assertEqual expected actual
      inputs = [ ("eye", True)
               , ("race car", True)
               , ("not a palindrome", False)
               , ("A man, a plan, a canal. Panama", True)
               , ("never odd or even", True)
               , ("nope", False)
               , ("almostomla", False)
               , ("My age is 0, 0 si ega ym.", True)
               , ("1 eye for of 1 eye.", False)
               , ("0_0 (: /-\\ :) 0-0", True)
               ]
      tests = List.map f inputs
  in suite "palindrome" tests
