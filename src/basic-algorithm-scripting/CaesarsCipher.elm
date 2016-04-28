module CaesarsCipher
  ( main, rot13
  ) where

{-| # Caesars Cipher
--------------------

One of the simplest and most widely known `ciphers` is a `Caesar cipher`, also
known as a `shift cipher`. In a `shift cipher` the meanings of the letters are
shifted by some set amount.

A common modern use is the [ROT13](https://en.wikipedia.org/wiki/ROT13) cipher,
where the values of the letters are shifted by 13 places. Thus 'A' <-> 'N', 'B'
<-> 'O' and so on.

Write a function which takes a [ROT13](https://en.wikipedia.org/wiki/ROT13)
encoded string as input and returns a decoded string.

All letters will be uppercase. Do not transform any non-alphabetic character
(i.e. spaces, punctuation), but do pass them on.

This is a certification challenge.

<https://www.freecodecamp.com/challenges/caesars-cipher>

# Core functions
@docs main, rot13

-}

import Graphics.Element exposing (Element)
import Char
import List
import String
import ElmTest exposing (..)


{-| Main entry point.

This just runs the tests.
-}
main : Element
main = elementRunner testSuite


{-| Return the decoded ROT13 string.

    rot13 "SERR PBQR PNZC" == "FREE CODE CAMP"
    rot13 "SERR CVMMN!" == "FREE PIZZA!"
    rot13 "SERR YBIR?" == "FREE LOVE?"
-}
rot13 : String -> String
rot13 string =
  let rot c =
        Char.fromCode
          <| if c < 'N' then
               -- 90 ('Z') - 13 + 1 == 78.
               78 + (Char.toCode c - 65)

             else
               Char.toCode c - 13
  in String.map (\c -> if Char.isUpper c then rot c else c) string


{-| Test suite for `CaesarsCipher`. -}
testSuite : Test
testSuite = suite "CaesarsCipher" [testRot13]


{-| Test suite for `rot13`. -}
testRot13 : Test
testRot13 =
  let f (str, expected) =
        let name = "input: " ++ toString str
            actual = rot13 str
        in test name <| assertEqual expected actual
      inputs = [ ("SERR PBQR PNZC", "FREE CODE CAMP")
               , ("SERR CVMMN!", "FREE PIZZA!")
               , ("SERR YBIR?", "FREE LOVE?")
               , ("GUR DHVPX OEBJA QBT WHZCRQ BIRE GUR YNML SBK.", "THE QUICK BROWN DOG JUMPED OVER THE LAZY FOX.")
               ]
      tests = List.map f inputs
  in suite "rot13" tests
