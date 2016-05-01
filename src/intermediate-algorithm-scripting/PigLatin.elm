module PigLatin
  ( main, translatePigLatin
  ) where

{-| # Pig Latin
---------------

Translate the provided string to pig latin.

[Pig Latin](https://en.wikipedia.org/wiki/Pig_Latin) takes the first consonant
(or consonant cluster) of an English word, moves it to the end of the word and
suffixes an "ay".

If a word begins with a vowel you just add "way" to the end.

This is a certification challenge.

<https://www.freecodecamp.com/challenges/pig-latin>

# Core functions
@docs main, translatePigLatin

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


{-| Translate the provided string to pig latin.

    translatePigLatin "california" == "aliforniacay"
    translatePigLatin "glove" == "oveglay"
    translatePigLatin "algorithm" == "algorithmway"
-}
translatePigLatin : String -> String
translatePigLatin string =
  let isVowel c = List.member c ['a', 'e', 'i', 'o', 'u']
  in case span isVowel string of
       ("", _) ->
         let (consonants, rest) = span (not << isVowel) string
         in rest ++ consonants ++ "ay"

       _ ->
         string ++ "way"


{-| Return a pair of strings whose first element is the longest prefix that
satisfies the predicate, and the second is the remainder of the string.

This is basically a clone of Haskell's `Data.List.break` / `Data.Text.break`.
-}
span : (Char -> Bool) -> String -> (String, String)
span predicate string =
  let f acc xs  =
        case String.uncons xs of
          Just (c, cs) ->
            if predicate c then
              f (String.cons c acc) cs

            else
              (String.reverse acc, xs)

          Nothing ->
            (String.reverse acc, xs)
  in f "" string


{-| Test suite for `PigLatin`. -}
testSuite : Test
testSuite = suite "PigLatin" [testTranslatePigLatin]


{-| Test suite for `translatePigLatin`. -}
testTranslatePigLatin : Test
testTranslatePigLatin =
  let f (str, expected) =
        let name = "input: " ++ toString str
            actual = translatePigLatin str
        in test name <| assertEqual expected actual
      inputs = [ ("california", "aliforniacay")
               , ("paragraphs", "aragraphspay")
               , ("glove", "oveglay")
               , ("algorithm", "algorithmway")
               , ("eight", "eightway")
               ]
      tests = List.map f inputs
  in suite "translatePigLatin" tests
