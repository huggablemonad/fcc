module RomanNumerals
  ( main, convertToRoman
  ) where

{-| # Roman Numeral Converter
-----------------------------

Convert the given number into a roman numeral.

All [roman numerals](https://www.mathsisfun.com/roman-numerals.html) answers
should be provided in upper-case.

This is a certification challenge.

<https://www.freecodecamp.com/challenges/roman-numeral-converter>

# Core functions
@docs main, convertToRoman

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


{-| Convert the given number into a roman numeral.

    convertToRoman 4 == "IV"
    convertToRoman 891 == "DCCCXCI"
    convertToRoman 3999 == "MMMCMXCIX"

Return an empty string if the input does not lie in the interval [1, 3999].
-}
convertToRoman : Int -> String
convertToRoman num =
  if num >= 1 && num <= 3999 then
    let thousands = String.repeat (num // 1000) <| romanSymbol 1000
        hundreds = toRoman 100 <| num `rem` 1000 // 100
        tens = toRoman 10 <| num `rem` 100 // 10
        ones = toRoman 1 <| num `rem` 10
    in String.concat [thousands, hundreds, tens, ones]

  else
    ""


{-| Helper function for `convertToRoman`. This is where most of the work is
done.

    toRoman 1 7 == "VII"
    toRoman 10 3 == "XXX"
    toRoman 100 8 == "DCCC"
-}
toRoman : Int -> Int -> String
toRoman multiplier num =
  let symbol = romanSymbol <| num * multiplier
      midpoint = multiplier * 10 // 2
      multiplierSymbol = romanSymbol multiplier
  in if String.isEmpty symbol then
       if num * multiplier < midpoint then
         String.repeat num multiplierSymbol

       else
         romanSymbol midpoint ++ String.repeat (num - 5) multiplierSymbol

     else symbol


{-| Return the basic roman symbols for the given number. -}
romanSymbol : Int -> String
romanSymbol num =
  case num of
    1 -> "I"
    4 -> "IV"
    5 -> "V"
    9 -> "IX"
    10 -> "X"
    40 -> "XL"
    50 -> "L"
    90 -> "XC"
    100 -> "C"
    400 -> "CD"
    500 -> "D"
    900 -> "CM"
    1000 -> "M"
    _ -> ""


{-| Test suite for `RomanNumerals`. -}
testSuite : Test
testSuite = suite "RomanNumerals" [testConvertToRoman]


{-| Test suite for `convertToRoman`. -}
testConvertToRoman : Test
testConvertToRoman =
  let f (num, expected) =
        let name = "input: " ++ toString num
            actual = convertToRoman num
        in test name <| assertEqual expected actual
      inputs = [ (2, "II")
               , (3, "III")
               , (4, "IV")
               , (5, "V")
               , (9, "IX")
               , (12, "XII")
               , (16, "XVI")
               , (29, "XXIX")
               , (44, "XLIV")
               , (45, "XLV")
               , (68, "LXVIII")
               , (83, "LXXXIII")
               , (99, "XCIX")
               , (500, "D")
               , (501, "DI")
               , (649, "DCXLIX")
               , (798, "DCCXCVIII")
               , (891, "DCCCXCI")
               , (1000, "M")
               , (1004, "MIV")
               , (1006, "MVI")
               , (1023, "MXXIII")
               , (2014, "MMXIV")
               , (3999, "MMMCMXCIX")
               ]
      tests = List.map f inputs
  in suite "convertToRoman" tests
