module Factorialize
  ( main, factorialize
  ) where

{-| # Factorialize a Number
---------------------------

Return the factorial of the provided integer.

If the integer is represented with the letter n, a factorial is the product of
all positive integers less than or equal to n.

Factorials are often represented with the shorthand notation `n!`

For example: `5! = 1 * 2 * 3 * 4 * 5 = 120`

This is a certification challenge.

<https://www.freecodecamp.com/challenges/factorialize-a-number>

# Core functions
@docs main, factorialize

-}

import Graphics.Element exposing (Element)
import List
import ElmTest exposing (..)


{-| Main entry point.

This just runs the tests.
-}
main : Element
main = elementRunner testSuite


{-| Return the factorial of the provided integer.

    factorialize 5 = 120
    factorialize 10 = 3628800
    factorialize 20 = 2432902008176640000
    factorialize 0 = 1

This function will return `1` for inputs less than `0`.
-}
factorialize : Int -> Float
factorialize n =
  if n < 2 then
    1

  else
    List.product [2..n]


{-| Test suite for `Factorialize`. -}
testSuite : Test
testSuite = suite "Factorialize" [testFactorialize]


{-| Test suite for `factorialize`. -}
testFactorialize : Test
testFactorialize =
  let f (n, expected) =
        let name = "input: " ++ toString n
            actual = factorialize n
        in test name <| assertEqual actual expected
      inputs = [ (5, 120.0)
               , (10, 3628800.0)
               , (20, 2432902008176640000.0)
               , (0, 1.0)
               ]
      tests = List.map f inputs
  in suite "factorialize" tests
