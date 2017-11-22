import Integer
import SetFunctions
import Test.Prop

-- Jumping Numbers
-- http://practice.geeksforgeeks.org/problems/jumping-numbers/0
-- 
-- Given a positive number x, print all Jumping Numbers smaller than or
-- equal to x. A number is called as a Jumping Number if all adjacent
-- digits in it differ by 1. The difference between â€˜9â€™ and â€˜0â€™ is not
-- considered as 1. All single digit numbers are considered as Jumping
-- Numbers. For example 7, 8987 and 4343456 are Jumping numbers but 796
-- and 89098 are not.

-- The isJumpingD function takes an Integer
-- and returns True if the number is 'Jumping'
-- otherwise False
isJumpingD :: Int -> Bool
isJumpingD x = aux (x `div` 10) (x `mod` 10) where
  aux remainder digit | remainder == 0 = True
                      | otherwise = if (abs ((remainder `mod` 10) - digit) /= 1) then False
                                    else aux (remainder `div` 10) (remainder `mod` 10)

-- The jumpingNumbersD function takes an Integer 'x'
-- and returns the list of 'Jumping' integers
-- up to x.
jumpingNumbersD :: Int -> [Int]
jumpingNumbersD x = [y | y <- [0..x], isJumpingD y]

-- The choiceND function non-deterministically 
-- chooses a value.
choiceND :: Int
choiceND = 1 ? 2 ? 3 ? 4 ? 5 ? 6 ? 7 ? 8 ? 9

-- The nextDigitND function takes an integer
-- and returns the next possible integer in the 
-- sequence for it to be jumping.
nextDigitND :: Int -> Int
nextDigitND 0 = 1
nextDigitND 1 = 0 ? 2
nextDigitND 2 = 1 ? 3
nextDigitND 3 = 2 ? 4
nextDigitND 4 = 3 ? 5
nextDigitND 5 = 4 ? 6
nextDigitND 6 = 5 ? 7
nextDigitND 7 = 6 ? 8
nextDigitND 8 = 7 ? 9
nextDigitND 9 = 8

-- The jumpingNumbersND function takes
-- an integer and creates all the possible
-- jumping numbers up to that integer.
jumpingNumbersND :: Int -> Int
jumpingNumbersND x = 0 ? aux 0 choiceND x where
  aux start pick end | (start + pick) > end = failed
                     | otherwise = (start + pick) ? aux (10 * (start + pick)) (nextDigitND pick) end
mainD :: [Int]
mainD = jumpingNumbersD 50

mainND :: [Int]
mainND = sortValues (set1 jumpingNumbersND 50)

propDContains8987 :: Test.Prop.Prop
propDContains8987 = 8987 `elem` (jumpingNumbersD 9000) -=- True

propNDContains8987 :: Test.Prop.Prop
propNDContains8987 = jumpingNumbersND 9000 ~> 8987

-- Test Case to see if 4343456 is a jumping number
-- Similar test case with the deterministic function
-- takes forever
propNDContains4343456 :: Test.Prop.Prop
propNDContains4343456 = jumpingNumbersND 4400000 ~> 4343456

propDEqualsND :: Test.Prop.Prop
propDEqualsND = jumpingNumbersD 1000 -=- (sortValues (set1 jumpingNumbersND 1000))
