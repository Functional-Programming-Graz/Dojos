{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck
import qualified Data.Text.Internal.Functions as T

import Lib

--------------------------------------------------------------------------
-- main

split' :: [a] -> (a, [a])
split' [] = error "fuck"
split' (x:xs) = (x, xs)

genTest :: [Int] -> String
genTest [] = ""
genTest (x:[]) = show x
genTest (x:xs) = show x ++ "," ++ genTest xs

genTest' :: String -> [Int] -> String
genTest' delimiter = concat . T.intersperse delimiter . map show

commaSepTest = genTest' ","
newlineSepTest = genTest' "\n"

prop_addition :: [Int] -> Bool
prop_addition xs = (sum xs) == add (genTest xs) 

prop_addition' :: [Int] -> Bool
prop_addition' xs = (sum xs) == add' (genTest xs) 

return []
main = $quickCheckAll



















