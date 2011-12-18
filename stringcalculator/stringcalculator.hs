module stringcalculator ( 
 add, add2, add3, add4
) where

import Data.String.Utils

-- Problem 1: Be abel to add integers given in a string, separated by commas.
-- Just the following cases:
-- "a"     -> n
-- "a,b"   -> n+m
-- "a,b,c" -> a+b+c 

add:: [Char] -> Int
add (x1:[]) = toInt [x1]
add (x1:',':x2:[]) = (toInt [x1]) + (toInt [x2])
add (x1:',':x2:',':x3:[]) = (toInt [x1]) + (toInt [x2]) + (toInt [x3])

toInt:: [Char] -> Int
toInt x = (read x) :: Int

-- Problem 2: Allow an undefined number of integers.
-- Examples: 
-- "1,2,3,4,5"       -> 15
-- "10,20,30,40,50"  -> 150
-- "3,3,3,3,3,3,3,3" -> 8

add2::[Char] -> Int
add2 xs =
  foldl (+) 0 . map toInt . words . replace "," " " $ xs

-- Problem 3: Allow an optional parametrizable 1 character delimiter.
-- When given a delimiter, it should precede the string of integers with
-- the following format:
--       "//[delimiter]\n[string_of_integers_with_delimiter]"
-- where [delimiter] can be any character.
-- when given no delimiter, "," is assumed.

add3::[Char] -> Int
add3 ('/':'/':delim:'\n':xs) = 
	foldl (+) 0 . map toInt . wordz [delim] $ xs
add3 xs = 
	foldl (+) 0 . map toInt . wordz "," $ xs

wordz:: [Char] -> [Char] -> [[Char]]
wordz delim xs = 
	words . replace delim " " $ xs
  
-- Problem 4: It should reject negative numbers.
-- When given one negative number within the list of integers. It should throw an error.
-- When given +1 negative numbers, it should return the list of all given negative numbers.
  
add4::[Char] -> Either [Int] Int
add4 ('/':'/':delim:'\n':xs) = 
	addStringOfInts [delim] xs
add4 xs = 
	addStringOfInts "," xs
	
addStringOfInts delim xs 
	| length negatives == 1 = error "One negative!"
	| length negatives > 1  = Left negatives
	| otherwise = Right $ foldr (+) 0 listOfInts 
		where listOfInts = map toInt . wordz delim $ xs
		      negatives = filter (<0) listOfInts

-- Problem 5. It should ignore all numbers greater than 1000
-- Pending...

-- Problem 6. It should support delimiters of any length
-- Pending...

-- Problem 7. It should support multiple delimiters of the same length.
-- When given multiple delimiters, they should be of the form
-- "[delim1][delim2]...[delim3]" (with square brackets included)
-- Pending...

-- Problem 8. It should support multiple delimiters of multiple lengths.
-- Pending...

