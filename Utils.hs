module  Utils where
import Data.Time.Clock
import Data.Time.Calendar
import Data.Char
import Data.String

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--						VALIDATION
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

validNumber :: String -> Bool
validNumber []  = False
validNumber [x] = isDigit x
validNumber (x:xs) = if isDigit x then validNumber xs else False

validString :: String -> Bool
validString []  = False
validString [x] = isAlpha x
validString (x:xs) = if isAlpha x then validString xs else False

-- date format: yyyy-mm-dd
validDate :: String -> Bool
validDate [] = False
validDate dateString= if not((countChar '-' dateString )==2) then False
			else (validDay day)&& (validMonth month) && (validYear year)
			where 	year = splitted!!0 
				month = splitted!!1
				day = splitted!!2
				splitted= split dateString '-'

validDay:: String -> Bool 
validDay [] = False
validDay input = validNumber input && number `elem` [1..31]
	where number = unsafeStringToInt input 

validMonth:: String -> Bool 
validMonth [] = False
validMonth input = validNumber input && number `elem` [1..12]
	where number = unsafeStringToInt input 

validYear:: String -> Bool 
validYear [] = False
validYear input = validNumber input && number `elem` [1800..3000]
	where number = unsafeStringToInt input 

validEmail::String -> Bool
validEmail [] = False
validEmail input = '@' `elem` input

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--						UTILS
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- unsafe conversion from String to Int, use only after confirmation with validNumber
unsafeStringToInt :: String -> Int
unsafeStringToInt stringToConvert = read stringToConvert :: Int

-- split string with char, first replaces chosen character with spaces, uses words  function to split
split :: String ->Char -> [String]
split [] c= []
split stringToSplit c = 	if not (c `elem` stringToSplit) then [stringToSplit]
			else  words(replaceChar stringToSplit c ' ')

replaceChar :: String -> Char ->Char -> String
replaceChar [] _ _ = []
replaceChar (x:xs) old new = 	if x == old then new:(replaceChar xs old new)
				else x:(replaceChar xs old new)

countChar :: Char -> String -> Int
countChar c [x]
	| c == x      = 1
	| otherwise   	= 0
countChar c (x:xs)
	| c == x 	= 1 + countChar c xs
	| otherwise   	= 0 + countChar c xs

-- get current date as IO String
date :: IO String 
date = do
	fmap (showGregorian . utctDay) getCurrentTime

-- args: date in format yyyy:mm:dd
-- return wheter days are equal in case of birthday: day & month is equal
isDateBirthdailyEqual::String -> String -> Bool
isDateBirthdailyEqual [] [] = False
isDateBirthdailyEqual dateString1 dateString2 = validDate dateString1 && validDate dateString2 && (drop 5 dateString1) == (drop 5 dateString2)
