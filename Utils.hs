module  Utils where
import Data.Time.Clock
import Data.Time.Calendar
import Data.Char

date :: IO (Integer,Int,Int) -- :: (year,month,day)
date = getCurrentTime >>= return . toGregorian . utctDay

validNumber :: String -> Bool
validNumber []  = False
validNumber [x] = isDigit x
validNumber (x:xs) = if isDigit x then validNumber xs else False

validString :: String -> Bool
validString []  = False
validString [x] = isAlpha x
validString (x:xs) = if isAlpha x then validString xs else False

