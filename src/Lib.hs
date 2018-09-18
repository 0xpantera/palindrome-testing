module Lib
    ( isPalindrome
    , preprocess
    ) where

preprocess :: String -> String
preprocess text = filter (not . (`elem` ['!','.'])) text

isPalindrome :: String -> Bool
isPalindrome text = cleanText == reverse cleanText
  where cleanText = preprocess text
