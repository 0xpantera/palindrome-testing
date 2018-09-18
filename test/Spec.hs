import Lib
import Data.Char (isPunctuation)
import Test.QuickCheck

assert :: Bool -> String -> String -> IO ()
assert test passStatement failStatement = if test
                                           then putStrLn passStatement
                                           else putStrLn failStatement

prop_punctuationInvariant text = preprocess text ==
                                   preprocess noPuncText
  where noPuncText = filter (not . isPunctuation) text
                                                
prop_reverseInvariant text = isPalindrome text ==
                             (isPalindrome (reverse text))
                             

main :: IO ()
main = do
  quickCheckWith stdArgs { maxSuccess = 1000 } prop_punctuationInvariant
  quickCheck prop_reverseInvariant
  putStrLn "done!"
