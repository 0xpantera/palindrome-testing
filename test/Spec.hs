import Lib
import Data.Char (isPunctuation)
import Data.Text as T
import Test.QuickCheck
import Test.QuickCheck.Instances


assert :: Bool -> String -> String -> IO ()
assert test passStatement failStatement = if test
                                           then putStrLn passStatement
                                           else putStrLn failStatement

prop_punctuationInvariant text = preprocess text ==
                                   preprocess noPuncText
  where noPuncText = T.filter (not . isPunctuation) text
                                                
prop_reverseInvariant text = isPalindrome text ==
                             (isPalindrome (T.reverse text))


prop_caseInvariant text = preprocess text ==
                            preprocess caseLessText
  where caseLessText = T.toLower text


prop_whiteSpaceInvariant text = preprocess text ==
                                  preprocess strippedText
  where strippedText = T.strip text

main :: IO ()
main = do
  quickCheckWith stdArgs { maxSuccess = 1000 } prop_punctuationInvariant
  quickCheck prop_reverseInvariant
  quickCheck prop_caseInvariant
  quickCheck prop_whiteSpaceInvariant
  putStrLn "done!"
