import Lib
import Data.Char (isPunctuation)


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
  putStrLn "Running tests..."
  assert (isPalindrome "racecar") "passed 'racecar'" "FAIL: 'racecar'"
  assert (isPalindrome "racecar!") "passed 'racecar!'" "'FAIL: 'racecar!'"
  assert ((not . isPalindrome) "cat") "passed 'cat'" "FAIL: 'cat'"
  assert (isPalindrome "racecar.") "passed 'racecar.'" "FAIL: 'racecar.'"
  putStrLn "done!"
