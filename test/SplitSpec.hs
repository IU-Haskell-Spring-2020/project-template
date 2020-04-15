module SplitSpec where

import           Data.Char       (isSpace)
import           Test.Hspec
import           Test.QuickCheck

import           Split

splitOn_words_prop :: Property
splitOn_words_prop =
  forAll (arbitrary `suchThat` onlyWhitespace)
   (\line -> splitOn ' ' line == words line)
  where
    onlyWhitespace = all (== ' ') . filter isSpace

unsplit_splitOn_prop :: Char -> String -> Bool
unsplit_splitOn_prop c s = unsplit c (splitOn c s) == s

splitOn_notElem_prop :: Char -> Property
splitOn_notElem_prop c =
  forAll (arbitrary `suchThat` (\s -> c `notElem` s && not (null s)))
    (\s -> splitOn c s == [s])

splitOn_empty_prop :: Char -> Bool
splitOn_empty_prop c = splitOn c "" == []

splitOn_unsplit_prop :: Char -> [String] -> Bool
splitOn_unsplit_prop c chunks = splitOn c (unsplit c chunks) == chunks

length_splitOn_prop :: Char -> String -> Bool
length_splitOn_prop c s =
  length (splitOn c s) <= length (filter (== c) s) + 1

splitOn_sym_prop :: Char -> String -> Bool
splitOn_sym_prop c s =
  map reverse (reverse (splitOn c (reverse s))) == splitOn c s

spec :: Spec
spec = do
  describe "Tests for splitting functions" $ do
    it "splitOn ' ' = words" $ property
      splitOn_words_prop
    it "unsplit c . splitOn c = id" $ property
      unsplit_splitOn_prop
    it "splitOn c s = [s]  when c is not in s" $ property
      splitOn_notElem_prop
    it "splitOn c \"\" = []" $ property
      splitOn_empty_prop
    it "splitOn c . unsplit c = id" $ property
      splitOn_unsplit_prop
    it "splitOn produces at most (n + 1) chunks for n delimiters" $ property
      length_splitOn_prop
    it "splitOn does not depend on input direction" $ property
      splitOn_sym_prop
