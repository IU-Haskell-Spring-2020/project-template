module Split where

-- | Split a string using a delimiter.
--
-- >>> splitOn ' ' "hello world"
-- ["hello","world"]
--
-- >>> splitOn '/' "/path/to/file"
-- ["","path","to","file"]
--
-- 'splitOn' is the right inverse of 'unsplit':
--
-- prop> unsplit c (splitOn c s) == s
splitOn :: Char -> String -> [String]
splitOn _ "" = []
splitOn c s = splitOn' c s
  where
    splitOn' _ "" = [""]
    splitOn' c s = chunk :
      if null leftover
         then []
         else splitOn' c (dropWhile (== c) leftover)
      where
        chunk = takeWhile (/= c) s
        leftover = dropWhile (/= c) s

-- | Join chunks of strings, separated by a delimiter.
--
-- >>> unsplit ' ' ["hello","world"]
-- "hello world"
--
-- >>> unsplit '/' ["","path","to","file"]
-- "/path/to/file"
unsplit :: Char -> [String] -> String
unsplit _ []             = ""
unsplit _ [chunk]        = chunk
unsplit c (chunk:chunks) = chunk ++ [c] ++ unsplit c chunks

