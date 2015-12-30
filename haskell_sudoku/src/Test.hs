module Main where
import Sudoku
import Data.Maybe

check :: (Show a, Eq a, Show b) => (b -> a) -> a -> b -> Maybe String
check f expectation arg 
  | expectation == (f arg) = Nothing
  | otherwise              = Just $ "for < " ++ (show arg)
                             ++ " > expected  < " ++ (show expectation)
                             ++ " > got < " ++ (show (f arg)) ++ " >"

--------------------------------------------------------------------------------------
checkNoArg f expectation
  | expectation == f       = Nothing
  | otherwise              = Just $ "for < " ++ ""
                             ++ " > expected  < " ++ (show expectation)
                             ++ " > got < " ++ (show f) ++ " >"

checkAllBlank = checkNoArg $ allBlankSudoku
allBlank :: Sudoku
allBlank =
    Sudoku
      [ [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
      , [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
      , [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
      , [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
      , [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
      , [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
      , [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
      , [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
      , [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
      ]

--------------------------------------------------------------------------------------
checkIsSudoku = check $ isSudoku
--------------------------------------------------------------------------------------
checkIsSolved = check $ isSolved
--------------------------------------------------------------------------------------
checkBlank    = check $ blank
--------------------------------------------------------------------------------------
checkReplace  = check $ uncurry (!!=)
--------------------------------------------------------------------------------------
uncurry3 f    = \(a,b,c) -> f a b c
checkUpdate   = check $ uncurry3 update
oneFilled   =
    Sudoku
      [ [Nothing,Just 2,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
      , [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
      , [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
      , [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
      , [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
      , [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
      , [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
      , [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
      , [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
      ]
--------------------------------------------------------------------------------------
checkSolve = check $ solve
oneBlank =
    Sudoku
      [ [Nothing, Just 6, Just 4, Just 8, Just 7, Just 1, Just 2, Just 9, Just 5]
      , [Just 7, Just 5, Just 2, Just 9, Just 3, Just 6, Just 1, Just 8, Just 4]
      , [Just 8, Just 1, Just 9, Just 2, Just 5, Just 4, Just 7, Just 3, Just 6]
      , [Just 5, Just 9, Just 6, Just 7, Just 1, Just 3, Just 4, Just 2, Just 8]
      , [Just 4, Just 3, Just 1, Just 5, Just 8, Just 2, Just 6, Just 7, Just 9]
      , [Just 2, Just 7, Just 8, Just 4, Just 6, Just 9, Just 3, Just 5, Just 1]
      , [Just 6, Just 4, Just 5, Just 3, Just 2, Just 8, Just 9, Just 1, Just 7]
      , [Just 9, Just 8, Just 3, Just 1, Just 4, Just 7, Just 5, Just 6, Just 2]
      , [Just 1, Just 2, Just 7, Just 6, Just 9, Just 5, Just 8, Just 4, Just 3]
      ]
solved =
    Sudoku
      [ [Just 3, Just 6, Just 4, Just 8, Just 7, Just 1, Just 2, Just 9, Just 5]
      , [Just 7, Just 5, Just 2, Just 9, Just 3, Just 6, Just 1, Just 8, Just 4]
      , [Just 8, Just 1, Just 9, Just 2, Just 5, Just 4, Just 7, Just 3, Just 6]
      , [Just 5, Just 9, Just 6, Just 7, Just 1, Just 3, Just 4, Just 2, Just 8]
      , [Just 4, Just 3, Just 1, Just 5, Just 8, Just 2, Just 6, Just 7, Just 9]
      , [Just 2, Just 7, Just 8, Just 4, Just 6, Just 9, Just 3, Just 5, Just 1]
      , [Just 6, Just 4, Just 5, Just 3, Just 2, Just 8, Just 9, Just 1, Just 7]
      , [Just 9, Just 8, Just 3, Just 1, Just 4, Just 7, Just 5, Just 6, Just 2]
      , [Just 1, Just 2, Just 7, Just 6, Just 9, Just 5, Just 8, Just 4, Just 3]
      ]

tests :: [Maybe String]
tests = [
    checkAllBlank allBlank
  , checkIsSudoku True example
  , checkIsSolved True complete
  , checkIsSolved False example
  , checkBlank (0,0) allBlank
  , checkReplace ["a","apa","c","d"] (["a","b","c","d"], (1, "apa"))
  , checkUpdate oneFilled (allBlank,(0,1),(Just 2))
  , checkSolve [Just solved] oneBlank
  ]
main :: IO ()
main = for each
  where
    for [] = return ()
    for (x:xs) = do
      _ <- putStrLn x
      for xs
    each = [ result | test <- tests, result <- maybeToList test ]
