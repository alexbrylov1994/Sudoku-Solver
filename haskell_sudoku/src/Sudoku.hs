module Sudoku where

import Data.List
import Data.Maybe
import Data.Char

-------------------------------------------------------------------------

data Sudoku = Sudoku [[Maybe Int]]
 deriving ( Eq )

instance Show Sudoku where
  show (Sudoku ls) = show [ [ f m | m <- row] | row <- ls]
    where
      f m = fromMaybe "_" $ fmap (show) m

rows :: Sudoku -> [[Maybe Int]]
rows (Sudoku rs) = rs

--allBlankSudoku is a sudoku with just blanks
--Fills 9 by 9 sudoku with Nothing
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku(replicate 9 (replicate 9 Nothing))



-- isSudoku sud checks if sud is really a valid representation of a sudoku
-- puzzle
isSudoku :: Sudoku -> Bool
isSudoku (Sudoku sud)
-- checks if number of rows and colums are equal to 9 and if input from the file 
-- consist from Nothing or if between 1-9
          | numberRows && numberColums && numInput = isOkay (Sudoku sud)
          | otherwise = False
            
            where numberRows          = length sud == 9 --row check
                  numberColums        = and $ map ((== 9) . length) sud --column check
                  numInput            = and $ concatMap (map validInput) sud
                  validInput Nothing  = True -- if nothing, everything ok
                  validInput (Just x) = x >= 1 && x <= 9 -- check if input is between 1-9



-- isSolved sud checks if sud is already solved, i.e. there are no blanks
--Checks if Nothing isn't element in any rows
isSolved :: Sudoku -> Bool
isSolved = (Nothing `notElem`) . concat . rows

-------------------------------------------------------------------------
--rowProcessor
--rowProcessor takes a Maybe Int. If it it a "Nothing" then it sends back a ".". Otherwise, it sends back the Maybe Int as a Char.
rowProcessor :: Maybe Int -> Char
rowProcessor el 
                    | el == Nothing = '.'
                    | otherwise = intToDigit(fromJust(el))

--printAssist
--printAssist takes the result of rows() on a sudoku and sends each row to rowProcesor using map. 
printAssist :: [[Maybe Int]] -> [String]
printAssist [] = []
printAssist (arr:xs) = map rowProcessor arr : printAssist xs


-- printSudoku sud prints a representation of the sudoku sud on the screen
printSudoku :: Sudoku -> IO ()
printSudoku sd = mapM_ putStrLn(printAssist(rows(sd)))




-- readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku

readSudoku file = readFile file >>= return . Sudoku . map sudRow . lines -- reads file and after import it line by line into Sudoku
  
  where sudRow = map toChar
        
        toChar char  
                      | char == '.'    = Nothing -- converts "." to type nothing
                      | char `elem` ['1'..'9'] = Just (digitToInt char) -- Checks if char is between 1 and 9 and it turns into int
                      | otherwise            = error "It's not a Sudoku!" -- error message


-------------------------------------------------------------------------

type Block = [Maybe Int]
isOkayBlock :: Block -> Bool
isOkayBlock [] = True
isOkayBlock (x@(Just a):xs) = not (elem x xs) && isOkayBlock xs
isOkayBlock (Nothing:xs) = isOkayBlock xs

rowBlocks :: [[Maybe Int]] -> [[Maybe Int]]
rowBlocks rs = rs

colBlocks :: [[Maybe Int]] -> [[Maybe Int]]
colBlocks rs = (map reverse . transpose) rs

squareBlocks :: [[Maybe Int]] -> [[Maybe Int]]
squareBlocks rs =  concat [ 
                            [concat ( map (take 3. drop d) (take 3 . drop d' $ rs)) | d <- [0, 3, 6] ]
                            |
                            d' <- [0, 3, 6]
                          ]

blocks :: Sudoku -> [Block]
blocks (Sudoku rs) = (rowBlocks rs) ++ (colBlocks rs) ++ (squareBlocks rs)


isOkay :: Sudoku -> Bool
-- takes sudoku, sends it to another function to find if block in Sudoku is fine
isOkay sud = and [ isOkayBlock block | block <- blocks sud] 


type Pos = (Int,Int)
blank :: Sudoku -> Pos
blank sud
          | list == [] = (-1,-1) --if empty, gives error 
          | otherwise = list !! 0 -- otherwise returns first element from the list
            where list = [(row, col) | col <- [0..8], row <- [0..8], ((rows sud) !! row) !! col==Nothing ]
            -- pretty much we have to check each row and column from index 0 to 8 until we find Nothing somewhere
                    

(!!=) :: [a] -> (Int,a) -> [a]


(!!=) list (i,elem) = take i list ++ [elem] ++ drop (i + 1) list 
-- drops element from the specific indext of the list and adds a new element at the position of the old element
-- was found there and modified after http://stackoverflow.com/questions/15530511/how-to-edit-nth-element-in-a-haskell-list



update :: Sudoku -> Pos -> Maybe Int -> Sudoku
update (Sudoku sud) (ci,ri) input = Sudoku $ (!!=) sud (ci, (sud!!ci) !!= (ri, input))
--updates each row using (!!=funcion),we pass sudoku, index, and a new element, after (!!=) does the work and we place this row into sudoku


--three possible cases for solving sudoku: a block contains same digit twice. cannot solve: answer Nothing
--2: sud does not contain any blanks i.e the sudoku is already solved - just sudoku is the answer 
--3: recursively solve sud 9 times; each time we update the blank cell with digit from 1 to 9. 
--if succeeded - return that answer, else return Nothing
solve :: Sudoku -> [Maybe Sudoku]
solve sd
        | not(isSudoku(sd)) = [Nothing] --if sd does not contain any blank cell & solution is invalid
        | blank sd == (-1,-1) && isOkay sd = [Just sd] --if sd does not contain any blank cell & solution is valid
        | otherwise = filter (\x -> x /= Nothing) solution -- Filters a list of Nothing form the solution 
      
         where
              emptyLocation = blank sd
              solution = concat [solve (update sd emptyLocation (Just index)) | index <- [1..9]]
              -- we pass index to blank (1-9), wheb we finds nothing, it updates it to a different value, we do it for each row
              -- and at the end we do concat to have all the 9 rows in sudoku


--readAndSolve reads a sudoku from file, calls solve to get all possible solutions for a sudoku and prints the solutions.
readAndSolve :: FilePath -> IO ()
readAndSolve filename = do
                            sudFile <- readSudoku filename
                            let solvedSud = solve sudFile
                            indexHelp (solvedSud)


--indexHelp takes care of printing all solutions given a list of sulutions as an argument
indexHelp :: [Maybe Sudoku]-> IO()
indexHelp [] = putStr("") --base case - empty list
indexHelp sd = do
              printHelp(head sd) --print the first element of the list
              indexHelp(tail sd) -- recurse with the rest of the elements of the list


--printHelp
--printHelp takes the Maybe Sudoku input and if it is a Nothing then it prints "No Solution!". Otherwise it uses the printSudoku function with fromJust
printHelp :: Maybe Sudoku -> IO()
printHelp sd 
              |sd == Nothing = putStrLn("No Solution!")
              |otherwise = printSudoku(fromJust sd)

example :: Sudoku
example =
    Sudoku
      [ [Just 3, Just 6, Nothing,Nothing,Just 7, Just 1, Just 2, Nothing,Nothing]
      , [Nothing,Just 5, Nothing,Nothing,Nothing,Nothing,Just 1, Just 8, Nothing]
      , [Nothing,Nothing,Just 9, Just 2, Nothing,Just 4, Just 7, Nothing,Nothing]
      , [Nothing,Nothing,Nothing,Nothing,Just 1, Just 3, Nothing,Just 2, Just 8]
      , [Just 4, Nothing,Nothing,Just 5, Nothing,Just 2, Nothing,Nothing,Just 9]
      , [Just 2, Just 7, Nothing,Just 4, Just 6, Nothing,Nothing,Nothing,Nothing]
      , [Nothing,Nothing,Just 5, Just 3, Nothing,Just 8, Just 9, Nothing,Nothing]
      , [Nothing,Just 8, Just 3, Nothing,Nothing,Nothing,Nothing,Just 6, Nothing]
      , [Nothing,Nothing,Just 7, Just 6, Just 9, Nothing,Nothing,Just 4, Just 3]
      ]

complete :: Sudoku
complete =
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