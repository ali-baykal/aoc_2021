import Data.Set(Set, fromList, isSubsetOf, difference, toList, filter, map)
import Data.Tuple (fst, snd)
import Data.List (sort)

boardHeight = 5
boardWith = 5
boardSize = boardHeight * boardWith

type Board = [Int]
type Round = Int

getElementAt :: Int -> [a] -> a
getElementAt i = head . drop i

replaceWith :: Eq a => a -> a -> [a] -> [a]
replaceWith _ _ [] = []
replaceWith x y list
    | head list == y = x : replaceWith x y (tail list)
    | otherwise = head list : replaceWith x y (tail list)

convertToInt :: String -> Int 
convertToInt str =  read str::Int

convertToIntList :: String -> [Int]
convertToIntList =  Prelude.map convertToInt . words . replaceWith ' ' ','

splitIntoBoards :: [Int] ->[Board]
splitIntoBoards [] = []
splitIntoBoards list = take boardSize list : splitIntoBoards (drop boardSize list)

getColumn :: Board -> Int  -> [Int]
getColumn [] _ = []
getColumn board columnNum = getElementAt columnNum board : getColumn (drop boardWith board) columnNum

getRow :: Board -> Int -> [Int]
getRow board rowNum = take boardWith $ drop (rowNum * boardWith) board

hasWon :: [Int] -> [Int] -> Bool
hasWon numbers colOrRow = 
    colOrRowSet `isSubsetOf` numbersSet
    where
        numbersSet = fromList numbers;
        colOrRowSet = fromList colOrRow;


hasBoardWon :: [Int] -> Board -> Bool
hasBoardWon numbers board = 
    any (hasWon numbers) rowsAndColumns
    where 
        columns = Prelude.map (getColumn board) [0..(boardWith - 1)] 
        rows = Prelude.map (getRow board) [0..(boardHeight - 1)]
        rowsAndColumns = columns ++ rows

getWiningBoards :: [Int] -> Set Board -> Set Board
getWiningBoards numbers = Data.Set.filter (hasBoardWon numbers)

playInReverse :: Round -> [Int] -> Set Board -> [Int] -> Set Board -> ([Int], Set Board)
playInReverse round allNumbers boardSet previousNumbers previousWinningBoards
    | round < 0 = error "no result"
    | not (winningBoards == previousWinningBoards) = (previousNumbers, difference previousWinningBoards winningBoards) 
    | otherwise = playInReverse (round - 1) allNumbers boardSet drawnNumbers winningBoards
    where 
        drawnNumbers = take round allNumbers
        winningBoards = getWiningBoards drawnNumbers boardSet

play :: [Int] -> Set Board -> ([Int], Set Board)
play numbers boards = playInReverse (length numbers - 1) numbers boards numbers (getWiningBoards numbers boards)

calcScore :: [Int] -> Board -> Int 
calcScore numbers board = 
    sum unmarkedNumbers * last numbers
    where
        boardSet = fromList board
        numbersSet = fromList numbers
        unmarkedNumbers = difference boardSet numbersSet

main::IO()
main = do
    fileContent <- readFile "./input"
    let w = words fileContent
    let numbers = convertToIntList $ head w
    let boards = fromList $ splitIntoBoards $ Prelude.map convertToInt $ tail w
    let playResult = play numbers boards 
    let drawnNumbers = fst playResult
    let winningBoards = snd playResult
    let scores = Data.Set.map (calcScore drawnNumbers) winningBoards
    print $ maximum scores