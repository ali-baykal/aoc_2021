import Data.Set(fromList, isSubsetOf, difference, toList)
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
convertToIntList =  map convertToInt . words . replaceWith ' ' ','

splitIntoBoards :: [Int] -> [Board]
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
        columns = map (getColumn board) [0..(boardWith - 1)] 
        rows = map (getRow board) [0..(boardHeight - 1)]
        rowsAndColumns = columns ++ rows

getWiningBoards :: [Int] -> [Board] -> [Board]
getWiningBoards numbers = filter (hasBoardWon numbers)

play :: Round -> [Int] -> [Board] -> [Board] -> ([Int], [Board])
play round allNumbers boardList previousWinnigBoards
    | round < 0 = error "no result"
    | length winingBoards < length previousWinnigBoards = (take (round + 1) allNumbers, toList  $ difference (fromList previousWinnigBoards) (fromList winingBoards)) 
    | otherwise = play (round - 1) allNumbers boardList winingBoards
    where 
        drawnNumbers = take round allNumbers
        winingBoards = getWiningBoards drawnNumbers boardList

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
    let boards = splitIntoBoards $ map convertToInt $ tail w
    let playResult = play (length numbers - 1) numbers boards (getWiningBoards numbers boards)
    let lastWinning =  playResult
    let drawnNumbers = fst lastWinning
    let winningBoards = snd lastWinning
    let scores = map (calcScore drawnNumbers) winningBoards
    print playResult
    print $ maximum scores