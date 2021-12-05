import qualified Data.Map.Strict as M

type Coordinate = (Int, Int)
type Line = (Coordinate, Coordinate)
type Field =  M.Map Coordinate Int 

replaceWith :: Eq a => a -> a -> [a] -> [a]
replaceWith _ _ [] = []
replaceWith x y list
    | head list == y = x : replaceWith x y (tail list)
    | otherwise = head list : replaceWith x y (tail list)

parseInt::String -> Int
parseInt str = read str::Int

rollingZip' :: [a] -> [a] -> [a] -> [a] -> [(a,a)]
rollingZip' [] _ _ _ = []
rollingZip' _ [] _ _ = []
rollingZip' originalX originalY x y
    | lenOrigX == lenOrigY = zip originalX originalY
    | lenOrigX > lenOrigY && lenY == 0 = rollingZip' originalX originalY x originalY
    | lenOrigX > lenOrigY && lenX == 0 = []
    | lenOrigX < lenOrigY && lenX == 0 = rollingZip' originalX originalY originalX y
    | lenOrigX < lenOrigY && lenY == 0 = []
    | otherwise = (head x, head y) : rollingZip' originalX originalY (tail x) (tail y)
    where
        lenOrigX = length originalX
        lenOrigY = length originalY
        lenX = length x
        lenY = length y

rollingZip :: [a] -> [a] -> [(a,a)]
rollingZip x y = rollingZip' x y x y

stringToCoordinate :: String -> Coordinate
stringToCoordinate  str =
    (head intList, last intList)
    where intList = map parseInt $ words $ replaceWith ' ' ','  str

twoSizedWordsToLine::[String]  -> Line
twoSizedWordsToLine wordList
    | length wordList /= 2 = error "Only array of the size of two are allowed"
    | otherwise = (xCoord, yCoord)
    where
        xCoordStr = head wordList
        yCoordStr = last wordList
        xCoord = stringToCoordinate xCoordStr
        yCoord = stringToCoordinate yCoordStr


wordsToLines'::[String] -> [Line]
wordsToLines' [] = []
wordsToLines' wordList = line : wordsToLines' (drop 2 wordList)
    where line = twoSizedWordsToLine $ take 2 wordList

wordsToLines :: [String] -> [Line]
wordsToLines = wordsToLines' . filter (/= "->")

isHorizontal :: Line -> Bool
isHorizontal line = 
    fromX == toX 
    where
        from = fst line
        to = snd line
        fromX = fst from
        toX = fst to

makeRange :: Int -> Int -> [Int]
makeRange x y 
    | x > y = [x, (x-1)..y]
    | otherwise = [x..y]

expandLine :: Line -> [Coordinate]
expandLine line =
    rollingZip xRange yRange
    where
        from = fst line
        to = snd line
        fromX = fst from
        fromY = snd from
        toX = fst to
        toY = snd to
        xRange = makeRange fromX toX
        yRange = makeRange fromY toY

initialField :: Field
initialField =  M.empty 
insertAt ::  Int -> a -> [a] ->  [a]
insertAt _ elem [] = [elem]
insertAt index elem list= take index list ++ [elem] ++ drop (index + 1) list

placeInField :: Field -> Coordinate -> Field
placeInField field coord 
    | not (M.member coord field) = M.insert coord 1 field
    | otherwise  = M.insert coord incremented field
    where
        incremented = (+) 1 $ field M.! coord

fillField :: Field -> [Coordinate] -> Field
fillField  = foldl placeInField
main:: IO()
main = do
    fileContent <- readFile "./input"
    let w = words fileContent
    let lines = wordsToLines w
    let coordList = concatMap expandLine lines
    let field = initialField
    let filledField = fillField field coordList
    let result = M.size $ M.filter (>= 2) filledField
    print result 