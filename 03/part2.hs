charToBit :: Char -> Int
charToBit c = read [c]::Int

convertToBitArray :: [Char] -> [Int]
convertToBitArray = map charToBit

getElementAt :: Int -> [a] -> a
getElementAt i = head . drop i

containsAt :: Eq a => Int -> a -> [a] -> Bool 
containsAt i el list = getElementAt i list == el

calcBitSum :: Int -> [[Int]] -> Int
calcBitSum position = sum . map (getElementAt position)

type CalcAtPosition = (Int -> [[Int]] -> Int)

calcMostCommonBit :: CalcAtPosition
calcMostCommonBit position list
    | bitSum >= threshhold = 1
    | bitSum < threshhold = 0
    | otherwise = error "Undefined state: 1 and 0 occour at the same amount"
    where 
        listLength = fromIntegral (length list)::Float
        threshhold = listLength / 2.0
        bitSum = fromIntegral (calcBitSum position list)::Float

calcLeastCommonBit :: CalcAtPosition
calcLeastCommonBit position list
    | bitSum >= threshhold = 0
    | bitSum < threshhold = 1
    | otherwise = error "Undefined state: 1 and 0 occour at the same amount"
    where 
        listLength = fromIntegral (length list)::Float
        threshhold = listLength / 2.0
        bitSum = fromIntegral (calcBitSum position list)::Float

binaryArrayToDecimal :: Int -> [Int] -> Int
binaryArrayToDecimal acc [] = acc
binaryArrayToDecimal acc list = acc + binaryArrayToDecimal ((2 ^ n) * head list) (tail list)
    where n = length list - 1

calcRating :: CalcAtPosition -> Int -> [[Int]] -> Int 
calcRating _ _ [] = error "No elements left"
calcRating _ 13 _ = error "Index has exceeded bit count"
calcRating _ _ [x] = binaryArrayToDecimal 0 x
calcRating calcBit i bitList =  
    calcRating calcBit (i + 1) fileteredList
    where 
        bit = calcBit i bitList
        fileteredList = filter (containsAt i bit) bitList
 
calcOxygenGeneratorRating = calcRating calcMostCommonBit 0
calcCO2Scrubberrating = calcRating calcLeastCommonBit 0

calcLifeSupportRating :: [[Int]] -> Int 
calcLifeSupportRating  bitList = calcCO2Scrubberrating bitList * calcOxygenGeneratorRating bitList

main::IO ()
main = do
    inputContent <- readFile "./input"
    let binaryStrings = words inputContent
    let bitArray = map convertToBitArray binaryStrings
    print $ calcLifeSupportRating bitArray

