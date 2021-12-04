charToBit :: Char -> Int
charToBit c = read [c]::Int

convertToBitArray :: [Char] -> [Int]
convertToBitArray = map charToBit

getElementAt :: Int -> [a] -> a
getElementAt i = head . drop i

calcBitSum :: Int -> [[Int]] -> Int
calcBitSum position = sum . map (getElementAt position)

type CalcAtPosition = (Int -> [[Int]] -> Int)

calcMostCommonBit :: CalcAtPosition
calcMostCommonBit position list
    | bitSum > threshhold = 1
    | bitSum < threshhold = 0
    | otherwise = error "Undefined state: 1 and 0 occour at the same amount"
    where 
        listLength = fromIntegral (length list)::Float
        threshhold = listLength / 2.0
        bitSum = fromIntegral (calcBitSum position list)::Float

calcLeastCommonBit :: CalcAtPosition
calcLeastCommonBit position list
    | bitSum > threshhold = 0
    | bitSum < threshhold = 1
    | otherwise = error "Undefined state: 1 and 0 occour at the same amount"
    where 
        listLength = fromIntegral (length list)::Float
        threshhold = listLength / 2.0
        bitSum = fromIntegral (calcBitSum position list)::Float

calcCommonList :: CalcAtPosition -> [[Int]] -> [Int] 
calcCommonList calcAtPostion bitList = map (\calc -> calc bitList) calculators
    where calculators = map calcAtPostion [0..11]

calcMostCommonList = calcCommonList calcMostCommonBit 
calcLeastCommonList = calcCommonList calcLeastCommonBit 

binaryArrayToDecimal :: Int -> [Int] -> Int
binaryArrayToDecimal acc [] = acc
binaryArrayToDecimal acc list = acc + binaryArrayToDecimal ((2 ^ n) * head list) (tail list)
    where n = length list - 1

calcGammaRate :: [[Int]] -> Int 
calcGammaRate = binaryArrayToDecimal 0 . calcMostCommonList

calcEpsilonRate :: [[Int]] -> Int 
calcEpsilonRate = binaryArrayToDecimal 0 . calcLeastCommonList

calcPowerConsumption :: [[Int]] -> Int
calcPowerConsumption bitList = calcGammaRate bitList * calcEpsilonRate bitList
 
main::IO ()
main = do
    inputContent <- readFile "./input"
    let binaryStrings = words inputContent
    let bitArray = map convertToBitArray binaryStrings
    print $ calcPowerConsumption bitArray

