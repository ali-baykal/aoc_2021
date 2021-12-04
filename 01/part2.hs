import TestData
import Control.Arrow (Arrow(first))
import Distribution.Simple.Program.HcPkg (list)

group :: Int -> [Int] -> [[Int]]
group n l
    | n < 0 = error "Negative or zero n"
    | n <= length l = (take n l) : (group n $ tail l)
    | n > length l = []  

isGrowing :: [Int] -> Bool
isGrowing list
    | length list < 2 = error "Too few elemnents"
    | length list > 2 = error "Too many elements"
    | otherwise = head list < last list

boolToNum :: Bool -> Int
boolToNum x
    | x = 1
    | otherwise = 0

solve :: [Int] -> Int
solve =  sum . map (boolToNum . isGrowing) . group 2 . map sum . group 3 

main::IO ()
main = do 
    print $ solve testData