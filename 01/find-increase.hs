import TestData

isGrowing :: Int -> Int -> Bool 
isGrowing x y = x < y 

boolToNum :: Bool -> Int
boolToNum x
    | x = 1
    | otherwise = 0

solve::[Int] -> Int
solve list =
    sum (map boolToNum (zipWith isGrowing list tailed))
    where tailed = tail list

main::IO ()
main = do 
    print (solve testData)

   
