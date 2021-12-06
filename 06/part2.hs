type Population = [Int];
type Input = [Int];

-- stack overflow answer to how to split a string
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'


transformInitialData' :: Population -> Int -> Population
transformInitialData' population timer = 
     h ++ (timerCount : (tail t))
            where
                splitPopulation = splitAt timer population
                h = fst splitPopulation
                t = snd splitPopulation
                timerCount = (+1) $ head t

transformInitialData :: [Int] -> Population
transformInitialData  = foldl transformInitialData' (replicate 9 0)

simulate:: Int -> Population -> Population
simulate 0 population = population
simulate days population = 
    simulate (days - 1) nextGen
    where
        created = head population 
        populationWithCreated = (tail population) ++ [created]
        h = take 6 populationWithCreated
        t = drop 6 populationWithCreated
        withTimer6 = (head t) + created
        nextGen = h ++ (withTimer6 : (tail t))


solve :: Input -> Int
solve = sum . simulate 256 . transformInitialData

parseInput :: String -> [Int]
parseInput = map (\str -> read str::Int) . wordsWhen (==',')

main :: IO ()
main = do
    fileContent <- readFile "./input"
    (print . solve. parseInput) fileContent
