import qualified Data.Array as A;

type Population = A.Array Int Int;
type Input = [Int];

-- stack overflow answer to how to split a string
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'


transformInitialData' :: Population -> Int -> Population
transformInitialData' population timer =
     population A.// [(timer, incremented)]
     where
         current = population A.! timer
         incremented = current + 1

rotate :: Population -> Population
rotate population = A.listArray (0, 8) (tail values ++ [head values])
    where values = A.elems population

transformInitialData :: [Int] -> Population
transformInitialData  = foldl transformInitialData' (A.listArray (0, 8) (repeat 0))

transitionPopulation :: Population -> Population
transitionPopulation population =
    populationWithCreated A.// [(6, createCount + withTimer6)]
    where
        createCount = population A.! 0
        populationWithCreated = rotate population
        withTimer6 = populationWithCreated A.! 6

simulate:: Int -> Population -> Population
simulate 0 = id
simulate days = (!! max 0 days) . iterate transitionPopulation

solve :: Input -> Int
solve = sum . simulate 256 . transformInitialData

parseInput :: String -> [Int]
parseInput = map (\str -> read str::Int) . wordsWhen (==',')

main :: IO ()
main = do
    fileContent <- readFile "./input"
    (print . solve. parseInput) fileContent
