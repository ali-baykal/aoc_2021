import Distribution.Simple.Program.HcPkg (list)
data Position = Position{
    aim:: Int,
    depth:: Int ,
    horizontal:: Int
}

tuplify2 :: [a] -> (a,a)
tuplify2 [x,y] = (x,y)
tuplify2 _ = error "List with more aor less than two items"

group :: Int -> [a] -> [[a]]
group n l
    | n < 0 = error "Negative or zero n"
    | n <= length l = (take n l) : (group n $ drop n l)
    | n > length l = []
    | otherwise = error "Non groupable list"

convertToTuples::[String] -> [(String, String )]
convertToTuples = map tuplify2 . group 2

convertParameter::String -> Int
convertParameter parameter = read parameter::Int

goDown :: Int -> Position -> Position
goDown x Position{depth = d, horizontal = h, aim = a} = Position {depth = d, horizontal= h, aim = a + x}

goUp :: Int -> Position -> Position
goUp x Position{depth = d, horizontal = h, aim = a} = Position {depth = d, horizontal= h, aim = a - x}

goForward :: Int -> Position -> Position
goForward x Position{depth = d, horizontal = h, aim = a } = Position {depth = d + a * x, horizontal= h + x, aim = a}

convertCommand::String -> (Int-> Position -> Position)
convertCommand "forward" = goForward
convertCommand "down" = goDown
convertCommand "up" = goUp
convertCommand _ = error "Unknown command"

convertToFunctionAndParameter :: (String, String) -> (Int -> Position -> Position, Int)
convertToFunctionAndParameter (c,p) = (convertCommand c, convertParameter p)

convertToModifier :: (Int-> Position -> Position, Int) -> (Position -> Position)
convertToModifier (fn, p) = fn p

multiplyPosition :: Position -> Int
multiplyPosition Position{depth = d, horizontal = h} = d * h;


main:: IO()
main = do 
    x <- readFile "./input"
    let tuples = convertToTuples $ words x  
    let modifiers = map (convertToModifier . convertToFunctionAndParameter) tuples
    let finalPosition = foldl (\acc fn -> fn acc) Position{horizontal = 0, depth = 0, aim = 0} modifiers
    print $ multiplyPosition finalPosition
