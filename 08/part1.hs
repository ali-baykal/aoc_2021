import qualified Data.Text as T
import Data.Array (Array, listArray, elems)
import Data.Set (Set, empty, insert, fromList, fromAscList, size, toList, member, intersection, difference)
import Data.Map (Map, fromList, (!), empty, insert)
import qualified Data.List as L
import Data.Maybe (isJust, fromJust)
import Control.Parallel (par)
import qualified Data.IntMap as M

data Segment = A | B | C | D | E | F | G deriving (Eq, Ord, Show)

data SegmentPosition = Top | Middle | Bottom | TopLeft | TopRight | BottomLeft | BottomRight deriving (Eq, Ord, Show)
type Possibilities = Map SegmentPosition (Set Segment)
type Configuration = Map SegmentPosition Segment

type SignalPattern = Set Segment
type InputValues = Array Int SignalPattern
type Digits = Array Int SignalPattern
type Display =  (InputValues, Digits)

type Input = [([T.Text], [T.Text])]

or :: [a -> Bool] -> a -> Bool
or predicates elem = foldl (\ acc f -> acc || f elem)  False predicates

mapCharToSegment :: Char -> Segment
mapCharToSegment 'a' = A
mapCharToSegment 'b' = B
mapCharToSegment 'c' = C
mapCharToSegment 'd' = D
mapCharToSegment 'e' = E
mapCharToSegment 'f' = F
mapCharToSegment 'g' = G
mapCharToSegment _ = error "Uniterpretable char"

_0 = Data.Set.fromList [Top, TopRight, TopLeft, BottomRight, BottomLeft, Bottom]
_1 = Data.Set.fromList [TopRight, BottomRight]
_2 = Data.Set.fromList [Top, TopRight, Middle, BottomLeft, Bottom]
_3 = Data.Set.fromList [Top, TopRight, Middle, BottomRight, Bottom]
_4 = Data.Set.fromList [TopRight, TopLeft, Middle, BottomRight]
_5 = Data.Set.fromList [Top, TopLeft, Middle, BottomRight, Bottom]
_6 = Data.Set.fromList [Top, TopLeft, Middle, BottomRight, BottomLeft, Bottom]
_7 = Data.Set.fromList [Top, TopRight, BottomRight]
_8 = Data.Set.fromList [Top, TopRight, TopLeft, Middle, BottomRight, BottomLeft, Bottom]
_9 = Data.Set.fromList [Top, TopRight, TopLeft, Middle, BottomRight, Bottom]

tuple2 :: [a] -> (a, a)
tuple2 list
    | length list == 2 = (head list, list !! 1)
    | otherwise = error "Only works with a list of two elements"


-- taken from http://book.realworldhaskell.org/read/concurrent-and-multicore-programming.html
parallelMap :: (a -> b) -> [a] -> [b]
parallelMap f (x:xs) = let r = f x
                       in r `par` r : parallelMap f xs
parallelMap _ _      = []

allSegments = [A, B, C, D, E, F, G];
allPositions = [Top, TopLeft, TopRight, Middle, BottomLeft, BottomRight, Bottom]
allSegmentsSet = Data.Set.fromList allSegments
allPosibilities :: Possibilities
allPosibilities = Data.Map.fromList $ zip allPositions (repeat allSegmentsSet)

deduceSegmentsarInSignal :: Char -> SignalPattern -> SignalPattern
deduceSegmentsarInSignal c = Data.Set.insert segment
    where segment = mapCharToSegment c

convertToSignalPattern :: String -> SignalPattern
convertToSignalPattern chars_ =
    rec chars_ Data.Set.empty
    where
        rec [] p = p
        rec chars pattern  = rec (tail chars) (deduceSegmentsarInSignal (head chars) pattern)

convertSingle :: ([T.Text], [T.Text]) -> Display
convertSingle ioTuple = (inputArray, outputArray)
    where
        convert  = map (convertToSignalPattern . T.unpack)
        inputPart = convert $ fst ioTuple
        inputArray = listArray (0,9) inputPart
        outputPart = convert $ snd ioTuple
        outputArray = listArray (0,3) outputPart

convertInput :: Input -> [Display]
convertInput = L.map convertSingle

parseFile :: T.Text -> Input
parseFile fileContent =
    group [] inputOutputList
    where
     inputOutputList = map T.words $ T.split (Main.or [('\n' ==), ('|' ==)]) $ T.strip fileContent
     group acc [] = acc
     group acc list = group (acc ++ [tuple2 (take 2 list)]) (drop 2 list)

signalPatternHasLength :: Int -> SignalPattern -> Bool
signalPatternHasLength num = (==) num . Data.Set.size

isRepresenting1 = signalPatternHasLength (Data.Set.size _1)
isRepresenting4 = signalPatternHasLength (Data.Set.size _4)
isRepresenting7 = signalPatternHasLength (Data.Set.size _7)
isRepresenting8 = signalPatternHasLength (Data.Set.size _8)

isNum :: Set SegmentPosition -> Configuration -> SignalPattern -> Bool
isNum positions configuration pattern =
 size positions == size pattern && all (\position -> Data.Set.member (configuration ! position) pattern) positions

is0 = isNum _0
is1 = isNum _1
is2 = isNum _2
is3 = isNum _3
is4 = isNum _4
is5 = isNum _5
is6 = isNum _6
is7 = isNum _7
is8 = isNum _8
is9 = isNum _9

getNum :: Configuration -> SignalPattern -> Maybe Int
getNum configuration pattern
    | is0 configuration pattern = Just 0
    | is1 configuration pattern = Just 1
    | is2 configuration pattern = Just 2
    | is3 configuration pattern = Just 3
    | is4 configuration pattern = Just 4
    | is5 configuration pattern = Just 5
    | is6 configuration pattern = Just 6
    | is7 configuration pattern = Just 7
    | is8 configuration pattern = Just 8
    | is9 configuration pattern = Just 9
    | otherwise = Nothing

isRelevantForPart1 :: SignalPattern -> Bool
isRelevantForPart1 = Main.or [isRepresenting1, isRepresenting4, isRepresenting7, isRepresenting8]

solvePart1 :: [Display] -> Int
solvePart1 = length . concatMap (filter isRelevantForPart1 . elems . snd)

deductPossibilities :: [SignalPattern] -> Possibilities -> Possibilities
deductPossibilities [] p = p
deductPossibilities patterns possibilities
    | isRepresenting1 currentPattern = deductNextStep . calcDeducedPosibilities _1 $ possibilities
    | isRepresenting4 currentPattern = deductNextStep . calcDeducedPosibilities _4 $ possibilities
    | isRepresenting7 currentPattern = deductNextStep . calcDeducedPosibilities _7 $ possibilities
    | otherwise = deductNextStep possibilities
    where
        currentPattern = head patterns
        deductNextStep = deductPossibilities (tail patterns)
        calcDeducedPosibilities :: Set SegmentPosition -> Possibilities -> Possibilities
        calcDeducedPosibilities num possibilities = newPosibilities
            where
                createDeductionFunc :: (Set Segment -> Set Segment -> Set Segment) -> SegmentPosition -> Possibilities -> Set Segment
                createDeductionFunc deduceSegments position possibilities = deduceSegments (possibilities ! position) currentPattern
                getDeductionFunc :: SegmentPosition -> (Possibilities -> Set Segment)
                getDeductionFunc position
                    | member position num = createDeductionFunc intersection position
                    | otherwise = createDeductionFunc difference position
                deductPosibilitiesForPostion :: Possibilities -> SegmentPosition -> Possibilities
                deductPosibilitiesForPostion possibilities position = Data.Map.insert position (getDeductionFunc position possibilities) possibilities
                newPosibilities = foldl deductPosibilitiesForPostion possibilities allPositions
                
getPossibleConfigurrations :: Possibilities -> [Configuration]
getPossibleConfigurrations possibilities = [
    Data.Map.fromList [(Top, t), (TopRight, tr), (TopLeft, tl), (Middle, m), (BottomRight, br), (BottomLeft, bl), (Bottom, b)] |
    t <-  getPossibilities Top,
    tr <- getPossibilities TopRight,
    tl <- getPossibilities TopLeft,
    m <- getPossibilities Middle,
    br <- getPossibilities BottomRight,
    bl <- getPossibilities BottomLeft,
    b <- getPossibilities Bottom,
    t /= tr,
    t /= tl,
    t /= m,
    t /= br,
    t /= bl,
    t /= b,
    tr /= tl,
    tr /= m,
    tr /= br,
    tr /= bl,
    tr /= b,
    tl /= m,
    tl /= br,
    tl /= bl,
    tl /= b,
    m /= br,
    m /= bl,
    m /= b,
    br /= bl,
    br /= b,
    bl /= b]
    where getPossibilities position = Data.Set.toList $ possibilities ! position

resolveDisplay  :: [Configuration] -> [SignalPattern] -> [Int]
resolveDisplay [] _ = []
resolveDisplay configurations display
    | all isJust maybeIntList = map fromJust maybeIntList
    | otherwise = resolveDisplay (tail configurations) display
    where
        currentConf = head configurations
        maybeIntList = map (getNum currentConf) display


solvePart2' :: Display -> [Int]
solvePart2' display =
    drop 10 $ resolveDisplay configurations patterns
    where
        inputPatterns = elems $ fst display
        outputPatterns = elems $ snd display
        patterns = inputPatterns ++ outputPatterns
        configurations = getPossibleConfigurrations $ deductPossibilities patterns allPosibilities

sumLists :: [[Int]] -> [Int]
sumLists = foldl (zipWith (+)) [0,0,0,0]

solvePart2 :: [Display] -> Int
solvePart2 display =
    sum [x*10^n | (x,n) <- zip reversed [0..3]]
    where
        summedDigits = sumLists . parallelMap solvePart2' $ display
        reversed = reverse summedDigits

main::IO ()
main =  readFile "./input" >>= print . solvePart2 . convertInput . parseFile . T.pack