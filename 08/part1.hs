import qualified Data.Text as T
import Data.Array (Array, listArray, elems)
import Data.Set (Set, empty, insert, fromList, fromAscList, size)
import qualified Data.List as L

data Segment = A | B | C | D | E | F | G
 deriving (Eq, Ord, Show)

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

setCharInSignal :: Char -> SignalPattern -> SignalPattern
setCharInSignal c = insert segment
    where segment = mapCharToSegment c


convertToSignalPattern :: [Char] -> SignalPattern
convertToSignalPattern chars_ =
    rec chars_ empty
    where
        rec [] p = p
        rec chars pattern  = rec (tail chars) (setCharInSignal (head chars) pattern)


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
    zip inputOutputList (tail inputOutputList)
    where
     inputOutputList = map T.words $ T.split ('|' ==) fileContent

signalPatternHasLength :: Int -> SignalPattern -> Bool 
signalPatternHasLength num = (==) num . Data.Set.size

isRepresenting1 = signalPatternHasLength 2
isRepresenting4 = signalPatternHasLength 4
isRepresenting7 = signalPatternHasLength 3
isRepresenting8 = signalPatternHasLength 7


isRelevantForPart1 :: SignalPattern -> Bool
isRelevantForPart1 = Main.or [isRepresenting1, isRepresenting4, isRepresenting7, isRepresenting8]

solvePart1 :: [Display] -> Int
solvePart1 = length . concatMap (filter isRelevantForPart1 . elems . snd)
-- solvePart1 = map  (elems.snd)

main::IO ()
main =  readFile "./input" >>= print . solvePart1 . convertInput . parseFile . T.pack