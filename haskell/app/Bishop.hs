module Bishop where

import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import Data.Word (Word8)
import Data.Array
import Data.Bits
import Data.Char (ord, chr)
import System.IO

type BishopArray = Array Int Int

data Direction = NW | NE | SW | SE deriving (Eq, Show)
getDirection 0 = NW
getDirection 1 = NE
getDirection 2 = SW
getDirection 3 = SE
getDirection _ = error "unknown direction"

xlim = 17
ylim = 9
arsz = xlim * ylim

symbols :: Array Int Char
symbols = listArray (0, (length raw) - 1) raw
    where 
        raw = " .o+=*BOX@%&#/^SE"

newBoard :: BishopArray
newBoard = listArray (0, (arsz - 1)) $ map (\_ -> 0) [1..arsz]

printGraph :: Handle -> BishopArray -> IO ()
printGraph handle b = do
    put "+--[ RandomArt ]--+"
    mapM (\start -> put $ ( "|" ++ (translate $ slice start blist) ++ "|" )) starts
    put "+-----------------+"
    where
        starts = [1, (xlim + 1)..arsz]
        blist = elems b
        slice start = (take xlim) . (drop (start - 1))
        translate = map (\x -> symbols ! x)
        put = hPutStrLn handle

drunkenWalk :: ByteString -> BishopArray
drunkenWalk bs = walk xs 0 76 x newBoard
    where
        x = word8toInt $ B.head bs
        xs = map word8toInt $ (B.unpack . B.tail) bs


walk :: [Int] -> Int -> Int -> Int -> BishopArray -> BishopArray
walk     []     _   _    _ b = b
walk (x:xs)     4 pos    _ b = walk xs 0 pos x b
walk     xs stage pos temp b = walk xs (stage + 1) pos' temp' b'
    where
        (updated, pos') = move pos (getDirection $ temp .&. 3)
        b' = if updated
            then (b // [(pos', (b ! pos') + 1)])
            else b
        temp' = shiftR temp 2

move :: Int -> Direction -> (Bool, Int)
move pos direction = (updated, pos')
    where
        x = mod pos xlim
        y = div pos xlim
        x' = case direction of
            NW -> x - 1
            NE -> x + 1
            SW -> x - 1
            SE -> x + 1
        y' = case direction of
            NW -> y - 1
            NE -> y - 1
            SW -> y + 1
            SE -> y + 1
        pos' = (limit ylim y') * xlim + (limit xlim x')
        updated = pos /= pos'


limit :: Int -> Int -> Int
limit upper =
    (max 0) . (min (upper - 1))

word8toInt :: Word8 -> Int
word8toInt x = fromInteger x'
    where
        x' = toInteger x

intToWord8 :: Word8 -> Int
intToWord8 x = fromInteger x'
    where
        x' = toInteger x
