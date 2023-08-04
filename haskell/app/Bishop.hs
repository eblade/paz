module Bishop where

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

printGraph :: BishopArray -> IO ()
printGraph b = do
    putStrLn "+--[ RandomArt ]--+"
    mapM (\start -> putStrLn $ ( "|" ++ (translate $ slice start blist) ++ "|" )) starts
    putStrLn "+-----------------+"
    where
        starts = [1, (xlim + 1)..arsz]
        blist = elems b
        slice start = (take xlim) . (drop (start - 1))
        translate = map (\x -> symbols ! x)

drunkenWalk :: [Char] -> BishopArray
drunkenWalk (x:xs) = walk xs 0 76 x newBoard

walk :: [Char] -> Int -> Int -> Char -> BishopArray -> BishopArray
walk     []     _   _    _ b = b
walk (x:xs)     4 pos    _ b = walk xs 0 pos x b
walk     xs stage pos temp b = walk xs (stage + 1) pos' temp' b'
    where
        (updated, pos') = move pos (getDirection $ otemp .&. 3)
        b' = if updated
            then (b // [(pos', (b ! pos') + 1)])
            else b
        temp' = chr $ shiftR otemp 2
        otemp = ord temp

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
        pos' = (limit 0 ylim y') * xlim + (limit 0 xlim x')
        updated = pos /= pos'


limit :: Int -> Int -> Int -> Int
limit lower upper =
    (max lower) . (min upper)
