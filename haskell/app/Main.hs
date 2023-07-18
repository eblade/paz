module Main where

import System.IO
import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import Data.Word (Word8)
import qualified Data.ByteString.Base64 as Base64
import qualified Crypto.Hash.SHA512 as SHA512
import qualified Data.ByteString.Char8 as C

data Configuration = Configuration {
    master :: String,
    site :: String,
    plength :: Int,
    miniterations :: Int }

main :: IO ()
main = do
    hSetBuffering stdin LineBuffering
    putStrLn (C.unpack (B.take (plength config) result))
    putStrLn (show iterations)
        where
            config = Configuration {
                site="site",
                master="master",
                plength=15,
                miniterations=10 }
            start = makeStart (master config) (site config)
            calculate = calculate' (plength config)
            (iterations, result) = pazify calculate (check (plength config) (miniterations config)) start

makeStart :: String -> String -> ByteString
makeStart master site = C.pack (master ++ ":" ++ site)

translate :: Word8 -> Word8
translate 43 = 57 -- + -> 9
translate 47 = 56 -- / -> 8 
translate 61 = 65 -- = -> A
translate x = x

-- calculate :: ByteString -> ByteString
-- calculate = calculate' 15
calculate' :: Int -> ByteString -> ByteString
calculate' length s = B.map translate (Base64.encode digest)
    where
        digest = SHA512.finalize ctx
        ctx = SHA512.update ctx0 s
        ctx0 = SHA512.init

check :: Int -> Int -> Int -> ByteString -> Bool
check plength miniterations n x = enough && (startsWithLowerCase pw) && (hasTheStuffNoKnowns pw)
    where
        enough = (n >= miniterations)
        pw = B.take plength x

startsWithLowerCase :: ByteString -> Bool
startsWithLowerCase x = ((B.head x) >= 97) && ((B.head x) <= 122)

hasTheStuffNoKnowns :: ByteString -> Bool
hasTheStuffNoKnowns x =
    if B.null x
    then False
    else if isUpperCase x0
        then hasTheStuffKnownToHaveUppers (B.tail x)
        else if isNumber x0
            then hasTheStuffKnownToHaveNumbers (B.tail x)
            else hasTheStuffNoKnowns (B.tail x)
    where x0 = B.head x

hasTheStuffKnownToHaveUppers :: ByteString -> Bool
hasTheStuffKnownToHaveUppers x =
    if B.null x
    then False
    else if isNumber x0
        then True
        else hasTheStuffKnownToHaveUppers (B.tail x)
    where x0 = B.head x

hasTheStuffKnownToHaveNumbers :: ByteString -> Bool
hasTheStuffKnownToHaveNumbers x =
    if B.null x
    then False
    else if isUpperCase x0
        then True
        else hasTheStuffKnownToHaveNumbers (B.tail x)
    where x0 = B.head x

isUpperCase :: Word8 -> Bool 
isUpperCase x = x >= 65 && x <= 90
isNumber :: Word8 -> Bool 
isNumber x = x >= 48 && x <= 57

pazify :: (ByteString -> ByteString) -> (Int -> ByteString -> Bool) -> ByteString -> (Int, ByteString)
pazify = pazify' 1
pazify' :: Int -> (ByteString -> ByteString) -> (Int -> ByteString -> Bool) -> ByteString -> (Int, ByteString)
pazify' n calculate p x0 =
    if p n x then (n, x) else pazify' (n+1) calculate p x
        where
            x = calculate x0
