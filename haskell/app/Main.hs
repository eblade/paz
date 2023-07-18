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
    hPutStrLn stderr (show (iterations) ++ " iterations")
        where
            config = Configuration {
                site="sites",
                master="master",
                plength=15,
                miniterations=1000000 }
            start = makeStart (master config) (site config)
            calculate = calculate' (plength config)
            (iterations, result) = pazify calculate (check (plength config) (miniterations config)) start

makeStart :: String -> String -> ByteString
makeStart master site = C.pack (master ++ ":" ++ site)

pazify :: (ByteString -> ByteString) -> (Int -> ByteString -> Bool) -> ByteString -> (Int, ByteString)
pazify = pazify' 1
pazify' :: Int -> (ByteString -> ByteString) -> (Int -> ByteString -> Bool) -> ByteString -> (Int, ByteString)
pazify' n f p xn_1
    | p n xn = (n, xn)
    | otherwise = pazify' (n+1) f p xn
    where
        xn = f xn_1

calculate' :: Int -> ByteString -> ByteString
calculate' length s = B.map translate (Base64.encode digest)
    where
        digest = SHA512.finalize ctx
        ctx = SHA512.update ctx0 s
        ctx0 = SHA512.init

translate :: Word8 -> Word8
translate 43 = 57 -- + -> 9
translate 47 = 56 -- / -> 8 
translate 61 = 65 -- = -> A
translate x = x

check :: Int -> Int -> Int -> ByteString -> Bool
check plength miniterations n x = enough && (startsWithLowerCase pw) && (hasTheStuff pw)
    where
        enough = (n >= miniterations)
        pw = B.take plength x

startsWithLowerCase :: ByteString -> Bool
startsWithLowerCase x = ((B.head x) >= 97) && ((B.head x) <= 122)

hasTheStuff :: ByteString -> Bool
hasTheStuff = hasTheStuff' False False
hasTheStuff' :: Bool -> Bool -> ByteString -> Bool
hasTheStuff' hasUpper hasNumber x
    | B.null x = False
    | hasNumber && isUpper = True
    | hasUpper && isNumber = True
    | otherwise = hasTheStuff' (hasUpper || isUpper) (hasNumber || isNumber) (B.tail x)
    where
        isUpper = wordIsUpper x0
        isNumber = wordIsNumber x0
        x0 = B.head x

wordIsUpper :: Word8 -> Bool 
wordIsUpper x = x >= 65 && x <= 90
wordIsNumber :: Word8 -> Bool 
wordIsNumber x = x >= 48 && x <= 57
