module Main where

import System.IO
import qualified Data.ByteString
import Data.ByteString (ByteString)
import Data.Word (Word8)
import qualified Data.ByteString.Base64 as Base64
import qualified Crypto.Hash.SHA512 as SHA512
import qualified Data.ByteString.Char8 as C

main :: IO ()
main = do
    hSetBuffering stdin LineBuffering
    putStrLn (C.unpack result)
    putStrLn (show iterations)
        where
            start = makeStart "master" "site"
            calculate = calculate' 15
            (iterations, result) = pazify calculate check start

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
calculate' length s = Data.ByteString.map translate (Base64.encode digest)
    where
        digest = SHA512.finalize ctx
        ctx = SHA512.update ctx0 s
        ctx0 = SHA512.init

check :: Int -> ByteString -> Bool
check n x = (n > 10) && (startsWithLowerCase x)

startsWithLowerCase :: ByteString -> Bool
startsWithLowerCase x = ((Data.ByteString.head x) >= 97) && ((Data.ByteString.head x) <= 122)

pazify :: (ByteString -> ByteString) -> (Int -> ByteString -> Bool) -> ByteString -> (Int, ByteString)
pazify = pazify' 0
pazify' :: Int -> (ByteString -> ByteString) -> (Int -> ByteString -> Bool) -> ByteString -> (Int, ByteString)
pazify' n calculate p x0 =
    if p n x then (n, x) else pazify' (n+1) calculate p x
        where
            x = calculate x0
