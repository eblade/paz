module Paz where

import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import Data.Word (Word8)
import qualified Data.ByteString.Base64 as Base64
import qualified Crypto.Hash.SHA512 as SHA512
import qualified Data.ByteString.Char8 as C

makeStart :: String -> String -> ByteString
makeStart master site = C.pack (master ++ ":" ++ site)

cutToString :: Int -> ByteString -> String
cutToString length = C.unpack . (B.take length)

pazify :: (ByteString -> ByteString) -> (Int -> ByteString -> Bool) -> ByteString -> (Int, ByteString)
pazify = pazify' 1
    where
        pazify' n f p xn_1
            | p n xn = (n, xn)
            | otherwise = pazify' (n+1) f p xn
            where
                xn = f xn_1

calculate :: Int -> ByteString -> ByteString
calculate length s = B.map translate (Base64.encode digest)
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
check plength miniterations n x = enough && (startsWithLowerCase pw) && (hasUpperAndNumber pw)
    where
        enough = (n >= miniterations)
        pw = B.take plength x
        startsWithLowerCase x = ((B.head x) >= 97) && ((B.head x) <= 122)
        hasUpperAndNumber = f False False
            where
            f hasUpper hasNumber xxs
                | B.null xxs = False
                | hasNumber && isUpper = True
                | hasUpper && isNumber = True
                | otherwise = f (hasUpper || isUpper) (hasNumber || isNumber) xs
                where
                    isUpper = x >= 65 && x <= 90
                    isNumber = x >= 48 && x <= 57
                    x = B.head xxs
                    xs = B.tail xxs
