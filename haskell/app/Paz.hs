module Paz where

import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import Data.Word (Word8)
import qualified Data.ByteString.Base64 as Base64
import qualified Crypto.Hash.SHA512 as SHA512
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString.Char8 as C

data Hash = SHA512 | SHA256 | MD5 deriving (Show, Eq)

makeStart :: String -> String -> ByteString
makeStart master site = C.pack (master ++ ":" ++ site)

finalize :: Int -> Maybe String -> ByteString -> String
finalize length_ maybeAddition bs =
    case maybeAddition of
        Just addition -> cut ++ addition
        Nothing -> cut
    where
        cut = cutToString length_ bs

cutToString :: Int -> ByteString -> String
cutToString length_ = C.unpack . (B.take length_)

pazify :: (ByteString -> ByteString) -> (Int -> ByteString -> Bool) -> ByteString -> (Int, ByteString)
pazify = pazify' 1
    where
        pazify' n f p xn_1
            | p n xn = (n, xn) -- if p(n, xn) is true, return (n, xn), we are done
            | otherwise = pazify' (n+1) f p xn
            where
                xn = B.map translate (Base64.encode xn_unencoded)
                xn_unencoded = f xn_1

                translate :: Word8 -> Word8
                translate 43 = 57 -- + -> 9
                translate 47 = 56 -- / -> 8 
                translate 61 = 65 -- = -> A
                translate x = x

calculate :: Hash -> (ByteString -> ByteString)
calculate hash =
    case hash of
        SHA512 -> calculateSHA512
        SHA256 -> calculateSHA256
        MD5 -> calculateMD5

calculateSHA512 :: ByteString -> ByteString
calculateSHA512 s = digest
    where
        digest = SHA512.finalize ctx
        ctx = SHA512.update ctx0 s
        ctx0 = SHA512.init

calculateSHA256 :: ByteString -> ByteString
calculateSHA256 s = digest
    where
        digest = SHA256.finalize ctx
        ctx = SHA256.update ctx0 s
        ctx0 = SHA256.init

calculateMD5 :: ByteString -> ByteString
calculateMD5 s = digest
    where
        digest = MD5.finalize ctx
        ctx = MD5.update ctx0 s
        ctx0 = MD5.init

check :: Int -> Int -> Int -> ByteString -> Bool
check plength miniterations n x = enough && (startsWithLowerCase pw) && (hasUpperAndNumber pw)
    where
        enough = (n >= miniterations)
        pw = B.take plength x
        startsWithLowerCase x' = ((B.head x') >= 97) && ((B.head x') <= 122)
        hasUpperAndNumber = f False False
            where
            f hasUpper hasNumber xxs
                | B.null xxs = False
                | hasNumber && isUpper = True
                | hasUpper && isNumber = True
                | otherwise = f (hasUpper || isUpper) (hasNumber || isNumber) xs
                where
                    isUpper = x' >= 65 && x' <= 90
                    isNumber = x' >= 48 && x' <= 57
                    x' = B.head xxs
                    xs = B.tail xxs

appendRevision :: Maybe Int -> String -> String
appendRevision maybeRevision site =
    case maybeRevision of
        Just revision -> site ++ (show revision)
        Nothing -> site
