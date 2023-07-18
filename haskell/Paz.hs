module Main where

import System.IO
import qualified Data.ByteString
import qualified Data.ByteString.Base64 as Base64
import qualified Crypto.Hash.SHA512 as SHA512
import qualified Data.ByteString.Lazy.Char8 as C

main = do
    hSetBuffering stdin LineBuffering
    putStrLn C.unpack (calculate (makeStart "master" "site") 15)


makeStart :: String String => ByteString
makeStart master site = Data.ByteString.pack (master ++ ":" ++ site)

calculate :: ByteString Int => ByteString
calculate start length = Base64.encode digest
    where
        digest = SHA512.finalize ctx
        ctx = SHA512.update ctx0 start
        ctx0 = SHA512.update
