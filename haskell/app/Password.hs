module Password where

import System.IO
import Control.Exception

data Password = Given String | Prompt

getPassword :: String -> IO String
getPassword x = case p of
        Prompt -> do 
            hPutStr stderr "Password: "
            withoutEcho getLine
        Given s -> return s
        where
            p = if x == "-"
              then Prompt
              else Given x

withoutEcho :: IO a -> IO a
withoutEcho action =
    finally (hSetEcho stdin False >> action) (hSetEcho stdin True)
