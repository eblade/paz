module Password where

import System.IO
import Control.Exception

data Password = Given String | Prompt

getPassword :: Maybe String -> IO String
getPassword x = case x of
        Nothing -> do 
            hPutStr stderr "Password: "
            withoutEcho getLine
        Just x -> return x

withoutEcho :: IO a -> IO a
withoutEcho action =
    finally (hSetEcho stdin False >> action) (hSetEcho stdin True)
