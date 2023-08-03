module Password where

import System.IO
import Control.Exception

data Password = Given String | Prompt

getPassword :: Maybe String -> IO String
getPassword maybeGiven = case maybeGiven of
        Nothing -> do 
            hPutStr stderr "Password: "
            result <- withoutEcho getLine
            hPutStrLn stderr "OK"
            return result
        Just given -> return given

withoutEcho :: IO a -> IO a
withoutEcho action =
    finally (hSetEcho stdin False >> action) (hSetEcho stdin True)
