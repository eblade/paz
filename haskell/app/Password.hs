module Password where

import System.IO
import Control.Exception

data Password = Given String | Prompt

getPassword :: Maybe String -> Maybe String -> String -> IO String
getPassword maybeGiven maybeUsername strategy =
    case maybeGiven of
        Nothing -> do 
            hPutStr stderr ("Password" ++ for ++ " (" ++ strategy ++ "): ")
            result <- withoutEcho getLine
            hPutStrLn stderr "OK"
            return result
        Just given -> return given
    where
        for = case maybeUsername of
            Just username -> " for " ++ username
            Nothing -> ""

withoutEcho :: IO a -> IO a
withoutEcho action =
    finally (hSetEcho stdin False >> action) (hSetEcho stdin True)
