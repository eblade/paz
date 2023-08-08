module ConfigProvider where

import ConfigData (Config)
import ConfigParser (parseConfigMaybe)
import System.Directory (doesFileExist)

loadConfigMaybe :: String -> IO (Maybe Config)
loadConfigMaybe path = do
    fileExists <- doesFileExist path
    if fileExists
        then do
            text <- readFile path
            return $ parseConfigMaybe text
        else
            return Nothing

