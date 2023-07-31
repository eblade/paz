module ConfigData where

import qualified Data.Map as Map

data Config = Config
    { keys :: Map.Map String String
    , sections :: Map.Map String (Map.Map String String)
    } deriving (Show)

surely c = case c of Just x -> x

getSectionMaybe :: String -> Config -> Maybe (Map.Map String String)
getSectionMaybe sectionName config =
    Map.lookup sectionName (sections config)

getValueMaybe :: String -> Map.Map String String -> Maybe String
getValueMaybe key section =
    Map.lookup key section

getSections :: Config -> [String]
getSections = Map.keys . sections

