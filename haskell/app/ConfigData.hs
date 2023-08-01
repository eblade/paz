module ConfigData where

import qualified Data.Map as Map

data Config = Config
    { keys :: Map.Map String String
    , sections :: Map.Map String (Map.Map String String)
    } deriving (Show)

surely :: Maybe a -> a
surely c = case c of
    Just x -> x
    Nothing -> error "Not so sure!"

getSectionMaybe :: String -> Maybe Config -> Maybe (Map.Map String String)
getSectionMaybe sectionName maybeConfig =
    case maybeConfig of
        Just config -> Map.lookup sectionName (sections config)
        Nothing -> Nothing

getValueMaybe :: String -> Map.Map String String -> Maybe String
getValueMaybe key section =
    Map.lookup key section

getSections :: Config -> [String]
getSections = Map.keys . sections

