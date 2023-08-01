module Resolve where

import qualified Data.Map as Map

resolveInt :: String -> Int -> Maybe Int -> Maybe (Map.Map String String) -> Maybe (Map.Map String String)-> Maybe (Map.Map String String) -> Int
resolveInt name defaultValue maybeArg remote local defaults =
    case maybeArg of
        Just arg -> arg
        Nothing -> case maybeLocalValue of
            Just localValue -> localValue
            Nothing -> case maybeRemoteValue of 
                Just remoteValue -> remoteValue
                Nothing -> case maybeDefaultsValue of
                    Just defaultsValue -> defaultsValue
                    Nothing -> defaultValue
    where
        maybeLocalValue = getIntFromMap' local
        maybeRemoteValue = getIntFromMap' remote
        maybeDefaultsValue = getIntFromMap' defaults
        getIntFromMap' = getIntFromMap name

getIntFromMap :: String -> Maybe (Map.Map String String) -> Maybe Int
getIntFromMap name maybeMap =
    case maybeMap of
        Just map_ -> case (Map.lookup name map_) of
            Just s -> Just $ read s
            Nothing -> Nothing
        Nothing -> Nothing
