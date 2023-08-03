module Resolve where

import qualified Data.Map as Map

type MaybeStringMap = Maybe (Map.Map String String)

resolve :: (String -> MaybeStringMap -> Maybe a) -> String -> a -> Maybe a -> MaybeStringMap -> MaybeStringMap -> MaybeStringMap -> a
resolve getFromMap name defaultValue maybeArg remote local defaults =
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
        maybeLocalValue = getFromMap' local
        maybeRemoteValue = getFromMap' remote
        maybeDefaultsValue = getFromMap' defaults
        getFromMap' = getFromMap name

resolveInt :: String -> Int -> Maybe Int -> MaybeStringMap -> MaybeStringMap-> MaybeStringMap -> Int
resolveInt = resolve getIntFromMap

resolveString :: String -> String -> Maybe String -> MaybeStringMap -> MaybeStringMap -> MaybeStringMap -> String
resolveString = resolve getStringFromMap

resolveMaybe :: (String -> MaybeStringMap -> Maybe a) -> String -> Maybe a -> Maybe a -> MaybeStringMap -> MaybeStringMap -> MaybeStringMap -> Maybe a
resolveMaybe getFromMap name defaultValue maybeArg remote local defaults =
    case maybeArg of
        Just arg -> Just arg
        Nothing -> case maybeLocalValue of
            Just localValue -> Just localValue
            Nothing -> case maybeRemoteValue of 
                Just remoteValue -> Just remoteValue
                Nothing -> case maybeDefaultsValue of
                    Just defaultsValue -> Just defaultsValue
                    Nothing -> defaultValue
    where
        maybeLocalValue = getFromMap' local
        maybeRemoteValue = getFromMap' remote
        maybeDefaultsValue = getFromMap' defaults
        getFromMap' = getFromMap name

resolveMaybeInt :: String -> Maybe Int -> Maybe Int -> MaybeStringMap -> MaybeStringMap-> MaybeStringMap -> Maybe Int
resolveMaybeInt = resolveMaybe getIntFromMap

resolveMaybeString :: String -> Maybe String -> Maybe String -> MaybeStringMap -> MaybeStringMap-> MaybeStringMap -> Maybe String
resolveMaybeString = resolveMaybe getStringFromMap

getIntFromMap :: String -> MaybeStringMap -> Maybe Int
getIntFromMap name maybeMap =
    case maybeMap of
        Just map_ -> case (Map.lookup name map_) of
            Just s -> Just $ read s
            Nothing -> Nothing
        Nothing -> Nothing

getStringFromMap :: String -> MaybeStringMap -> Maybe String
getStringFromMap name maybeMap =
    case maybeMap of
        Just map_ -> Map.lookup name map_
        Nothing -> Nothing

