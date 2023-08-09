module Resolve where

import qualified Data.Map as Map

type MaybeMap = Maybe (Map.Map String String)
type Getter a = (String -> MaybeMap -> Maybe a)

unit :: a -> a
unit x = x

resolve :: Getter a -> String -> a -> Maybe a -> MaybeMap -> MaybeMap -> a
resolve get name defaultValue maybeArg site defaults =
    case maybeArg of
        Just arg -> arg
        Nothing -> case maybeSiteValue of
            Just siteValue -> siteValue
            Nothing -> case maybeDefaultsValue of
                Just defaultsValue -> defaultsValue
                Nothing -> defaultValue
    where
        maybeSiteValue = get' site
        maybeDefaultsValue = get' defaults
        get' = get name

resolveInt :: String -> Int -> Maybe Int -> MaybeMap -> MaybeMap -> Int
resolveInt = resolve getIntFromMap

resolveString :: String -> String -> Maybe String -> MaybeMap -> MaybeMap -> String
resolveString = resolve getStringFromMap

resolveMaybe :: Getter a -> String -> Maybe a -> Maybe a -> MaybeMap -> MaybeMap -> Maybe a
resolveMaybe get name defaultValue maybeArg site defaults =
    case maybeArg of
        Just arg -> Just arg
        Nothing -> case maybeSiteValue of
            Just siteValue -> Just siteValue
            Nothing -> case maybeDefaultsValue of
                Just defaultsValue -> Just defaultsValue
                Nothing -> defaultValue
    where
        maybeSiteValue = get' site
        maybeDefaultsValue = get' defaults
        get' = get name

resolveMaybeInt :: String -> Maybe Int -> Maybe Int -> MaybeMap-> MaybeMap -> Maybe Int
resolveMaybeInt = resolveMaybe getIntFromMap

resolveMaybeString :: String -> Maybe String -> Maybe String -> MaybeMap-> MaybeMap -> Maybe String
resolveMaybeString = resolveMaybe getStringFromMap

getFromMap :: (String -> a) -> String -> MaybeMap -> Maybe a
getFromMap convert name maybeMap =
    case maybeMap of
        Just map_ -> case (Map.lookup name map_) of
            Just s -> Just $ convert s
            Nothing -> Nothing
        Nothing -> Nothing

getIntFromMap :: String -> MaybeMap -> Maybe Int
getIntFromMap = getFromMap read

getStringFromMap :: String -> MaybeMap -> Maybe String
getStringFromMap = getFromMap unit

