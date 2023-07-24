{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import Options.Generic
import System.IO
import Paz (makeStart, pazify, check, calculate, cutToString)
import Data.Maybe (fromMaybe)

data Options = Options
    { mastera :: Maybe String
    , sitea   :: Maybe String
    , lengtha :: Maybe Int
    , miniterationsa :: Maybe Int
    } deriving (Generic, Show)

instance ParseRecord Options

data Configuration = Configuration {
    master :: String,
    site :: String,
    plength :: Int,
    miniterations :: Int }

main :: IO ()
main = getRecord "Paz"
    >>= completeOptions
    >>= computeResult
    >>= putStrLn

completeOptions :: Options -> IO Configuration
completeOptions options = return Configuration {
    site=fromMaybe "default" (sitea options),
    master=fromMaybe "master" (mastera options),
    plength=fromMaybe 15 (lengtha options),
    miniterations=fromMaybe 10 (miniterationsa options) }

computeResult :: Configuration -> IO String
computeResult config = return $ cutToString (plength config) result
    where start = makeStart (master config) (site config)
          calculate' = calculate (plength config)
          (iterations, result) = pazify calculate' (check (plength config) (miniterations config)) start

