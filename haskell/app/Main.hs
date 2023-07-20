module Main where

import System.IO
import Paz (makeStart, pazify, check, calculate, cutToString)

data Configuration = Configuration {
    master :: String,
    site :: String,
    plength :: Int,
    miniterations :: Int }

main :: IO ()
main = do
    hSetBuffering stdin LineBuffering
    putStrLn $ cutToString (plength config) result
    hPutStrLn stderr (show (iterations) ++ " iterations")
        where
            config = Configuration {
                site="sites",
                master="master",
                plength=15,
                miniterations=10 }
            start = makeStart (master config) (site config)
            calculate' = calculate (plength config)
            (iterations, result) = pazify calculate' (check (plength config) (miniterations config)) start

