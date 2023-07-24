module Main where

import Options.Applicative
import System.IO
import Paz (makeStart, pazify, check, calculate, cutToString)
import Password (getPassword, toPassword)
import Data.Maybe (fromMaybe)

data Paz = Paz {
    master :: String,
    site :: String,
    length_ :: Int,
    miniterations :: Int }

paz :: Parser Paz
paz = Paz
    <$> strOption
        ( long "master"
       <> short 'm'
       <> value "-"
       <> metavar "PASSWORD"
       <> help "Your master password" )
    <*> strOption
        ( long "site"
       <> short 's'
       <> metavar "SITE"
       <> help "The name of the site" )
    <*> option auto
        ( long "length"
       <> short 'n'
       <> showDefault
       <> value 15
       <> metavar "INT"
       <> help "Length of the generated password" )
    <*> option auto
        ( long "min-iterations"
       <> short 'i'
       <> showDefault
       <> value 10
       <> metavar "INT"
       <> help "Minimum number of hash function passes" )


main :: IO ()
main = execParser opts
    >>= completeOptions
    >>= computeResult
    >>= putStrLn
    where
        opts = info (paz <**> helper)
          ( fullDesc
         <> progDesc "Deterministically generates a password based on input parameters"
         <> header "paz - an SGP-based password generator" )

completeOptions :: Paz -> IO Paz
completeOptions options = do
    newMaster <- getPassword $ toPassword (master options)
    return options { master = newMaster }

computeResult :: Paz -> IO String
computeResult config = return $ cutToString (length_ config) result
    where start = makeStart (master config) (site config)
          calculate' = calculate (length_ config)
          (iterations, result) = pazify calculate' (check (length_ config) (miniterations config)) start

