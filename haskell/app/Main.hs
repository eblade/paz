module Main where

import Options.Applicative
import Control.Exception
import System.IO
import Paz (makeStart, pazify, check, calculate, cutToString)
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

data Password = Given String | Prompt

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

toPassword :: String -> Password
toPassword s = if s == "-"
    then Prompt
    else Given s

getPassword :: Password -> IO String
getPassword p = case p of
        Prompt -> do 
            hPutStr stderr "Password: "
            withoutEcho getLine
        Given s -> return s

withoutEcho :: IO a -> IO a
withoutEcho action =
    finally (hSetEcho stdin False >> action) (hSetEcho stdin True)

computeResult :: Paz -> IO String
computeResult config = return $ cutToString (length_ config) result
    where start = makeStart (master config) (site config)
          calculate' = calculate (length_ config)
          (iterations, result) = pazify calculate' (check (length_ config) (miniterations config)) start

