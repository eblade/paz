module Main where

import Options.Applicative
import Paz (makeStart, pazify, check, calculate, cutToString)
import ConfigData (Config, getSections)
import ConfigProvider (loadConfigMaybe)
import Password (getPassword)
import System.Directory (getHomeDirectory)
import System.Exit
import Data.List (sort)

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
       <> value "-"
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
    homeDirectory <- getHomeDirectory
    -- localConfig <- loadConfigMaybe $ homeDirectory ++ "/.pazrc"
    remoteConfig <- loadConfigMaybe $ homeDirectory ++ "/.pazrc.remote"
    if ((site options) == "-")
        then printSitesAndExit remoteConfig
        else do
            newMaster <- getPassword $ master options
            return options { master = newMaster }

printSitesAndExit :: Maybe Config -> IO Paz
printSitesAndExit config = do
    _ <- mapM putStrLn $ sortedSections
    exitSuccess
    where
        sortedSections = sort sectionsExceptDefault
        sectionsExceptDefault = filter (\x -> x /= "DEFAULT") sections
        sections = case config of
            Nothing -> []
            Just c -> getSections c

computeResult :: Paz -> IO String
computeResult config = return $ cutToString (length_ config) result
    where
        start = makeStart (master config) (site config)
        calculate' = calculate (length_ config)
        (_, result) = pazify calculate' (check (length_ config) (miniterations config)) start

