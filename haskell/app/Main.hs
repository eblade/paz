module Main where

import Options.Applicative
import Paz (makeStart, pazify, check, calculate, cutToString)
import ConfigData (Config, getSections, getSectionMaybe, surely)
import ConfigProvider (loadConfigMaybe)
import Password (getPassword)
import Resolve (resolveInt)
import System.Directory (getHomeDirectory)
import System.Exit
import Data.List (sort)
import Data.Maybe (fromMaybe, isNothing)

data CommandLineOptions = CommandLineOptions
    { maybeMaster :: Maybe String
    , maybeSite :: Maybe String
    , maybeLength :: Maybe Int
    , maybeMinIterations :: Maybe Int
    }

data CompleteOptions = CompleteOptions
    { master :: String
    , site :: String
    , length_ :: Int
    , minIterations :: Int
    }

defaults = CompleteOptions
    { master = ""
    , site = ""
    , length_ = 15
    , minIterations = 10
    }

paz :: Parser CommandLineOptions
paz = CommandLineOptions
    <$> (optional $ strOption
        ( long "master"
       <> short 'm'
       <> metavar "PASSWORD"
       <> help "Your master password" ))
    <*> (optional $ (argument str)
        ( metavar "SITE"
       <> help "The name of the site" ))
    <*> option auto
        ( long "length"
       <> short 'n'
       <> value Nothing
       <> showDefaultWith (\_ -> show $ length_ defaults)
       <> metavar "INT"
       <> help "Length of the generated password" )
    <*> option auto
        ( long "min-iterations"
       <> short 'i'
       <> value Nothing
       <> showDefaultWith (\_ -> show $ minIterations defaults)
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

completeOptions :: CommandLineOptions -> IO CompleteOptions
completeOptions options = do
    homeDirectory <- getHomeDirectory
    localConfig <- loadConfigMaybe $ homeDirectory ++ "/.pazrc"
    remoteConfig <- loadConfigMaybe $ homeDirectory ++ "/.pazrc.remote"
    case (maybeSite options) of
        Nothing -> printSitesAndExit remoteConfig
        Just theSite -> do
            finalMaster <- getPassword $ maybeMaster options
            localSite <- getSectionMaybe' theSite localConfig
            print localSite
            remoteSite <- getSectionMaybe' theSite remoteConfig
            print remoteSite
            defaultSite <- getSectionMaybe' "DEFAULT" localConfig
            print defaultSite
            return CompleteOptions
                { master = finalMaster
                , site = theSite
                , length_ = resolveInt "length" (length_ defaults) (maybeLength options) remoteSite localSite defaultSite
                , minIterations = resolveInt "min-iterations" (minIterations defaults) (maybeMinIterations options) remoteSite localSite defaultSite
                }
    where
        -- wrapper for returning IO (cannot use fmap because two args)
        getSectionMaybe' name maybeConfig = do
            return (getSectionMaybe name maybeConfig)

printSitesAndExit :: Maybe Config -> IO CompleteOptions
printSitesAndExit config = do
    _ <- mapM putStrLn $ sortedSections
    exitSuccess
    where
        sortedSections = sort sectionsExceptDefault
        sectionsExceptDefault = filter (\x -> x /= "DEFAULT") sections
        sections = case config of
            Nothing -> []
            Just c -> getSections c

computeResult :: CompleteOptions -> IO String
computeResult config = return $ cutToString (length_ config) result
    where
        start = makeStart (master config) (site config)
        calculate' = calculate (length_ config)
        (_, result) = pazify calculate' (check (length_ config) (minIterations config)) start

