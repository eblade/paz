module Main where

import Options.Applicative
import Paz (makeStart, pazify, check, calculate, finalize, appendRevision)
import ConfigData (Config, getSections, getSectionMaybe)
import ConfigProvider (loadConfigMaybe)
import Password (getPassword)
import Resolve (resolveInt, resolveMaybeInt, resolveMaybeString)
import System.Directory (getHomeDirectory)
import System.Exit
import Data.List (sort)

data CommandLineOptions = CommandLineOptions
    { maybeMaster :: Maybe String
    , maybeLength :: Maybe Int
    , maybeRevision :: Maybe Int
    , maybeMinIterations :: Maybe Int
    , maybeAddition :: Maybe String
    , maybeSite :: Maybe String
    } deriving (Show)

data CompleteOptions = CompleteOptions
    { master :: String
    , length_ :: Int
    , revision :: Maybe Int
    , minIterations :: Int
    , addition :: Maybe String
    , site :: String
    } deriving (Show)

defaults :: CompleteOptions
defaults = CompleteOptions
    { master = ""
    , length_ = 15
    , revision = Nothing
    , minIterations = 10
    , addition = Nothing
    , site = ""
    }

-- We need to "double-Just" this because the maybeReader "un-Justs" once
parseJust :: ReadM (Maybe Int)
parseJust = maybeReader $ ( Just . Just . read )

paz :: Parser CommandLineOptions
paz = CommandLineOptions
    <$> (optional $ strOption
        ( long "master"
       <> short 'm'
       <> metavar "PASSWORD"
       <> help "Your master password" ))
    <*> option parseJust
        ( long "length"
       <> short 'n'
       <> value Nothing
       <> showDefaultWith (\_ -> show $ length_ defaults)
       <> metavar "INT"
       <> help "Length of the generated password" )
    <*> (option parseJust
        ( long "revision"
       <> short 'r'
       <> value Nothing
       <> showDefaultWith (\_ -> show $ revision defaults)
       <> metavar "INT"
       <> help "Site revision (append this number to the site if specified)" ))
    <*> option parseJust
        ( long "min-iterations"
       <> short 'i'
       <> value Nothing
       <> showDefaultWith (\_ -> show $ minIterations defaults)
       <> metavar "INT"
       <> help "Minimum number of hash function passes" )
    <*> (optional $ strOption
        ( long "addition"
       <> short 'a'
       <> metavar "ADDITION"
       <> help "Append this string to the end of the generated password" ))
    <*> (optional $ (argument str)
        ( metavar "SITE"
       <> help "The name of the site" ))


main :: IO ()
main = execParser opts
    >>= completeOptions
    >>= computeResult
    >>= putStrLn
    where
        opts = info (paz <**> helper)
          ( fullDesc
         <> progDesc "Deterministically generate a password based on input parameters (read from args, ~/.pazrc.remote and ~/.pazrc in that given priority)"
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
            remoteSite <- getSectionMaybe' theSite remoteConfig
            defaultSite <- getSectionMaybe' "DEFAULT" localConfig
            return CompleteOptions
                { master = finalMaster
                , site = theSite
                , length_ = resolveInt "length" (length_ defaults) (maybeLength options) remoteSite localSite defaultSite
                , minIterations = resolveInt "min-iterations" (minIterations defaults) (maybeMinIterations options) remoteSite localSite defaultSite
                , revision = resolveMaybeInt "revision" (revision defaults) (maybeRevision options) remoteSite localSite defaultSite
                , addition = resolveMaybeString "addition" (addition defaults) (maybeAddition options) remoteSite localSite defaultSite
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
computeResult config = do
    return $ finalize (length_ config) (addition config) result
    where
        start = makeStart (master config) revisionedSite
        revisionedSite = appendRevision (revision config) (site config)
        (_, result) = pazify calculate (check (length_ config) (minIterations config)) start

