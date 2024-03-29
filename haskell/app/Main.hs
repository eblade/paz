module Main where

import System.IO (hPutStrLn, stderr)
import Options.Applicative
import Control.Monad (when)
import Paz (makeStart, pazify, check, calculate, finalize, appendRevision)
import qualified Paz
import qualified Bishop
import ConfigData (Config, getSections, getSectionMaybe)
import ConfigProvider (loadConfigMaybe)
import Password (getPassword)
import Resolve (resolveInt, resolveMaybeInt, resolveString, resolveMaybeString)
import System.Directory (getHomeDirectory)
import System.Exit
import Data.List (sort)
import Data.Char (toUpper)
import qualified Data.ByteString.Char8 as C

data CommandLineOptions = CommandLineOptions
    { maybeMaster :: Maybe String
    , maybeLength :: Maybe Int
    , maybeRevision :: Maybe Int
    , maybeMinIterations :: Maybe Int
    , maybeAddition :: Maybe String
    , maybeHash :: Maybe String
    , surelyLinebreak :: Bool
    , surelyBishop :: Bool
    , surelyVerbose :: Bool
    , maybeSite :: Maybe String
    } deriving (Show)

data CompleteOptions = CompleteOptions
    { master :: String
    , length_ :: Int
    , revision :: Maybe Int
    , minIterations :: Int
    , addition :: Maybe String
    , hash :: Paz.Hash
    , username :: Maybe String
    , strategy :: String
    , linebreak :: Bool
    , bishop :: Bool
    , verbose :: Bool
    , site :: String
    } deriving (Show)

defaults :: CompleteOptions
defaults = CompleteOptions
    { master = ""
    , length_ = 15
    , revision = Nothing
    , minIterations = 10
    , addition = Nothing
    , hash = Paz.SHA512
    , username = Nothing
    , strategy = "default"
    , linebreak = False
    , bishop = False
    , verbose = False
    , site = ""
    }

-- We need to "double-Just" this because the maybeReader "un-Justs" once
parseJustInt :: ReadM (Maybe Int)
parseJustInt = maybeReader $ ( Just . Just . read )

parseHash :: Maybe String -> Paz.Hash
parseHash maybeHashName =
    case maybeHashName of
        Just hashName -> case (map toUpper hashName) of
            "SHA512" -> Paz.SHA512
            "SHA256" -> Paz.SHA256
            "MD5" -> Paz.MD5
            _ -> error $ "unknown hash function: " ++ hashName
        Nothing -> hash defaults

paz :: Parser CommandLineOptions
paz = CommandLineOptions
    <$> ( optional $ strOption
        ( long "master"
       <> short 'm'
       <> metavar "PASSWORD"
       <> help "Your master password" ))
    <*> option parseJustInt
        ( long "length"
       <> short 'n'
       <> value Nothing
       <> showDefaultWith (\_ -> show $ length_ defaults)
       <> metavar "LENGTH"
       <> help "Length of the generated password" )
    <*> ( option parseJustInt
        ( long "revision"
       <> short 'r'
       <> value Nothing
       <> showDefaultWith (\_ -> show $ revision defaults)
       <> metavar "REVISION"
       <> help "Site revision (append this number to the site if specified)" ))
    <*> option parseJustInt
        ( long "min-iterations"
       <> short 'i'
       <> value Nothing
       <> showDefaultWith (\_ -> show $ minIterations defaults)
       <> metavar "MIN_ITERATIONS"
       <> help "Minimum number of hash function passes" )
    <*> ( optional $ strOption
        ( long "addition"
       <> short 'a'
       <> showDefaultWith (\_ -> show $ addition defaults)
       <> metavar "ADDITION"
       <> help "Append this string to the end of the generated password" ))
    <*> ( optional $ strOption
        ( long "hash"
       <> short 'H'
       <> showDefaultWith (\_ -> show $ hash defaults)
       <> metavar "HASH"
       <> help "Choose what hash funtion to use (sha512, sha256 or md5)" ))
    <*> switch
        ( long "linebreak"
       <> short 'l'
       <> help "Print new line character after the password on stdout" )
    <*> switch
        ( long "bishop"
       <> short 'b'
       <> help "Print a drunken bishop pattern of the master password to stderr" )
    <*> switch
        ( long "verbose"
       <> short 'v'
       <> help "Print all of the resulting options to stderr" )
    <*> (optional $ (argument str)
        ( metavar "SITE"
       <> help "The name of the site" ))


main :: IO ()
main = execParser opts
    >>= completeOptions
    >>= computeResult
    >>= printResult
    where
        opts = info (paz <**> helper)
          ( fullDesc
         <> progDesc "Deterministically generate a password based on input parameters (read from args and then ~/.pazrc)"
         <> header "paz - an SGP-based password generator" )

completeOptions :: CommandLineOptions -> IO CompleteOptions
completeOptions options = do
    homeDirectory <- getHomeDirectory
    config <- loadConfigMaybe $ homeDirectory ++ "/.pazrc"
    case (maybeSite options) of
        Nothing -> printSitesAndExit config
        Just theSite -> do
            configSite <- pure $ getSectionMaybe theSite config
            defaultSite <- pure $ getSectionMaybe "DEFAULT" config
            allButMaster <- return CompleteOptions
                { master = ""
                , site = theSite
                , length_ = resolveInt "length" (length_ defaults)
                    (maybeLength options) configSite defaultSite
                , minIterations = resolveInt "min-iterations" (minIterations defaults)
                    (maybeMinIterations options) configSite defaultSite
                , revision = resolveMaybeInt "revision" (revision defaults)
                    (maybeRevision options) configSite defaultSite
                , addition = resolveMaybeString "addition" (addition defaults)
                    (maybeAddition options) configSite defaultSite
                , hash = parseHash $ resolveMaybeString "hash" (Just $ show $ hash defaults)
                    (maybeHash options) configSite defaultSite
                , username = resolveMaybeString "username" (username defaults)
                    Nothing configSite defaultSite
                , strategy = resolveString "strategy" (strategy defaults)
                    Nothing configSite defaultSite
                , linebreak = surelyLinebreak options
                , bishop = surelyBishop options
                , verbose = surelyVerbose options
                }
            _ <- when (verbose allButMaster) $ printConfig allButMaster
            finalMaster <- getPassword (maybeMaster options)
                                       (username allButMaster)
                                       (strategy allButMaster)
            _ <- when (bishop allButMaster)
                $ Bishop.printGraph stderr $ Bishop.drunkenWalk $ (calculate Paz.SHA512) $ C.pack
                $ finalMaster ++ ['\n']
            return allButMaster { master = finalMaster }

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

computeResult :: CompleteOptions -> IO (CompleteOptions, String)
computeResult config = do
    return (config, ( finalize (length_ config) (addition config) result ) )
    where
        (_, result) = pazify calculate' check' start
        start = makeStart (master config) revisionedSite
        revisionedSite = appendRevision (revision config) (site config)
        calculate' = calculate $ hash config
        check' = check (length_ config) (minIterations config)

printConfig :: CompleteOptions -> IO ()
printConfig config = do
    put $ "site = " ++ (site config)
    put $ "hash = " ++ (show $ hash config)
    put $ "length = " ++ (show $ length_ config)
    put $ "min-iterations = " ++ (show $ minIterations config)
    put $ "revision = " ++ (case (revision config) of
        Just r -> (show r)
        Nothing -> "Nothing")
    put $ "addition = " ++ (case (addition config) of
        Just a -> a
        Nothing -> "Nothing")
    put $ "linebreak = " ++ (show $ linebreak config)
    put $ "username = " ++ (case (username config) of
        Just u -> u
        Nothing -> "Nothing")
    put $ "strategy = " ++ (strategy config)
    return ()
    where
        put = hPutStrLn stderr 
 
printResult :: (CompleteOptions, String) -> IO ()
printResult (config, result) = do
    if (linebreak config)
        then putStrLn result
        else putStr result
