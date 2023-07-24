module Config where

-- trying to follow https://lpraz.github.io/parsing/haskell/2020/12/19/haskell-readp-parse-ini-config.html

import Text.ParserCombinators.ReadP
import Data.Maybe (catMaybes)
import qualified Data.Map as Map

{-
eol parses either the end of a line (char \n) or the file (eof, from ReadP), and throws it away. restOfLine takes this a bit further, and reads from its input until it gets to the end of a line, using eol. Both of these will show up in a few other, “bigger” parsers.
-}

restOfLine :: ReadP String
restOfLine = manyTill get eol

eol :: ReadP ()
eol = choice
    [ char '\n' >> return ()
    , eof
    ]

iniKey :: ReadP (String, String)
iniKey = do
    name <- manyTill (satisfy (flip notElem "=;\n")) (char '=')
    value <- restOfLine
    return (name, value)

notChar :: Char -> ReadP Char
notChar c = satisfy (/= c)

iniSectionName :: ReadP String
iniSectionName = do
    char '['
    sectionName <- manyTill (notChar '\n') (char ']')
    eol
    return sectionName

iniIgnoreLine :: ReadP ()
iniIgnoreLine = choice [iniComment, iniBlankLine]

iniComment :: ReadP ()
iniComment = do
    char ';'
    restOfLine
    return ()

iniBlankLine :: ReadP ()
iniBlankLine = do
    manyTill (satisfy (flip elem " \t")) eol
    return ()

iniKeys :: ReadP (Map.Map String String)
iniKeys = do
    maybeKeys <- manyTill (choice
        [ iniKey >>= (return . Just)
        , iniIgnoreLine >> return Nothing
        ])
        lookEndOfSection
    let keys = catMaybes maybeKeys
    return $ Map.fromList keys

lookEndOfSection :: ReadP ()
lookEndOfSection = do
    rest <- look
    if (rest == []) || (head rest == '[') then return () else pfail

iniSection :: ReadP (String, Map.Map String String)
iniSection = do
    sectionName <- iniSectionName
    keys <- iniKeys
    return (sectionName, keys)

data Config = Config
    { keys :: Map.Map String String
    , sections :: Map.Map String (Map.Map String String)
    }

ini :: ReadP Config
ini = do
    keys <- iniKeys
    sections <- manyTill iniSection eof
    return $ Config keys (Map.fromList sections)

parseConfigMaybe :: String -> Maybe Config
parseConfigMaybe s =
    parseMaybe ini s

parseMaybe :: ReadP a -> String -> Maybe a
parseMaybe parser input =
    case readP_to_S parser input of
        [] -> Nothing
        ((result, _):_) -> Just result
