-- | Core interface for dealing with 'Config'.
module Text.Config.Ini
    ( Config (..)
    , parseConfig
    , formatConfig
    , readConfigFile
    , writeConfigFile
    , module Text.Config.Ini.Extract
    , module Text.Config.Ini.Builder
    ) where

import Data.Bifunctor
import Control.Arrow hiding (second)
import Data.List
import Data.List.Split
import Data.Maybe
import Safe

import Text.Config.Ini.Common
import Text.Config.Ini.Extract
import Text.Config.Ini.Builder

-- | Parse a configuration from a 'String'.
--
-- >>> parseConfig "foo = 42\n[bar]\nbaz = qux"
-- Right (Config {cGlobalSection = [("foo","42")], cSections = [("bar",[("baz","qux")])]})
parseConfig :: String -> Either String Config
parseConfig s = case splitSections s of
    (def, r) -> Config <$> parseSectionOptions def <*> mapM parseSection r

parseSection :: [String] -> Either String (String, SAssocList String)
parseSection ls = case ls of
    header : cLines -> (,) <$> parseSectionTitle header <*> parseSectionOptions cLines
    []              -> Left "not a section"

parseSectionOptions :: [String] -> Either String (SAssocList String)
parseSectionOptions = mapM parseOption

parseOption :: String -> Either String (String, String)
parseOption s = case break (== '=') s of
    (name, '=' : value) -> Right (trim name, trim value)
    _                   -> Left "expected a key-value pair seperated by '='"
  where
    trim      = reverse . trimFront . reverse . trimFront
    trimFront = dropWhile (== ' ')

-- Assumes that the 'String' starts with an opening bracket, as it is guaranteed
-- from 'splitSections'.
parseSectionTitle :: String -> Either String String
parseSectionTitle l = case break (== ']') $ drop 1 l of
    (title, "]") -> Right title
    _            -> Left "expected ']' to end section header"

-- | Splits off the global section lines with 'splitSection' and then repeatedly
-- does the same for the remaining sections.
splitSections :: String -> ([String], [[String]])
splitSections s =
    second (chop splitSection) $ span notSection
                               $ filter isRelevantLine
                               $ lines s
  where
    isRelevantLine        = maybe False (`notElem` ";#") . listToMaybe
    splitSection          = takeWhile1 notSection &&& dropWhile notSection . drop 1
    notSection            = (/= '[') . head

    -- | Ignore the first element, but add it anyway.
    takeWhile1 p (x : xs) = x : takeWhile p xs
    takeWhile1 _ []       = []

-- | Format a configuration into a 'String'.
formatConfig :: Config -> String
formatConfig (Config ds ss) = unlines $ intersperse "" $ formatEntries ds ++ map formatSection ss
  where
    formatSection (title, es) = unlines $ ('[' : title ++ "]") : formatEntries es
    formatEntries             = map formatEntry
    formatEntry (name, value) = name ++ " = " ++ value

-- | Read a configuration from a file.
readConfigFile :: FilePath -> IO (Either String Config)
readConfigFile = fmap parseConfig . readFile

-- | Write a configuration to a file.
writeConfigFile :: FilePath -> Config -> IO ()
writeConfigFile fp c = writeFile fp $ formatConfig c

