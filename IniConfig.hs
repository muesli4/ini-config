module IniConfig
    ( Config (Config)
    , parseConfig
    , formatConfig
    , readConfigFile
    , writeConfigFile
    ) where

import Control.Arrow
import Data.Function
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Ord
import Safe

type SAssocList a = [(String, a)]

data Config = Config
            { defEntries :: SAssocList String
            , sections   :: SAssocList (SAssocList String)
            } deriving Show

instance Monoid Config where
    mempty = Config [] []
    mappend (Config defL sL) (Config defR sR)
           = Config (defL ++ defR) $ map (fst . head &&& concatMap snd) $ groupBy ((==) `on` fst) $ merge sL sR
      where
        merge = foldr $ insertBy $ comparing fst

-- | Parse a configuration from a 'String'.
parseConfig :: String -> Either String Config
parseConfig s = case parseSections s of
    (def, r) -> Config <$> parseSectionOptions def <*> mapM parseSection r

parseSection :: [String] -> Either String (String, SAssocList String)
parseSection ls = case ls of
    header : cLines -> (,) <$> parseSectionTitle header <*> parseSectionOptions cLines
    _               -> Left "not a section"

parseSectionOptions :: [String] -> Either String (SAssocList String)
parseSectionOptions = mapM parseOption

parseOption :: String -> Either String (String, String)
parseOption s = case break (== '=') s of
    (name, '=' : value) -> Right (trim name, trim value)
    _                   -> Left "expected a key-value pair seperated by '='"
  where
    trim      = reverse . trimFront . reverse . trimFront
    trimFront = dropWhile (== ' ')

parseSectionTitle :: String -> Either String String
parseSectionTitle l = case break (== ']') $ drop 1 l of
    (title, "]") -> Right title
    _            -> Left "expected ']' to end section header"

parseSections :: String -> ([String], [[String]])
parseSections s =
    fmap (chop splitSection) $ span notSection
                             $ filter isCommentOrEmpty
                             $ lines s
  where
    isCommentOrEmpty      = (||) <$> not . null <*> (maybe False (`notElem` ";#") . listToMaybe)
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

