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
parseConfig :: String -> Maybe Config
parseConfig s = case parseSections s of
    []      -> pure $ Config [] []
    def : r -> Config <$> parseSectionOptions def <*> mapM parseSection r

parseSection :: [String] -> Maybe (String, SAssocList String)
parseSection ls = case ls of
    header : cLines -> (,) (parseSectionTitle header) <$> parseSectionOptions cLines
    _               -> Nothing

parseSectionOptions :: [String] -> Maybe (SAssocList String)
parseSectionOptions = mapM parseOption

parseOption :: String -> Maybe (String, String)
parseOption s = case break (== '=') s of
    (name, '=' : value) -> Just (trim name, trim value)
    _                   -> Nothing
  where
    trim      = reverse . trimFront . reverse . trimFront
    trimFront = dropWhile (== ' ')

parseSectionTitle :: String -> String
parseSectionTitle = init . drop 1

parseSections :: String -> [[String]]
parseSections s = case span notSection $ filter ((||) <$> not . null <*> (maybe False (`notElem` ";#") . listToMaybe)) $ lines s of
        (defs, r) -> defs : chop splitSection r
  where
    splitSection = takeWhile1 notSection &&& dropWhile notSection . drop 1
    notSection   = (/= '[') . head

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
readConfigFile :: FilePath -> IO (Maybe Config)
readConfigFile = fmap parseConfig . readFile

-- | Write a configuration to a file.
writeConfigFile :: FilePath -> Config -> IO ()
writeConfigFile fp c = writeFile fp $ formatConfig c

