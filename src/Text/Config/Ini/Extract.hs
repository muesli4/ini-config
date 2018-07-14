module Text.Config.Ini.Extract where

import Text.Config.Ini.Common

lookupKey :: String -> String -> Config -> [String]
lookupKey s k = concatMap (lookupAll k) . lookupAll s . sections

lookupFstKey :: String -> String -> Config -> Maybe String
lookupFstKey s k c = lookup s (sections c) >>= lookup k

lookupKeyG :: String -> Config -> [String]
lookupKeyG k = lookupAll k . globalSection

lookupFstKeyG :: String -> Config -> Maybe String
lookupFstKeyG k = lookup k . globalSection

lookupAll :: Eq k => k -> [(k, v)] -> [v]
lookupAll k = fmap snd . filter ((== k) . fst)
