module Text.Config.Ini.Common where

import Control.Arrow
import Data.Function
import Data.List
import Data.Ord

type SAssocList a = [(String, a)]

-- | An entry in a config may appear multiple times and appears in the order in
-- which it was defined in the config file.
data Config = Config
            { defEntries :: SAssocList String
            , sections   :: SAssocList (SAssocList String)
            } deriving Show

instance Monoid Config where
    mempty = Config [] []
    mappend (Config defL sL) (Config defR sR)
           = Config (defL ++ defR) $ map (fst . head &&& concatMap snd) $ groupBy ((==) `on` fst) $ merge sL sR
      where
        merge l = foldr (insertBy cmpFst) l . sortBy cmpFst
        cmpFst  = comparing fst
