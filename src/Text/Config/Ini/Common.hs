module Text.Config.Ini.Common where

import Control.Arrow
import Data.Function
import Data.List
import Data.Ord

type SAssocList a = [(String, a)]

-- | An entry in a config may appear multiple times and appears in the order in
-- which it was defined in the config file.
--
-- Note that 'Config' might change in the future (e.g., for adding comments). It
-- is therefore recommended to use 'globalSection' and 'sections'.
data Config
    = Config
    { cGlobalSection :: SAssocList String
    , cSections   :: SAssocList (SAssocList String)
    } deriving Show

globalSection :: Config -> SAssocList String
globalSection = cGlobalSection

sections :: Config -> SAssocList (SAssocList String)
sections = cSections

instance Semigroup Config where
    -- | Merges all equally named sections into one. Destroys existing order
    -- of sections.
    Config defL sL <> Config defR sR =
        Config (defL ++ defR) $ map (fst . head &&& concatMap snd) $ groupBy ((==) `on` fst) $ merge sL sR
      where
        merge l = foldr (insertBy cmpFst) l . sortBy cmpFst
        cmpFst  = comparing fst

instance Monoid Config where
    mempty = Config [] []

infixr 6 +++

-- | Merges sections while preserving order and duplicate sections.
appendConfig, (+++) :: Config -> Config -> Config
appendConfig l r = Config (globalSection l ++ globalSection r) (sections l ++ sections r)
(+++) = appendConfig

