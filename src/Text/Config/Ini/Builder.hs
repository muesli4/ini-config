{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | A simplistic configuration builder DSEL that builds sections from entries
-- and configurations from sections.
--
-- >>> formatConfig $ section "special" ("foo" ~= "42" <> "bar" ~= "baz") <> plain ("qux" ~= "23")
-- "qux = 23\n\n[special]\nfoo = 42\nbar = baz\n\n"
module Text.Config.Ini.Builder
    ( Section
    , (~=)
    , section
    , plain
    , (<>)
    ) where

import Data.Monoid

import Text.Config.Ini.Common

-- | Type to build sections. Sections are monoids that merge entries.
newtype Section = Section (Endo (SAssocList String)) deriving Monoid

runSection :: Section -> SAssocList String
runSection (Section endo) = appEndo endo []

-- | Precedence is one higher than '(<>)'.
infix 7 ~=

-- | Singleton key-value constructor for sections.
(~=) :: String -> String -> Section
k ~= v = Section $ Endo ((k, v) :)

-- | Name a section and create a configuration.
section :: String -> Section -> Config
section n s = Config [] [(n, runSection s)]

-- | Use a section without a name, which will put all entries within the default
-- section of the configuration.
plain :: Section -> Config
plain s = Config (runSection s) []

