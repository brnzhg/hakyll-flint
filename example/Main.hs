-- | A small recipe site showing hakyll-flint's custom properties.
--
-- > cd example/site && cabal run flint-example -- build
module Main (main) where

import Hakyll (hakyll)
import Site (siteRules)

main :: IO ()
main = hakyll siteRules
