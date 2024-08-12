{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_rcal (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "rcal"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "Raid-cost calculator CLI for Rust"
copyright :: String
copyright = ""
homepage :: String
homepage = "https://github.com/LivewareIssue/rcal"
