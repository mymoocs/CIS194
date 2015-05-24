{-# GHC_OPTIONS -Wall #-}
{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module HW06 where

import Data.Aeson
import Data.Monoid
import GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text as Text
import qualified Data.Text.IO as Text


-------------------------------------------------------------------------------
--- Exercise 1.

-- | a function that changes all occurence of
-- | "Y" to True nad all occurence of "N" to False
-- | no other part of the imput Value should change.
-- >>> ynToBool 

ynToBool :: Value -> Value

