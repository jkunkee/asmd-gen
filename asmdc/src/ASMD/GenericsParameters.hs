{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module ASMD.GenericsParameters where

import ASMD.Parseable

import Data.Aeson (ToJSON,FromJSON)
import Data.Text (Text)
import GHC.Generics
import qualified Data.Text as T
import qualified Data.List as L

data GenericParameter = GenericParameter 
  deriving (Generic,Show)

instance ToJSON GenericParameter
instance FromJSON GenericParameter

instance Parseable GenericParameter where
  parse xs = []
