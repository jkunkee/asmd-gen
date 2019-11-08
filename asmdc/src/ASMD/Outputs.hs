{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module ASMD.Outputs where

import ASMD.Parseable

import Data.Aeson (ToJSON,FromJSON)
import Data.Text (Text)
import GHC.Generics
import qualified Data.Text as T
import qualified Data.List as L

data Output = Output 
  deriving (Generic,Show)

instance ToJSON Output
instance FromJSON Output

instance Parseable Output where
  parse xs = []
