{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module ASMD.Inputs where

import ASMD.Parseable

import Data.Aeson (ToJSON,FromJSON)
import Data.Text (Text)
import GHC.Generics
import qualified Data.Text as T
import qualified Data.List as L

data Input = Input 
  deriving (Generic,Show)

instance ToJSON Input
instance FromJSON Input

instance Parseable Input where
  parse xs = []
