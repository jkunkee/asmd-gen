{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module ASMD.States where

import ASMD.Parseable

import Data.Aeson (ToJSON,FromJSON)
import Data.Text (Text)
import GHC.Generics
import qualified Data.Text as T
import qualified Data.List as L

data State = State 
  deriving (Generic,Show)

instance ToJSON State
instance FromJSON State

instance Parseable State where
  parse xs = []
