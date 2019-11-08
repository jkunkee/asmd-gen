{-# LANGUAGE OverloadedStrings #-}
module ASMD.Parseable where

import Data.Text (Text)

class Parseable s where
  parse :: [Text] -> [s]   
