{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    ) where

import ASMD (ASMD)
import Parser

import Data.Aeson (encode,decode)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

back :: ByteString -> Maybe ASMD
back = decode

msg :: Maybe ASMD -> T.Text
msg (Just a) = T.pack $ show a
msg Nothing = "Couldn't decode"

someFunc :: IO ()
someFunc = TIO.putStrLn . msg . back . encode . asmd =<< TIO.readFile "../test.asmd"
