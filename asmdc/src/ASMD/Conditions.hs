{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module ASMD.Conditions where

import ASMD.Parseable

import Control.Applicative
import Data.Aeson (ToJSON,FromJSON)
import Data.Attoparsec.Text as A
import Data.Text (Text)
import Data.Char
import Data.Function (on)
import GHC.Generics
import qualified Data.Text as T
import qualified Data.List as L

data Condition = Condition { name  :: Text
                           , body  :: ITE
                           } deriving (Generic)
instance ToJSON Condition
instance FromJSON Condition

instance Show Condition where
  show (Condition n b) = L.concat ["(",inner,")"]
    where inner = L.intercalate ", " [T.unpack n, show b]

data ITE = Leaf Text 
         | ITE (Text, ITE, ITE)
         deriving (Generic)

instance ToJSON ITE
instance FromJSON ITE

instance Show ITE where
  show (Leaf t) = T.unpack t
  show (ITE (p,t,f)) = L.concat ["(ite ",inner,")"]
    where inner = L.intercalate " " [p', show t, show f]
          p' = T.unpack $ T.concat ["(",p,")"]

lexeme :: Parser a -> Parser a
lexeme p = p <* skipSpace

mandatorySpace = skip isSpace
keyword p = lexeme (p <* mandatorySpace)

cond = keyword (string "condition") <?> "Cond"
kif = keyword (string "if") <?> "If"
kthen = keyword (string "then") <?> "Then"
kelse = keyword (string "else") <?> "Else"
test = lexeme (char '(' *> manyTill anyChar (char ')')) <?> "Test"
blockName = lexeme (A.many1 (notChar ':') <* (char ':')) <?> "Block"
ident = lexeme (A.takeWhile $ not . isSpace) <?> "ID"

leaf :: Parser ITE
leaf = do
  t <- ident
  return $ Leaf t

branch :: Parser ITE
branch = do
  expr <- kif *> test
  true <-  kthen *> choice [branch,leaf]
  false <- kelse *> choice [branch,leaf]
  return (ITE ((T.pack expr),true,false))

conditionBlock :: Parser Condition
conditionBlock = do
  n <- cond *> blockName
  b <- branch
  return $ Condition { name = (T.pack n) , body = b }

conditionParser :: Parser [Condition]
conditionParser = many1 conditionBlock

instance Parseable Condition where
  parse t = case (parseOnly (skipSpace *> conditionParser <* endOfInput) blob)of
              Left _ -> []
              Right res -> res 
            where blob = T.unlines $ L.reverse $ t
