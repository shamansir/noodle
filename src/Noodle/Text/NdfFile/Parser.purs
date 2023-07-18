module Noodle.Text.NdfFile.Parser where

import Prelude

import Data.Semigroup.Foldable (class Foldable1)
import Data.Array (many)
import Data.List.NonEmpty as NEL
import Data.String.NonEmpty.CodeUnits as CU
import Data.String.NonEmpty.Internal as StringX

import Parsing (Parser)
import Parsing.String (char, string, anyTill)
import Parsing.String.Basic (alphaNum, space, number, intDecimal)
import Parsing.Combinators (many1, many1Till, try)
import Control.Alt ((<|>))
import Data.Tuple as Tuple
import Data.Tuple.Nested ((/\))
import Noodle.Text.NdfFile.Command (Command(..))
import Noodle.Text.NdfFile (NdfFile(..), Header(..))

myFile :: String
myFile =
  """hydra 0.1
osc 40 60 osc-0
osc 40 60 osc-0
pi 20 20 pi-0
number 40 40 num-0
<> pi-0 0 osc-0 0
<> num-0 0 osc-0 1
-> osc-0 0 N 20.0
~> num-0 0 N 40.0
"""


createCommand :: Parser String Command
createCommand = do
    family <- tokenTill space
    _ <- many space
    x <- intDecimal
    _ <- many1 space
    y <- intDecimal
    _ <- many1 space
    instanceId <- tokenTill eol
    pure $ MakeNode family x y instanceId


connectCommand :: Parser String Command
connectCommand = do
    _ <- string "<>"
    _ <- many1 space
    instanceFromId <- tokenTill space
    _ <- many space
    outputIndex <- intDecimal
    _ <- many1 space
    instanceToId <- tokenTill space
    _ <- many space
    inputIndex <- intDecimal
    eol
    pure $ Connect instanceFromId outputIndex instanceToId inputIndex


connectCommand_ :: Parser String Command
connectCommand_ = do
    _ <- string "<>"
    _ <- many1 space
    instanceFromId <- tokenTill space
    _ <- many space
    outputId <- tokenTill space
    _ <- many space
    instanceToId <- tokenTill space
    _ <- many space
    inputId <- tokenTill eol
    pure $ Connect_ instanceFromId outputId instanceToId inputId


sendCommand :: Parser String Command
sendCommand = do
    _ <- string "->"
    _ <- many1 space
    instanceId <- tokenTill space
    _ <- many space
    inputIndex <- intDecimal
    _ <- many1 space
    valueStr <- Tuple.fst <$> anyTill eol
    pure $ Send instanceId inputIndex valueStr


sendCommand_ :: Parser String Command
sendCommand_ = do
    _ <- string "->"
    _ <- many1 space
    instanceId <- tokenTill space
    _ <- many space
    inputId <- tokenTill space
    _ <- many space
    valueStr <- Tuple.fst <$> anyTill eol
    pure $ Send_ instanceId inputId valueStr


sendOCommand :: Parser String Command
sendOCommand = do
    _ <- string "~>"
    _ <- many1 space
    instanceId <- tokenTill space
    _ <- many space
    outputIndex <- intDecimal
    _ <- many1 space
    valueStr <- Tuple.fst <$> anyTill eol
    pure $ SendO instanceId outputIndex valueStr


sendOCommand_ :: Parser String Command
sendOCommand_ = do
    _ <- string "~>"
    _ <- many1 space
    instanceId <- tokenTill space
    _ <- many space
    outputId <- tokenTill space
    _ <- many space
    valueStr <- Tuple.fst <$> anyTill eol
    pure $ SendO_ instanceId outputId valueStr


command :: Parser String Command
command =
  try connectCommand
  <|> try connectCommand_
  <|> try sendCommand
  <|> try sendCommand_
  <|> try sendOCommand
  <|> try sendOCommand_
  <|> try createCommand


parser :: Parser String NdfFile
parser = do
  toolkit <- tokenTill space
  version <- number
  eol
  cmds <- many1 command
  pure $ NdfFile (Header $ toolkit /\ version) $ NEL.toUnfoldable cmds


tokenChar :: Parser String Char
tokenChar = alphaNum <|> char '-'


token :: Parser String String
token =
  f1ts <$> many1 tokenChar
  where
    f1ts :: forall f. Foldable1 f => f Char -> String
    f1ts = CU.fromFoldable1 >>> StringX.toString


tokenTill :: forall a. Parser String a -> Parser String String
tokenTill stopAt =
  f1ts <$> many1Till tokenChar stopAt
  where
    f1ts :: forall f. Foldable1 f => f Char -> String
    f1ts = CU.fromFoldable1 >>> StringX.toString


eol :: Parser String Unit
eol = char '\n' *> pure unit
