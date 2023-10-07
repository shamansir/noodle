module Noodle.Text.NdfFile.Parser where

import Prelude

import Data.Semigroup.Foldable (class Foldable1)
import Data.Array (many)
import Data.List.NonEmpty as NEL
import Data.String.NonEmpty.CodeUnits as CU
import Data.String.NonEmpty.Internal as StringX
import Data.Either (Either(..))

import Parsing (Parser)
import Parsing.String (char, string, anyTill)
import Parsing.String.Basic (alphaNum, space, number, intDecimal)
import Parsing.Combinators (many1, many1Till, try)
import Control.Alt ((<|>))
import Data.Tuple as Tuple
import Data.Tuple.Nested ((/\))
import Noodle.Text.NdfFile.Command (Command)
import Noodle.Text.NdfFile.Command (Command(..)) as Cmd
import Noodle.Text.NdfFile.Command (nodeId, family, coord, inputAlias, inputIndex, outputAlias, outputIndex, encodedValue) as C

import Noodle.Text.NdfFile (NdfFile(..), Header(..))


createCommand :: Parser String Command
createCommand = do
    family <- tokenTill space
    _ <- many space
    x <- intDecimal
    _ <- many1 space
    y <- intDecimal
    _ <- many1 space
    instanceId <- tokenTill eol
    pure $ Cmd.MakeNode (C.family family) (C.coord x) (C.coord y) (C.nodeId instanceId)


connectCommandII :: Parser String Command
connectCommandII = do
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
    pure $ Cmd.Connect (C.nodeId instanceFromId) (C.outputIndex outputIndex) (C.nodeId instanceToId) (C.inputIndex inputIndex)


connectCommandSS :: Parser String Command
connectCommandSS = do
    _ <- string "<>"
    _ <- many1 space
    instanceFromId <- tokenTill space
    _ <- many space
    outputId <- tokenTill space
    _ <- many space
    instanceToId <- tokenTill space
    _ <- many space
    inputId <- tokenTill eol
    pure $ Cmd.Connect (C.nodeId instanceFromId) (C.outputAlias outputId) (C.nodeId instanceToId) (C.inputAlias inputId)


connectCommandIS :: Parser String Command
connectCommandIS = do
    _ <- string "<>"
    _ <- many1 space
    instanceFromId <- tokenTill space
    _ <- many space
    outputIndex <- intDecimal
    _ <- many1 space
    instanceToId <- tokenTill space
    _ <- many space
    inputId <- tokenTill eol
    pure $ Cmd.Connect (C.nodeId instanceFromId) (C.outputIndex outputIndex) (C.nodeId instanceToId) (C.inputAlias inputId)


connectCommandSI :: Parser String Command
connectCommandSI = do
    _ <- string "<>"
    _ <- many1 space
    instanceFromId <- tokenTill space
    _ <- many space
    outputId <- tokenTill space
    _ <- many space
    instanceToId <- tokenTill space
    _ <- many space
    inputIndex <- intDecimal
    eol
    pure $ Cmd.Connect (C.nodeId instanceFromId) (C.outputAlias outputId) (C.nodeId instanceToId) (C.inputIndex inputIndex)


sendCommandI :: Parser String Command
sendCommandI = do
    _ <- string "->"
    _ <- many1 space
    instanceId <- tokenTill space
    _ <- many space
    inputIndex <- intDecimal
    _ <- many1 space
    valueStr <- Tuple.fst <$> anyTill eol
    pure $ Cmd.Send (C.nodeId instanceId) (C.inputIndex inputIndex) (C.encodedValue valueStr)


sendCommandS :: Parser String Command
sendCommandS = do
    _ <- string "->"
    _ <- many1 space
    instanceId <- tokenTill space
    _ <- many space
    inputId <- tokenTill space
    _ <- many space
    valueStr <- Tuple.fst <$> anyTill eol
    pure $ Cmd.Send (C.nodeId instanceId) (C.inputAlias inputId) (C.encodedValue valueStr)


sendOCommandI :: Parser String Command
sendOCommandI = do
    _ <- string "~>"
    _ <- many1 space
    instanceId <- tokenTill space
    _ <- many space
    outputIndex <- intDecimal
    _ <- many1 space
    valueStr <- Tuple.fst <$> anyTill eol
    pure $ Cmd.SendO (C.nodeId instanceId) (C.outputIndex outputIndex) (C.encodedValue valueStr)


sendOCommandS :: Parser String Command
sendOCommandS = do
    _ <- string "~>"
    _ <- many1 space
    instanceId <- tokenTill space
    _ <- many space
    outputId <- tokenTill space
    _ <- many space
    valueStr <- Tuple.fst <$> anyTill eol
    pure $ Cmd.SendO(C.nodeId instanceId) (C.outputAlias outputId) (C.encodedValue valueStr)


moveCommand :: Parser String Command
moveCommand = do
    _ <- string "."
    _ <- many1 space
    x <- intDecimal
    _ <- many1 space
    y <- intDecimal
    _ <- many1 space
    instanceId <- tokenTill eol
    pure $ Cmd.Move (C.nodeId instanceId) (C.coord x) (C.coord y)


command :: Parser String Command
command =
  -- FIXME: just make a separate parser for InputId / OutputId
  try connectCommandII
  <|> try connectCommandSS
  <|> try connectCommandIS
  <|> try connectCommandSI
  <|> try sendCommandI
  <|> try sendCommandS
  <|> try sendOCommandI
  <|> try sendOCommandS
  <|> try createCommand
  <|> try moveCommand


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
