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
import Parsing.Combinators (many1, many1Till, try, option, (<?>))
import Control.Alt ((<|>))
import Data.Tuple as Tuple
import Data.Tuple.Nested ((/\))

import Noodle.Text.NdfFile.Command (Command)
import Noodle.Text.NdfFile.Command (Command(..)) as Cmd
import Noodle.Text.NdfFile.Types (nodeId, family, coord, inletAlias, inletIndex, outletAlias, outletIndex, encodedValue) as C

import Noodle.Text.NdfFile (NdfFile(..), Header(..), currentVersion)
import Noodle.Text.NdfFile.NodeDef.Parser (parser, assignmentParser) as NodeDef


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
    outletIndex <- intDecimal
    _ <- many1 space
    instanceToId <- tokenTill space
    _ <- many space
    inletIndex <- intDecimal
    eol
    pure $ Cmd.Connect (C.nodeId instanceFromId) (C.outletIndex outletIndex) (C.nodeId instanceToId) (C.inletIndex inletIndex)


connectCommandSS :: Parser String Command
connectCommandSS = do
    _ <- string "<>"
    _ <- many1 space
    instanceFromId <- tokenTill space
    _ <- many space
    outletId <- tokenTill space
    _ <- many space
    instanceToId <- tokenTill space
    _ <- many space
    inletId <- tokenTill eol
    pure $ Cmd.Connect (C.nodeId instanceFromId) (C.outletAlias outletId) (C.nodeId instanceToId) (C.inletAlias inletId)


connectCommandIS :: Parser String Command
connectCommandIS = do
    _ <- string "<>"
    _ <- many1 space
    instanceFromId <- tokenTill space
    _ <- many space
    outletIndex <- intDecimal
    _ <- many1 space
    instanceToId <- tokenTill space
    _ <- many space
    inletId <- tokenTill eol
    pure $ Cmd.Connect (C.nodeId instanceFromId) (C.outletIndex outletIndex) (C.nodeId instanceToId) (C.inletAlias inletId)


connectCommandSI :: Parser String Command
connectCommandSI = do
    _ <- string "<>"
    _ <- many1 space
    instanceFromId <- tokenTill space
    _ <- many space
    outletId <- tokenTill space
    _ <- many space
    instanceToId <- tokenTill space
    _ <- many space
    inletIndex <- intDecimal
    eol
    pure $ Cmd.Connect (C.nodeId instanceFromId) (C.outletAlias outletId) (C.nodeId instanceToId) (C.inletIndex inletIndex)


sendCommandI :: Parser String Command
sendCommandI = do
    _ <- string "->"
    _ <- many1 space
    instanceId <- tokenTill space
    _ <- many space
    inletIndex <- intDecimal
    _ <- many1 space
    valueStr <- Tuple.fst <$> anyTill eol
    pure $ Cmd.Send (C.nodeId instanceId) (C.inletIndex inletIndex) (C.encodedValue valueStr)


sendCommandS :: Parser String Command
sendCommandS = do
    _ <- string "->"
    _ <- many1 space
    instanceId <- tokenTill space
    _ <- many space
    inletId <- tokenTill space
    _ <- many space
    valueStr <- Tuple.fst <$> anyTill eol
    pure $ Cmd.Send (C.nodeId instanceId) (C.inletAlias inletId) (C.encodedValue valueStr)


sendOCommandI :: Parser String Command
sendOCommandI = do
    _ <- string "~>"
    _ <- many1 space
    instanceId <- tokenTill space
    _ <- many space
    outletIndex <- intDecimal
    _ <- many1 space
    valueStr <- Tuple.fst <$> anyTill eol
    pure $ Cmd.SendO (C.nodeId instanceId) (C.outletIndex outletIndex) (C.encodedValue valueStr)


sendOCommandS :: Parser String Command
sendOCommandS = do
    _ <- string "~>"
    _ <- many1 space
    instanceId <- tokenTill space
    _ <- many space
    outletId <- tokenTill space
    _ <- many space
    valueStr <- Tuple.fst <$> anyTill eol
    pure $ Cmd.SendO(C.nodeId instanceId) (C.outletAlias outletId) (C.encodedValue valueStr)


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


comment :: Parser String Command
comment = do
    _ <- string "#"
    _ <- space
    content <- Tuple.fst <$> anyTill eol
    pure $ Cmd.Comment content


command :: Parser String Command
command =
  try (NodeDef.parser <#> Cmd.DefineNode) <?> "node definition"
  <|> try (NodeDef.assignmentParser <#> Cmd.AssignProcess) <?> "process assign"
  <|> try connectCommandII <?> "connect command"
  <|> try connectCommandSS <?> "connect command"
  <|> try connectCommandIS <?> "connect command"
  <|> try connectCommandSI <?> "connect command"
  <|> try sendCommandI <?> "send command"
  <|> try sendCommandS <?> "send command"
  <|> try sendOCommandI <?> "send command"
  <|> try sendOCommandS <?> "send command"
  <|> try createCommand <?> "create command"
  <|> try moveCommand <?> "move command"
  <|> try comment <?> "comment"


parser :: Parser String NdfFile
parser = do
  toolkit <- tokenTill space
  toolkitVersion <- number
  ndfVersion <- option 0.1 $ try $ many1 space *> number
  eol
  cmds <- many1 command
  pure $ NdfFile (Header { toolkit, toolkitVersion, ndfVersion }) $ NEL.toUnfoldable cmds


tokenChar :: Parser String Char
tokenChar = alphaNum <|> char '-'


commentChar :: Parser String Char
commentChar = alphaNum <|> char '-'


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
