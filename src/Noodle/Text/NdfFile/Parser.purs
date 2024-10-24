module Noodle.Text.NdfFile.Parser where

import Prelude

import Data.Semigroup.Foldable (class Foldable1)
import Data.Array (many) as P
import Data.List.NonEmpty as NEL
import Data.String.NonEmpty.CodeUnits as CU
import Data.String.NonEmpty.Internal as StringX
import Data.Either (Either(..))

import Parsing (Parser) as P
import Parsing.String (char, string, anyTill) as P
import Parsing.String.Basic (alphaNum, space, number, intDecimal) as P
import Parsing.Combinators (many1, many1Till, try, option) as P
import Parsing.Combinators ((<?>))
import Parsing.String.Extra (tokenTill, eol) as P
import Control.Alt ((<|>))
import Data.Tuple as Tuple
import Data.Tuple.Nested ((/\))

import Noodle.Text.NdfFile.Command (Command)
import Noodle.Text.NdfFile.Command (Command(..)) as Cmd
import Noodle.Text.NdfFile.Types (nodeId, family, coord, inletAlias, inletIndex, outletAlias, outletIndex, encodedValue) as C

import Noodle.Text.NdfFile (NdfFile(..), Header(..), currentVersion)
import Noodle.Text.NdfFile.NodeDef.Parser (parser, assignmentParser) as NodeDef


createCommand :: P.Parser String Command
createCommand = do
    family <- P.tokenTill P.space
    _ <- P.many P.space
    x <- P.intDecimal
    _ <- P.many1 P.space
    y <- P.intDecimal
    _ <- P.many1 P.space
    instanceId <- P.tokenTill P.eol
    pure $ Cmd.MakeNode (C.family family) (C.coord x) (C.coord y) (C.nodeId instanceId)


connectCommandII :: P.Parser String Command
connectCommandII = do
    _ <- P.string "<>"
    _ <- P.many1 P.space
    instanceFromId <- P.tokenTill P.space
    _ <- P.many P.space
    outletIndex <- P.intDecimal
    _ <- P.many1 P.space
    instanceToId <- P.tokenTill P.space
    _ <- P.many P.space
    inletIndex <- P.intDecimal
    P.eol
    pure $ Cmd.Connect (C.nodeId instanceFromId) (C.outletIndex outletIndex) (C.nodeId instanceToId) (C.inletIndex inletIndex)


connectCommandSS :: P.Parser String Command
connectCommandSS = do
    _ <- P.string "<>"
    _ <- P.many1 P.space
    instanceFromId <- P.tokenTill P.space
    _ <- P.many P.space
    outletId <- P.tokenTill P.space
    _ <- P.many P.space
    instanceToId <- P.tokenTill P.space
    _ <- P.many P.space
    inletId <- P.tokenTill P.eol
    pure $ Cmd.Connect (C.nodeId instanceFromId) (C.outletAlias outletId) (C.nodeId instanceToId) (C.inletAlias inletId)


connectCommandIS :: P.Parser String Command
connectCommandIS = do
    _ <- P.string "<>"
    _ <- P.many1 P.space
    instanceFromId <- P.tokenTill P.space
    _ <- P.many P.space
    outletIndex <- P.intDecimal
    _ <- P.many1 P.space
    instanceToId <- P.tokenTill P.space
    _ <- P.many P.space
    inletId <- P.tokenTill P.eol
    pure $ Cmd.Connect (C.nodeId instanceFromId) (C.outletIndex outletIndex) (C.nodeId instanceToId) (C.inletAlias inletId)


connectCommandSI :: P.Parser String Command
connectCommandSI = do
    _ <- P.string "<>"
    _ <- P.many1 P.space
    instanceFromId <- P.tokenTill P.space
    _ <- P.many P.space
    outletId <- P.tokenTill P.space
    _ <- P.many P.space
    instanceToId <- P.tokenTill P.space
    _ <- P.many P.space
    inletIndex <- P.intDecimal
    P.eol
    pure $ Cmd.Connect (C.nodeId instanceFromId) (C.outletAlias outletId) (C.nodeId instanceToId) (C.inletIndex inletIndex)


sendCommandI :: P.Parser String Command
sendCommandI = do
    _ <- P.string "->"
    _ <- P.many1 P.space
    instanceId <- P.tokenTill P.space
    _ <- P.many P.space
    inletIndex <- P.intDecimal
    _ <- P.many1 P.space
    valueStr <- Tuple.fst <$> P.anyTill P.eol
    pure $ Cmd.Send (C.nodeId instanceId) (C.inletIndex inletIndex) (C.encodedValue valueStr)


sendCommandS :: P.Parser String Command
sendCommandS = do
    _ <- P.string "->"
    _ <- P.many1 P.space
    instanceId <- P.tokenTill P.space
    _ <- P.many P.space
    inletId <- P.tokenTill P.space
    _ <- P.many P.space
    valueStr <- Tuple.fst <$> P.anyTill P.eol
    pure $ Cmd.Send (C.nodeId instanceId) (C.inletAlias inletId) (C.encodedValue valueStr)


sendOCommandI :: P.Parser String Command
sendOCommandI = do
    _ <- P.string "~>"
    _ <- P.many1 P.space
    instanceId <- P.tokenTill P.space
    _ <- P.many P.space
    outletIndex <- P.intDecimal
    _ <- P.many1 P.space
    valueStr <- Tuple.fst <$> P.anyTill P.eol
    pure $ Cmd.SendO (C.nodeId instanceId) (C.outletIndex outletIndex) (C.encodedValue valueStr)


sendOCommandS :: P.Parser String Command
sendOCommandS = do
    _ <- P.string "~>"
    _ <- P.many1 P.space
    instanceId <- P.tokenTill P.space
    _ <- P.many P.space
    outletId <- P.tokenTill P.space
    _ <- P.many P.space
    valueStr <- Tuple.fst <$> P.anyTill P.eol
    pure $ Cmd.SendO(C.nodeId instanceId) (C.outletAlias outletId) (C.encodedValue valueStr)


moveCommand :: P.Parser String Command
moveCommand = do
    _ <- P.string "."
    _ <- P.many1 P.space
    x <- P.intDecimal
    _ <- P.many1 P.space
    y <- P.intDecimal
    _ <- P.many1 P.space
    instanceId <- P.tokenTill P.eol
    pure $ Cmd.Move (C.nodeId instanceId) (C.coord x) (C.coord y)


comment :: P.Parser String Command
comment = do
    _ <- P.string "#"
    _ <- P.space
    content <- Tuple.fst <$> P.anyTill P.eol
    pure $ Cmd.Comment content


command :: P.Parser String Command
command =
  P.try (NodeDef.parser <#> Cmd.DefineNode) <?> "node definition"
  <|> P.try (NodeDef.assignmentParser <#> Cmd.AssignProcess) <?> "process assign"
  <|> P.try connectCommandII <?> "connect command"
  <|> P.try connectCommandSS <?> "connect command"
  <|> P.try connectCommandIS <?> "connect command"
  <|> P.try connectCommandSI <?> "connect command"
  <|> P.try sendCommandI <?> "send command"
  <|> P.try sendCommandS <?> "send command"
  <|> P.try sendOCommandI <?> "send command"
  <|> P.try sendOCommandS <?> "send command"
  <|> P.try createCommand <?> "create command"
  <|> P.try moveCommand <?> "move command"
  <|> P.try comment <?> "comment"


parser :: P.Parser String NdfFile
parser = do
  toolkit <- P.tokenTill P.space
  toolkitVersion <- P.number
  ndfVersion <- P.option 0.1 $ P.try $ P.many1 P.space *> P.number
  P.eol
  cmds <- P.many1 command
  pure $ NdfFile (Header { toolkit, toolkitVersion, ndfVersion }) $ NEL.toUnfoldable cmds
