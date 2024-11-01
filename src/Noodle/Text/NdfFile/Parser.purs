module Noodle.Text.NdfFile.Parser where

import Prelude

import Data.Semigroup.Foldable (class Foldable1)
import Data.Array (many) as P
import Data.List.NonEmpty as NEL
import Data.String (Pattern(..))
import Data.String (split) as String
import Data.String.NonEmpty.CodeUnits as CU
import Data.String.NonEmpty.Internal as StringX
import Data.Either (Either(..))
import Data.String.Extra2 (lines) as String
import Data.Array (drop, length) as Array
import Data.Foldable (sum) as F

import Parsing (Parser) as P
import Parsing.String (char, string, anyTill) as P
import Parsing.String.Basic (alphaNum, space, number, intDecimal) as P
import Parsing.Combinators (many1, many1Till, try, option) as P
import Parsing.Combinators ((<?>))
import Parsing.Extra (source) as P
import Parsing.String.Extra (tokenTill, eol) as P
import Control.Alt ((<|>))
import Data.Tuple as Tuple
import Data.Tuple.Nested ((/\))

import Noodle.Id (unsafeFamilyR) as Id
import Noodle.Text.NdfFile.Command (Command)
import Noodle.Text.NdfFile.Command (Command(..), ndfLinesCount, reviewOrder_) as Cmd
import Noodle.Text.NdfFile.Types (ndfNodeId, coord, inletAlias, inletIndex, outletAlias, outletIndex, encodedValue) as C

import Noodle.Text.NdfFile (NdfFile(..), Header(..), currentVersion, FailedLine(..))
import Noodle.Text.NdfFile.FamilyDef.Parser (parser, assignmentParser) as FamilyDef


createCommand :: P.Parser String Command
createCommand = do
    family <- P.tokenTill P.space
    _ <- P.many P.space
    x <- P.intDecimal
    _ <- P.many1 P.space
    y <- P.intDecimal
    _ <- P.many1 P.space
    instanceId <- P.tokenTill P.eol
    pure $ Cmd.MakeNode (Id.unsafeFamilyR family) (C.coord x) (C.coord y) (C.ndfNodeId instanceId)


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
    pure $ Cmd.Connect (C.ndfNodeId instanceFromId) (C.outletIndex outletIndex) (C.ndfNodeId instanceToId) (C.inletIndex inletIndex)


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
    pure $ Cmd.Connect (C.ndfNodeId instanceFromId) (C.outletAlias outletId) (C.ndfNodeId instanceToId) (C.inletAlias inletId)


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
    pure $ Cmd.Connect (C.ndfNodeId instanceFromId) (C.outletIndex outletIndex) (C.ndfNodeId instanceToId) (C.inletAlias inletId)


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
    pure $ Cmd.Connect (C.ndfNodeId instanceFromId) (C.outletAlias outletId) (C.ndfNodeId instanceToId) (C.inletIndex inletIndex)


sendCommandI :: P.Parser String Command
sendCommandI = do
    _ <- P.string "->"
    _ <- P.many1 P.space
    instanceId <- P.tokenTill P.space
    _ <- P.many P.space
    inletIndex <- P.intDecimal
    _ <- P.many1 P.space
    valueStr <- Tuple.fst <$> P.anyTill P.eol
    pure $ Cmd.Send (C.ndfNodeId instanceId) (C.inletIndex inletIndex) (C.encodedValue valueStr)


sendCommandS :: P.Parser String Command
sendCommandS = do
    _ <- P.string "->"
    _ <- P.many1 P.space
    instanceId <- P.tokenTill P.space
    _ <- P.many P.space
    inletId <- P.tokenTill P.space
    _ <- P.many P.space
    valueStr <- Tuple.fst <$> P.anyTill P.eol
    pure $ Cmd.Send (C.ndfNodeId instanceId) (C.inletAlias inletId) (C.encodedValue valueStr)


sendOCommandI :: P.Parser String Command
sendOCommandI = do
    _ <- P.string "~>"
    _ <- P.many1 P.space
    instanceId <- P.tokenTill P.space
    _ <- P.many P.space
    outletIndex <- P.intDecimal
    _ <- P.many1 P.space
    valueStr <- Tuple.fst <$> P.anyTill P.eol
    pure $ Cmd.SendO (C.ndfNodeId instanceId) (C.outletIndex outletIndex) (C.encodedValue valueStr)


sendOCommandS :: P.Parser String Command
sendOCommandS = do
    _ <- P.string "~>"
    _ <- P.many1 P.space
    instanceId <- P.tokenTill P.space
    _ <- P.many P.space
    outletId <- P.tokenTill P.space
    _ <- P.many P.space
    valueStr <- Tuple.fst <$> P.anyTill P.eol
    pure $ Cmd.SendO(C.ndfNodeId instanceId) (C.outletAlias outletId) (C.encodedValue valueStr)


moveCommand :: P.Parser String Command
moveCommand = do
    _ <- P.string "."
    _ <- P.many1 P.space
    x <- P.intDecimal
    _ <- P.many1 P.space
    y <- P.intDecimal
    _ <- P.many1 P.space
    instanceId <- P.tokenTill P.eol
    pure $ Cmd.Move (C.ndfNodeId instanceId) (C.coord x) (C.coord y)


comment :: P.Parser String Command
comment = do
    _ <- P.string "#"
    _ <- P.space
    content <- Tuple.fst <$> P.anyTill P.eol
    pure $ Cmd.Comment content


orderCommand :: P.Parser String Command
orderCommand = do
    _ <- P.string "*"
    _ <- P.space
    -- _ <- P.string "| "
    content <- Tuple.fst <$> P.anyTill P.eol -- (P.anyTill $ P.string " |")
    -- _ <- P.string " |"
    -- P.eol
    pure
        $ Cmd.Order
        $ Cmd.reviewOrder_
        $ (String.split $ Pattern " ")
       <$> String.split (Pattern " | ") content


importCommand :: P.Parser String Command
importCommand = do
    _ <- P.string "i"
    _ <- P.space
    path <- Tuple.fst <$> P.anyTill P.eol
    pure $ Cmd.Import path


command :: P.Parser String Command
command =
  P.try (FamilyDef.parser <#> Cmd.DefineFamily) <?> "node definition"
  <|> P.try (FamilyDef.assignmentParser <#> Cmd.AssignProcess) <?> "process assign"
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
  <|> P.try orderCommand <?> "order command"
  <|> P.try importCommand <?> "import command"
  <|> P.try comment <?> "comment"


parser :: P.Parser String NdfFile
parser = do
  source <- P.source
  toolkit <- P.tokenTill P.space
  toolkitVersion <- P.number
  ndfVersion <- P.option 0.1 $ P.try $ P.many1 P.space *> P.number
  P.eol
  cmds <- P.many1 command
  let
    cmdsArray = NEL.toUnfoldable cmds
    -- one line as a header + parsed cmds, usually one per line, but processCode may take longer
    failedLines = FailedLine <$> (Array.drop (1 + (F.sum $ Cmd.ndfLinesCount <$> cmdsArray)) $ String.lines source)
  pure $ NdfFile (Header { toolkit, toolkitVersion, ndfVersion }) failedLines cmdsArray
