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
import Data.Array (drop, length, filter) as Array
import Data.Maybe (Maybe(..))
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
import Noodle.Text.NdfFile.Command.Op (CommandOp)
import Noodle.Text.NdfFile.Command.Op (CommandOp(..)) as Cmd
import Noodle.Text.NdfFile.Types (nodeInstanceId, coord, inletAlias, inletIndex, outletAlias, outletIndex, encodedValue) as C

import Noodle.Text.NdfFile (NdfFile(..), Header(..), currentVersion, FailedLine(..))
import Noodle.Text.NdfFile.FamilyDef.Parser (parser, assignmentParser) as FamilyDef


createCommandOp :: P.Parser String CommandOp
createCommandOp = do
    family <- P.tokenTill P.space
    _ <- P.many P.space
    x <- P.intDecimal
    _ <- P.many1 P.space
    y <- P.intDecimal
    _ <- P.many1 P.space
    instanceId <- P.tokenTill P.eol
    pure $ Cmd.MakeNode (Id.unsafeFamilyR family) (C.coord x) (C.coord y) (C.nodeInstanceId instanceId)


connectCommandOpII :: P.Parser String CommandOp
connectCommandOpII = do
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
    pure $ Cmd.Connect (C.nodeInstanceId instanceFromId) (C.outletIndex outletIndex) (C.nodeInstanceId instanceToId) (C.inletIndex inletIndex)


connectCommandOpSS :: P.Parser String CommandOp
connectCommandOpSS = do
    _ <- P.string "<>"
    _ <- P.many1 P.space
    instanceFromId <- P.tokenTill P.space
    _ <- P.many P.space
    outletId <- P.tokenTill P.space
    _ <- P.many P.space
    instanceToId <- P.tokenTill P.space
    _ <- P.many P.space
    inletId <- P.tokenTill P.eol
    pure $ Cmd.Connect (C.nodeInstanceId instanceFromId) (C.outletAlias outletId) (C.nodeInstanceId instanceToId) (C.inletAlias inletId)


connectCommandOpIS :: P.Parser String CommandOp
connectCommandOpIS = do
    _ <- P.string "<>"
    _ <- P.many1 P.space
    instanceFromId <- P.tokenTill P.space
    _ <- P.many P.space
    outletIndex <- P.intDecimal
    _ <- P.many1 P.space
    instanceToId <- P.tokenTill P.space
    _ <- P.many P.space
    inletId <- P.tokenTill P.eol
    pure $ Cmd.Connect (C.nodeInstanceId instanceFromId) (C.outletIndex outletIndex) (C.nodeInstanceId instanceToId) (C.inletAlias inletId)


connectCommandOpSI :: P.Parser String CommandOp
connectCommandOpSI = do
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
    pure $ Cmd.Connect (C.nodeInstanceId instanceFromId) (C.outletAlias outletId) (C.nodeInstanceId instanceToId) (C.inletIndex inletIndex)


sendCommandOpI :: P.Parser String CommandOp
sendCommandOpI = do
    _ <- P.string "->"
    _ <- P.many1 P.space
    instanceId <- P.tokenTill P.space
    _ <- P.many P.space
    inletIndex <- P.intDecimal
    _ <- P.many1 P.space
    valueStr <- Tuple.fst <$> P.anyTill P.eol
    pure $ Cmd.Send (C.nodeInstanceId instanceId) (C.inletIndex inletIndex) (C.encodedValue valueStr)


sendCommandOpS :: P.Parser String CommandOp
sendCommandOpS = do
    _ <- P.string "->"
    _ <- P.many1 P.space
    instanceId <- P.tokenTill P.space
    _ <- P.many P.space
    inletId <- P.tokenTill P.space
    _ <- P.many P.space
    valueStr <- Tuple.fst <$> P.anyTill P.eol
    pure $ Cmd.Send (C.nodeInstanceId instanceId) (C.inletAlias inletId) (C.encodedValue valueStr)


sendOCommandOpI :: P.Parser String CommandOp
sendOCommandOpI = do
    _ <- P.string "~>"
    _ <- P.many1 P.space
    instanceId <- P.tokenTill P.space
    _ <- P.many P.space
    outletIndex <- P.intDecimal
    _ <- P.many1 P.space
    valueStr <- Tuple.fst <$> P.anyTill P.eol
    pure $ Cmd.SendO (C.nodeInstanceId instanceId) (C.outletIndex outletIndex) (C.encodedValue valueStr)


sendOCommandOpS :: P.Parser String CommandOp
sendOCommandOpS = do
    _ <- P.string "~>"
    _ <- P.many1 P.space
    instanceId <- P.tokenTill P.space
    _ <- P.many P.space
    outletId <- P.tokenTill P.space
    _ <- P.many P.space
    valueStr <- Tuple.fst <$> P.anyTill P.eol
    pure $ Cmd.SendO(C.nodeInstanceId instanceId) (C.outletAlias outletId) (C.encodedValue valueStr)


moveCommandOp :: P.Parser String CommandOp
moveCommandOp = do
    _ <- P.string "."
    _ <- P.many1 P.space
    x <- P.intDecimal
    _ <- P.many1 P.space
    y <- P.intDecimal
    _ <- P.many1 P.space
    instanceId <- P.tokenTill P.eol
    pure $ Cmd.Move (C.nodeInstanceId instanceId) (C.coord x) (C.coord y)


comment :: P.Parser String CommandOp
comment = do
    _ <- P.string "#"
    _ <- P.space
    content <- Tuple.fst <$> P.anyTill P.eol
    pure $ Cmd.Comment content


orderCommandOp :: P.Parser String CommandOp
orderCommandOp = do
    _ <- P.string "*"
    _ <- P.space
    -- _ <- P.string "| "
    content <- Tuple.fst <$> P.anyTill P.eol -- (P.anyTill $ P.string " |")
    -- _ <- P.string " |"
    -- P.eol
    pure
        $ Cmd.Order
        $ Cmd.reviewOrder_
        $ map (map Id.unsafeFamilyR)
        $ map (Array.filter (_ /= "|"))
        $ (String.split $ Pattern " ")
       <$> String.split (Pattern " | ") content


importCommandOp :: P.Parser String CommandOp
importCommandOp = do
    _ <- P.string "i"
    _ <- P.space
    path <- Tuple.fst <$> P.anyTill P.eol
    pure $ Cmd.Import path


commandOp :: P.Parser String CommandOp
commandOp =
  P.try (FamilyDef.parser <#> Cmd.DefineFamily) <?> "node definition"
  <|> P.try (FamilyDef.assignmentParser <#> Cmd.AssignProcess) <?> "process assign"
  <|> P.try connectCommandOpII <?> "connect command"
  <|> P.try connectCommandOpSS <?> "connect command"
  <|> P.try connectCommandOpIS <?> "connect command"
  <|> P.try connectCommandOpSI <?> "connect command"
  <|> P.try sendCommandOpI <?> "send command"
  <|> P.try sendCommandOpS <?> "send command"
  <|> P.try sendOCommandOpI <?> "send command"
  <|> P.try sendOCommandOpS <?> "send command"
  <|> P.try createCommandOp <?> "create command"
  <|> P.try moveCommandOp <?> "move command"
  <|> P.try orderCommandOp <?> "order command"
  <|> P.try importCommandOp <?> "import command"
  <|> P.try comment <?> "comment"


parser :: P.Parser String NdfFile
parser = do
  source <- P.source
  toolkit <- P.tokenTill P.space
  toolkitVersion <- P.number
  ndfVersion <- P.option 0.1 $ P.try $ P.many1 P.space *> P.number
  P.eol
  cmdOps <- P.many1 commandOp
  let
    cmdsArray = Cmd.Command Nothing <$> NEL.toUnfoldable cmdOps -- FIXME: collect source info
    -- one line as a header + parsed cmds, usually one per line, but processCode may take longer
    failedLines = FailedLine <$> (Array.drop (1 + (F.sum $ Cmd.ndfLinesCount <$> cmdsArray)) $ String.lines source)
  pure $ NdfFile (Header { toolkit, toolkitVersion, ndfVersion }) failedLines cmdsArray
