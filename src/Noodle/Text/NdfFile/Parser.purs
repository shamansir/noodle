module Noodle.Text.NdfFile.Parser where

import Prelude

import Data.Array (many) as P
import Data.List.NonEmpty as NEL
import Data.String (Pattern(..))
import Data.String (split, drop) as String
import Data.String.CodeUnits as CU
import Data.String.Extra2 (lines) as String
import Data.Array (drop, filter) as Array
import Data.Maybe (Maybe(..))
import Data.Foldable (sum) as F

import Parsing (Parser, Position(..)) as P
import Parsing.String (char, string, anyTill) as P
import Parsing.String.Basic (alphaNum, space, number, intDecimal) as P
import Parsing.Combinators (many1, many1Till, try, option) as P
import Parsing.Combinators ((<?>))
import Parsing.Extra (source, sourceAt) as P
import Parsing.String.Extra (tokenTill, eol) as P
import Control.Alt ((<|>))
import Data.Tuple as Tuple
import Data.Tuple.Nested ((/\))

import Noodle.Id (unsafeFamilyR) as Id
import Noodle.Text.NdfFile.Command (Command)
import Noodle.Text.NdfFile.Command (Command(..), ndfLinesCount, reviewOrder_) as Cmd
import Noodle.Text.NdfFile.Command.Op (CommandOp)
import Noodle.Text.NdfFile.Command.Op (CommandOp(..)) as Cmd
import Noodle.Text.NdfFile.Types (nodeInstanceId, coord, inletAlias, inletIndex, outletAlias, outletIndex, encodedValue, OutletId, InletId, NodeInstanceId, EncodedValue) as C

import Noodle.Text.NdfFile (NdfFile(..), Header(..), FailedLine(..))
import Noodle.Text.NdfFile.FamilyDef.Parser (parser, assignmentParser) as FamilyDef


type SParser a = P.Parser String a
type OpParser = P.Parser String CommandOp


createCommandOp :: OpParser
createCommandOp = do
    family <- P.tokenTill P.space
    _ <- P.many P.space
    x <- P.intDecimal
    _ <- P.many1 P.space
    y <- P.intDecimal
    _ <- P.many1 P.space
    instanceId <- P.tokenTill P.eol
    pure $ Cmd.MakeNode (Id.unsafeFamilyR family) (C.coord x) (C.coord y) (C.nodeInstanceId instanceId)


connectCommandOpII :: OpParser
connectCommandOpII =
    _command2OpHelper "<>" (P.intDecimal <#> C.outletIndex) true (P.intDecimal <#> C.inletIndex) true Cmd.Connect


connectCommandOpSS :: OpParser
connectCommandOpSS = do
    _command2OpHelper "<>" (P.tokenTill P.space <#> C.outletAlias) false (P.tokenTill P.eol <#> C.inletAlias) false Cmd.Connect


connectCommandOpIS :: OpParser
connectCommandOpIS = do
    _command2OpHelper "<>" (P.intDecimal <#> C.outletIndex) true (P.tokenTill P.eol <#> C.inletAlias) false Cmd.Connect


connectCommandOpSI :: OpParser
connectCommandOpSI = do
    _command2OpHelper "<>" (P.tokenTill P.space <#> C.outletAlias) false (P.intDecimal <#> C.inletIndex) true Cmd.Connect


disconnectCommandOpII :: OpParser
disconnectCommandOpII =
    _command2OpHelper "><" (P.intDecimal <#> C.outletIndex) true (P.intDecimal <#> C.inletIndex) true Cmd.Disconnect


disconnectCommandOpSS :: OpParser
disconnectCommandOpSS = do
    _command2OpHelper "><" (P.tokenTill P.space <#> C.outletAlias) false (P.tokenTill P.eol <#> C.inletAlias) false Cmd.Disconnect


disconnectCommandOpIS :: OpParser
disconnectCommandOpIS = do
    _command2OpHelper "><" (P.intDecimal <#> C.outletIndex) true (P.tokenTill P.eol <#> C.inletAlias) false Cmd.Disconnect


disconnectCommandOpSI :: OpParser
disconnectCommandOpSI = do
    _command2OpHelper "><" (P.tokenTill P.space <#> C.outletAlias) false (P.intDecimal <#> C.inletIndex) true Cmd.Disconnect


sendCommandOpI :: OpParser
sendCommandOpI = _command1OpHelper "->" (P.intDecimal <#> C.inletIndex) true Cmd.Send


sendCommandOpS :: OpParser
sendCommandOpS = _command1OpHelper "->" (P.tokenTill P.space <#> C.inletAlias) false Cmd.Send


sendOCommandOpI :: OpParser
sendOCommandOpI = _command1OpHelper "~>" (P.intDecimal <#> C.outletIndex) true Cmd.SendO


sendOCommandOpS :: OpParser
sendOCommandOpS = _command1OpHelper "~>" (P.tokenTill P.space <#> C.outletAlias) false Cmd.SendO


moveCommandOp :: OpParser
moveCommandOp = do
    _ <- P.string "."
    _ <- P.many1 P.space
    x <- P.intDecimal
    _ <- P.many1 P.space
    y <- P.intDecimal
    _ <- P.many1 P.space
    instanceId <- P.tokenTill P.eol
    pure $ Cmd.Move (C.nodeInstanceId instanceId) (C.coord x) (C.coord y)


comment :: OpParser
comment = do
    _ <- P.string "#"
    _ <- P.space
    content <- Tuple.fst <$> P.anyTill P.eol
    pure $ Cmd.Comment content


orderCommandOp :: OpParser
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


importCommandOp :: OpParser
importCommandOp = do
    _ <- P.string "i"
    _ <- P.space
    path <- Tuple.fst <$> P.anyTill P.eol
    pure $ Cmd.Import path


commandOp :: OpParser
commandOp =
  P.try (FamilyDef.parser <#> Cmd.DefineFamily) <?> "node definition"
  <|> P.try (FamilyDef.assignmentParser <#> Cmd.AssignProcess) <?> "process assign"
  <|> P.try connectCommandOpII <?> "connect command"
  <|> P.try connectCommandOpSS <?> "connect command"
  <|> P.try connectCommandOpIS <?> "connect command"
  <|> P.try connectCommandOpSI <?> "connect command"
  <|> P.try disconnectCommandOpII <?> "disconnect command"
  <|> P.try disconnectCommandOpSS <?> "disconnect command"
  <|> P.try disconnectCommandOpIS <?> "disconnect command"
  <|> P.try disconnectCommandOpSI <?> "disconnect command"
  <|> P.try sendCommandOpI <?> "send command"
  <|> P.try sendCommandOpS <?> "send command"
  <|> P.try sendOCommandOpI <?> "send command"
  <|> P.try sendOCommandOpS <?> "send command"
  <|> P.try createCommandOp <?> "create command"
  <|> P.try moveCommandOp <?> "move command"
  <|> P.try orderCommandOp <?> "order command"
  <|> P.try importCommandOp <?> "import command"
  <|> P.try comment <?> "comment"


command :: SParser Command
command = do
  source /\ pos <- P.sourceAt
  cmdOp <- commandOp
  pure $
    Cmd.Command
    (Just $
        -- FIXME: I didn't find any proper way to get a chunk where we succeeded in the parser
        --        The approach with `pos.index` failed somehow...
        --        It could be in String parsers somewhere though, but since we didn't use it yet...
        --        Using `toolkitList` below we can retreive the source line for sure, but it's harder with `NdfFile` implementation
        { line : CU.takeWhile (_ /= '\n') $ String.drop 1 source
        , lineIndex : case pos of
            P.Position { line } -> line + 1
        }
    )
    cmdOp


parser :: SParser NdfFile
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


_command2OpHelper :: forall oi ii. String -> SParser oi -> Boolean -> SParser ii -> Boolean -> (C.NodeInstanceId -> oi -> C.NodeInstanceId -> ii -> CommandOp) -> OpParser
_command2OpHelper marker pOutletId requireSpace pInletId withEol constructF = do
    _ <- P.string marker
    _ <- P.many1 P.space
    instanceFromId <- P.tokenTill P.space
    _ <- P.many P.space
    outletId <- pOutletId
    if (requireSpace)
        then P.many1 P.space *> pure unit
        else P.many  P.space *> pure unit
    instanceToId <- P.tokenTill P.space
    _ <- P.many P.space
    if withEol then do
        inletId <- pInletId
        P.eol
        pure $ constructF (C.nodeInstanceId instanceFromId) outletId (C.nodeInstanceId instanceToId) inletId
    else do
        inletId <- pInletId
        pure $ constructF (C.nodeInstanceId instanceFromId) outletId (C.nodeInstanceId instanceToId) inletId


_command1OpHelper :: forall subj. String -> SParser subj -> Boolean -> (C.NodeInstanceId -> subj -> C.EncodedValue -> CommandOp) -> OpParser
_command1OpHelper marker pSubj requireSpace constructF = do
    _ <- P.string marker
    _ <- P.many1 P.space
    instanceId <- P.tokenTill P.space
    _ <- P.many P.space
    subj <- pSubj
    if (requireSpace)
        then P.many1 P.space *> pure unit
        else P.many  P.space *> pure unit
    valueStr <- Tuple.fst <$> P.anyTill P.eol
    pure $ constructF (C.nodeInstanceId instanceId) subj (C.encodedValue valueStr)
