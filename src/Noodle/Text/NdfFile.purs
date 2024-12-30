module Noodle.Text.NdfFile where

import Prelude

import Type.Proxy (Proxy)

import Data.Maybe (Maybe(..))
import Data.Array ((:))
import Data.Array (sortWith, length, fromFoldable, mapWithIndex, concat, snoc, filter) as Array
import Data.Array.Extra (sortUsing) as Array
import Data.Tuple (fst, snd) as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Data.Map (Map)
import Data.Map (empty, insert, update, values) as Map
import Data.Foldable (foldl)
import Data.Newtype (class Newtype, unwrap)

import Data.Text.Format (Tag, nl, space) as T

import Foreign (F, Foreign)
import Yoga.JSON (class ReadForeign, class WriteForeign)

import Noodle.Id (FamilyR, family) as Id
import Noodle.Toolkit (Name) as Toolkit
import Noodle.Text.NdfFile.Command (Command(..), commandsToNdf, commandsToTaggedNdf, FamiliesOrder)
import Noodle.Text.NdfFile.Command (_priority, op, fromOp) as Command
import Noodle.Text.NdfFile.Command (optimize) as Commands
import Noodle.Text.NdfFile.Command.Op (CommandOp(..))
import Noodle.Text.ToCode (class ToCode, class ToTaggedCode)
import Noodle.Text.Code.Target (NDF, ndf)
import Noodle.Text.NdfFile.Types (Source)
import Noodle.Text.NdfFile.FamilyDef (FamilyDef, ProcessAssign(..))
import Noodle.Text.NdfFile.FamilyDef (family, forceAssign) as FD
import Noodle.Text.NdfFile.FamilyDef.Codegen as FCG
import Noodle.Text.NdfFile.Codegen as MCG
import Noodle.Ui.Cli.Tagging (ndfVersion, tkVersion, toolkit) as T


currentVersion = 0.2 :: Number


newtype Header =
    Header
        { toolkit :: String
        , toolkitVersion :: Number
        , ndfVersion :: Number
        }


data NdfFile =
    NdfFile
        Header
        (Array FailedLine)
        (Array Command)


derive instance Eq Header
derive instance Eq NdfFile


newtype FailedLine = FailedLine String


derive instance Newtype FailedLine _
derive newtype instance Eq FailedLine
derive newtype instance Show FailedLine


instance Show NdfFile where
    show :: NdfFile -> String
    show = toNdfCode


instance ToCode NDF opts NdfFile where
    toCode :: Proxy NDF -> opts -> NdfFile -> String
    toCode = const $ const toNdfCode


instance ToTaggedCode NDF opts NdfFile where
    toTaggedCode :: Proxy NDF -> opts -> NdfFile -> T.Tag
    toTaggedCode = const $ const toTaggedNdfCode


init :: String -> Number -> NdfFile
init tk version = init_ { toolkit : tk, toolkitVersion : version, ndfVersion : currentVersion }


init_ :: { toolkit :: String
         , toolkitVersion :: Number
         , ndfVersion :: Number
         } -> NdfFile
init_ header = from_ header []


from :: String -> Number -> Array Command -> NdfFile
from tk version = from_ { toolkit : tk, toolkitVersion : version, ndfVersion : currentVersion }


from_ :: { toolkit :: String
         , toolkitVersion :: Number
         , ndfVersion :: Number
         } -> Array Command -> NdfFile
from_ header = NdfFile (Header header) []


infixl 6 snoc as :->
infixr 6 cons as <-:

infixl 6 snocOp as &->
infixr 6 consOp as <-&


cons :: Command -> NdfFile -> NdfFile
cons cmd (NdfFile header failedLines cmds) = NdfFile header failedLines $ cmd : cmds


snoc :: NdfFile -> Command -> NdfFile
snoc (NdfFile header failedLines cmds) cmd = NdfFile header failedLines $ cmds `Array.snoc` cmd


consOp :: CommandOp -> NdfFile -> NdfFile
consOp cmdop (NdfFile header failedLines cmds) = NdfFile header failedLines $ Command.fromOp cmdop : cmds


snocOp :: NdfFile -> CommandOp -> NdfFile
snocOp (NdfFile header failedLines cmds) cmdop = NdfFile header failedLines $ cmds `Array.snoc` Command.fromOp cmdop


prepend :: NdfFile -> NdfFile -> NdfFile
prepend = flip append


append :: NdfFile -> NdfFile -> NdfFile
append (NdfFile _ failedLinesA cmdsA) (NdfFile headerB failedLinesB cmdsB) = NdfFile headerB (failedLinesA <> failedLinesB) $ cmdsA <> cmdsB


toNdfCode :: NdfFile -> String
toNdfCode (NdfFile (Header { toolkit, toolkitVersion, ndfVersion }) _ commands) =
    toolkit <> " "
    <> show toolkitVersion
    <> (if ndfVersion == 0.1 then "\n" else " " <> show ndfVersion <> "\n")
    <> commandsToNdf commands


toTaggedNdfCode :: NdfFile -> T.Tag
toTaggedNdfCode (NdfFile (Header { toolkit, toolkitVersion, ndfVersion }) _ commands) =
    T.toolkit toolkit <> T.space
    <> T.tkVersion toolkitVersion
    <> (if ndfVersion == 0.1 then T.nl else T.space <> T.ndfVersion ndfVersion <> T.nl)
    <> commandsToTaggedNdf commands


extractCommands :: NdfFile -> Array Command
extractCommands (NdfFile _ _ commands) = commands


failedLines :: NdfFile -> Array FailedLine
failedLines (NdfFile _ failedLines' _) = failedLines'


hasFailedLines :: NdfFile -> Boolean
hasFailedLines = failedLines >>> Array.length >>> (_ > 0)


-- TODO: used when loading NDF file, to place imports before commands and process assignment after family definitions, but may be just ask user to order the file properly?
_normalize :: NdfFile -> NdfFile
_normalize (NdfFile header failedCommands commands) =
    NdfFile header failedCommands $ _normalizeCommands commands


-- TODO: used when loading NDF file, to place imports before commands and process assignment after family definitions, but may be just ask user to order the file properly?
_normalizeCommands :: Array Command -> Array Command
_normalizeCommands = Array.sortWith Command._priority


optimize :: NdfFile -> NdfFile
optimize (NdfFile header failedCommands commands) =
    NdfFile header failedCommands $ Commands.optimize commands


definitionsFromCommands_ :: Array Command -> Array (Maybe Source /\ FamilyDef)
definitionsFromCommands_ =
    foldl applyCommand Map.empty
        >>> Map.values
        >>> Array.fromFoldable
    where
        applyCommand :: Map Id.FamilyR (Maybe Source /\ FamilyDef) -> Command -> Map Id.FamilyR (Maybe Source /\ FamilyDef)
        applyCommand theMap =
            case _ of
                Command mbSource (DefineFamily familyDef) ->
                    theMap # Map.insert (FD.family familyDef) (mbSource /\ familyDef)
                Command _ (AssignProcess (ProcessAssign (family /\ processCode))) ->
                    theMap # Map.update ((map $ FD.forceAssign processCode) >>> Just) family
                _ -> theMap


loadDefinitions :: NdfFile -> Array (Maybe Source /\ FamilyDef) -- a) TODO: Use Order, b) FIXME: gets auto-sorted by family name
loadDefinitions ndfFile =
    ndfFile
        # extractCommands
        # _normalizeCommands
        # definitionsFromCommands_
        # Array.sortWith (Tuple.fst >>> map _.lineIndex)
        # case loadOrder ndfFile of
                Just familiesOrder -> Array.sortUsing (Tuple.snd >>> FD.family) (Array.concat familiesOrder)
                Nothing -> identity -- Array.sortWith (Tuple.fst >>> map _.lineIndex)


loadOrder :: NdfFile -> Maybe FamiliesOrder
loadOrder = extractCommands >>> map Command.op >>> foldl mergeOrders Nothing
    where
        mergeOrders :: Maybe FamiliesOrder -> CommandOp -> Maybe FamiliesOrder
        mergeOrders mbMergedOrders =
            case _ of
                Order nextOrders ->
                    case mbMergedOrders of
                        Just mergedOrders -> Just $ mergedOrders <> nextOrders
                        Nothing -> Just nextOrders
                _ -> mbMergedOrders


documentationFor :: Id.FamilyR -> NdfFile -> Array String
documentationFor familyR = extractCommands >>> map Command.op >>> foldl extractDocs []
    where
        extractDocs docs (Documentation familyR' docLine) | familyR' == familyR = Array.snoc docs docLine
        extractDocs docs _  = docs


-- TODO: add `ToCode` implementation for `PureScript`? Maybe `ToCode` could generate several files?
codegen :: forall strepr chrepr. FCG.CodegenRepr strepr => FCG.CodegenRepr chrepr => Toolkit.Name -> FCG.Options strepr chrepr -> NdfFile -> Map MCG.FilePath MCG.FileContent
codegen tkName options = loadDefinitions >>> MCG.codegen tkName options