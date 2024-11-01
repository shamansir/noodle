module Noodle.Text.NdfFile.Command where

import Prelude

import Data.String as String
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\), type (/\))

import Data.Text.Format as T

import Type.Proxy (Proxy)

import Noodle.Id (FamilyR)
import Noodle.Id (family) as Id
import Noodle.Text.ToCode (class ToCode, class ToTaggedCode, toCode, toTaggedCode)
import Noodle.Text.Code.Target (NDF, ndf)
import Noodle.Ui.Cli.Tagging as F
import Noodle.Text.NdfFile.Types
import Noodle.Text.NdfFile.FamilyDef (FamilyDef, ProcessAssign)
import Noodle.Text.NdfFile.FamilyDef (ndfLinesCount, processAssignNdfLinesCount) as FD
import Noodle.Text.NdfFile.Command.Op (CommandOp)
import Noodle.Text.NdfFile.Command.Op as Op



type FamiliesOrder = Op.FamiliesOrder


type Source = { line :: String, lineIndex :: Int }


data Command =
    Command
        (Maybe Source)
        CommandOp


derive instance Eq Command


instance ToCode NDF opts Command where
    toCode :: Proxy NDF -> opts -> Command -> String
    toCode pndf opts = op >>> toCode pndf opts


instance ToTaggedCode NDF opts Command where
    toTaggedCode :: Proxy NDF -> opts -> Command -> T.Tag
    toTaggedCode pndf opts = op >>> toTaggedCode pndf opts


-- instance ToCode NDF (Array Command) where
commandsToNdf :: Array Command -> String
commandsToNdf cmds = String.joinWith "\n" $ toCode ndf unit <$> (optimize cmds)


-- instance ToCode NDF (Array Command) where
commandsToTaggedNdf :: Array Command -> T.Tag
commandsToTaggedNdf cmds = T.joinWith T.nl $ toTaggedCode ndf unit <$> (optimize cmds)


optimize :: Array Command -> Array Command
optimize = identity -- TODO : remove duplicating commands or the ones that can be merged into a single one


ndfLinesCount :: Command -> Int
ndfLinesCount = op >>> Op.ndfLinesCount -- FIXME: use `Source`!


op :: Command -> CommandOp
op (Command _ op) = op


priority :: Command -> Int
priority = op >>> Op.priority


reviewOrder_ :: Op.FamiliesOrder -> Op.FamiliesOrder
reviewOrder_ = Op.reviewOrder_