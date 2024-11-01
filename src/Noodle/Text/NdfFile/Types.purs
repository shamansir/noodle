module Noodle.Text.NdfFile.Types where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Newtype (class Newtype)

import Noodle.Id (GroupR, FamilyR, unsafeFamilyR)
import Noodle.Text.FromCode (Source) as FC
import Noodle.Fn.ToFn (Fn)
import Noodle.Fn.ToFn (name) as Fn
import Noodle.Text.NdfFile.FamilyDef.ProcessCode (ProcessCode)


type FamilyDefRec =
    { group :: GroupR
    , fn :: Fn ChannelDef ChannelDef
    , state :: StateDef
    , process :: ProcessCode
    , source :: Maybe { lineIndex :: Int, line :: FC.Source } -- TODO: add source file
    }


newtype NodeInstanceId = NodeInstanceId String -- node ID which is local to a single NDF file (and so recognisable by commands in this file)
newtype Coord = Coord Int
newtype InletId = InletId (Either String Int)
newtype OutletId = OutletId (Either String Int)
newtype EncodedType = EncodedType String
newtype EncodedValue = EncodedValue String
newtype ChannelName = ChannelName String
type DefaultAndType = { mbType :: Maybe EncodedType, mbDefault :: Maybe EncodedValue }
newtype ChannelDef = ChannelDef DefaultAndType
newtype StateDef = StateDef DefaultAndType


derive instance Newtype NodeInstanceId _
derive instance Newtype Coord _
derive instance Newtype InletId _
derive instance Newtype OutletId _
derive instance Newtype EncodedType _
derive instance Newtype EncodedValue _
derive instance Newtype ChannelName _
derive instance Newtype ChannelDef _
derive instance Newtype StateDef _


derive newtype instance Eq NodeInstanceId
derive newtype instance Eq Coord
derive newtype instance Eq InletId
derive newtype instance Eq OutletId
derive newtype instance Eq EncodedType
derive newtype instance Eq EncodedValue
derive newtype instance Eq ChannelName
derive newtype instance Eq ChannelDef
derive newtype instance Eq StateDef


nodeInstanceId :: String -> NodeInstanceId
nodeInstanceId = NodeInstanceId


coord :: Int -> Coord
coord = Coord


inletIndex :: Int -> InletId
inletIndex = InletId <<< Right


inletAlias :: String -> InletId
inletAlias = InletId <<< Left


outletIndex :: Int -> OutletId
outletIndex = OutletId <<< Right


outletAlias :: String -> OutletId
outletAlias = OutletId <<< Left


encodedValue :: String -> EncodedValue
encodedValue = EncodedValue


emptyDefAndType :: DefaultAndType
emptyDefAndType = { mbType : Nothing, mbDefault : Nothing }


emptyStateDef :: StateDef
emptyStateDef = StateDef emptyDefAndType


emptyChannelDef :: ChannelDef
emptyChannelDef = ChannelDef emptyDefAndType


familyOf :: FamilyDefRec -> FamilyR
familyOf = _.fn >>> Fn.name >>> unsafeFamilyR