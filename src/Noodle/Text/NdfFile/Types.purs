module Noodle.Text.NdfFile.Types where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Newtype (class Newtype)

import Noodle.Text.FromCode (Source) as FC
import Noodle.Fn.ToFn (Fn)
import Noodle.Fn.ToFn (name) as Fn
import Noodle.Text.NdfFile.NodeDef.ProcessCode (ProcessCode)


type NodeDefRec =
    { group :: FamilyGroup
    , fn :: Fn ChannelDef ChannelDef
    , state :: StateDef
    , process :: ProcessCode
    , source :: Maybe FC.Source -- TODO: add source file and line in the source file
    }


newtype FamilyGroup = FamilyGroup String -- TODO: rename to FamilyGropId, move to Noodle.Id
newtype NodeFamily = NodeFamily String -- TODO: rename to NodeFamilyId, move to Noodle.Id
newtype NodeId = NodeId String
newtype Coord = Coord Int
newtype InletId = InletId (Either String Int)
newtype OutletId = OutletId (Either String Int)
newtype EncodedType = EncodedType String
newtype EncodedValue = EncodedValue String
newtype ChannelName = ChannelName String
type DefaultAndType = { mbType :: Maybe EncodedType, mbDefault :: Maybe EncodedValue }
newtype ChannelDef = ChannelDef DefaultAndType
newtype StateDef = StateDef DefaultAndType


derive instance Newtype FamilyGroup _
derive instance Newtype NodeFamily _
derive instance Newtype NodeId _
derive instance Newtype Coord _
derive instance Newtype InletId _
derive instance Newtype OutletId _
derive instance Newtype EncodedType _
derive instance Newtype EncodedValue _
derive instance Newtype ChannelName _
derive instance Newtype ChannelDef _
derive instance Newtype StateDef _


derive newtype instance Eq FamilyGroup
derive newtype instance Eq NodeFamily
derive newtype instance Eq NodeId
derive newtype instance Eq Coord
derive newtype instance Eq InletId
derive newtype instance Eq OutletId
derive newtype instance Eq EncodedType
derive newtype instance Eq EncodedValue
derive newtype instance Eq ChannelName
derive newtype instance Eq ChannelDef
derive newtype instance Eq StateDef

derive newtype instance Ord FamilyGroup
derive newtype instance Ord NodeFamily



family :: String -> NodeFamily
family = NodeFamily


nodeId :: String -> NodeId
nodeId = NodeId


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


familyOf :: NodeDefRec -> NodeFamily
familyOf = _.fn >>> Fn.name >>> NodeFamily