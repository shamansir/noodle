module Noodle.Text.NdfFile.Command.FromInput where

import Prelude

import Effect.Class (class MonadEffect)

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Either (Either(..))
import Data.Tuple.Nested ((/\), type (/\))
import Data.Bifunctor (bimap)

import Parsing (runParser) as P

import Noodle.Id (FamilyR, unsafeFamilyR, PatchR) as Id
import Noodle.Raw.Node (Node) as Raw
import Noodle.Raw.Node (_fromSignature) as RawNode
import Noodle.Toolkit (Toolkit)
import Noodle.Toolkit (class HoldsFamilies, isKnownFamily, spawnAnyRaw) as Toolkit
import Noodle.Fn.Signature (Signature, class PossiblyToSignature)
import Noodle.Repr.ValueInChannel (ValueInChannel)
import Noodle.Repr.HasFallback (class HasFallback, fallback)
import Noodle.Repr.Tagged (class ValueTagged) as VT
import Noodle.Text.NdfFile.FamilyDef.Codegen (class ParseableRepr)
import Noodle.Text.NdfFile.FamilyDef.Codegen (toRepr, toDefault) as CG
import Noodle.Text.NdfFile.FamilyDef.Parser as NdfFamilyParser
import Noodle.Text.NdfFile.Types (ChannelDef)
import Noodle.Text.NdfFile.Types (encodedTypeOf, encodedValueOf) as CD


data CommandResult strepr chrepr m
    = FromFamily Id.FamilyR (Raw.Node strepr chrepr m)
    | CustomNode (Signature chrepr chrepr) (Raw.Node strepr chrepr m)
    | CannotSpawn Id.FamilyR
    | UnknownCommand String

tryExecute
    :: forall tk fs strepr chrepr mo mi
     . MonadEffect mo
    => HasFallback strepr
    => HasFallback chrepr
    => ParseableRepr chrepr
    => VT.ValueTagged chrepr
    => Toolkit.HoldsFamilies strepr chrepr mi fs
    => PossiblyToSignature tk (ValueInChannel chrepr) (ValueInChannel chrepr) Id.FamilyR
    => Toolkit tk fs strepr chrepr mi
    -> String
    -> mo (CommandResult strepr chrepr mi)
tryExecute toolkit command =
    case Toolkit.isKnownFamily command toolkit of
        Just familyR -> do
            mbRawNode <- Toolkit.spawnAnyRaw familyR toolkit
            pure $ fromMaybe (CannotSpawn familyR) $ FromFamily familyR <$> mbRawNode
        Nothing ->
            case P.runParser command $ NdfFamilyParser.fnSignature "custom" of
                Right (stateDef /\ fn) -> do
                    rawNode <- RawNode._fromSignature (Id.unsafeFamilyR "custom") (fallback :: strepr) (convertFn fn) $ pure unit
                    pure $ CustomNode (convertFn fn) rawNode
                Left error ->
                    pure $ UnknownCommand command
    where
        defToRepr :: ChannelDef -> chrepr
        defToRepr chanDef =
            case CD.encodedTypeOf chanDef of
                Just encodedType ->
                    case CD.encodedValueOf chanDef of
                        Just encodedValue ->
                            CG.toRepr encodedType encodedValue # fromMaybe fallback
                        Nothing -> CG.toDefault encodedType
                Nothing -> fallback

        convertFn :: Signature ChannelDef ChannelDef -> Signature chrepr chrepr
        convertFn = bimap defToRepr defToRepr