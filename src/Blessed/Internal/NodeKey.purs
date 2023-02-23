module Blessed.Internal.NodeKey where

import Prelude

import Data.Newtype (class Newtype)
import Data.Symbol (class IsSymbol, reflectSymbol, reifySymbol)
import Data.Maybe (Maybe(..))
import Type.Proxy (Proxy(..))
import Data.Tuple (curry, uncurry)
import Data.Tuple.Nested ((/\), type (/\))
import Unsafe.Coerce (unsafeCoerce)


import Data.Argonaut.Encode (class EncodeJson)

import Blessed.Internal.BlessedSubj as K



data NodeKey (kind :: K.Subject) (symbol :: Symbol) = NodeKey (Maybe Int)
newtype RawNodeKey = RawNodeKey { subject :: K.Subject_, id :: String }


derive instance Newtype RawNodeKey _
derive newtype instance EncodeJson RawNodeKey


raw :: K.Subject_ -> String -> RawNodeKey
raw subject id = RawNodeKey { subject, id }


infixl 6 make as <^>
infixl 6 type NodeKey as <^>


make :: forall subj sym. K.IsSubject subj => IsSymbol sym => Proxy subj -> Proxy sym -> NodeKey subj sym
make _ _ = NodeKey Nothing


makeN :: forall subj sym. K.IsSubject subj => IsSymbol sym => Proxy subj -> (Proxy sym /\ Int) -> NodeKey subj sym
makeN _ ( _ /\ n) = NodeKey $ Just n


nk :: forall subj sym. NodeKey subj sym
nk = NodeKey Nothing


next :: forall subj sym. NodeKey subj sym -> NodeKey subj sym
next (NodeKey maybeN) = NodeKey $ nextN maybeN
    where
        nextN (Just n) = Just $ n + 1
        nextN Nothing = Just 0


-- TODO: join, two node keys concatenated in one


makeUnsafe :: forall subj sym. IsSymbol sym => K.IsSubject subj => Proxy subj -> String -> NodeKey subj sym
makeUnsafe subj s = unsafeCoerce $ reifySymbol s \sym -> unsafeCoerce $ make subj sym


rawify :: forall subj sym. K.IsSubject subj => IsSymbol sym => NodeKey subj sym -> RawNodeKey
rawify = uncurry raw <<< rawify'


rawify' :: forall subj sym. K.IsSubject subj => IsSymbol sym => NodeKey subj sym -> K.Subject_ /\ String
rawify' nodeKey = getSubject nodeKey /\ getId nodeKey


process :: K.Ext K.Process <^> ""
process = NodeKey Nothing


toString :: forall subj sym. K.IsSubject subj => IsSymbol sym => NodeKey subj sym -> String
toString nodeKey =
    case rawify' nodeKey of
        subj /\ id -> id <> ":" <> K.toString subj


-- reflectSymbol (Proxy :: _ sym) <> ":" <> K.toString (K.reflectSubject (Proxy :: _ subj))


getId :: forall subj sym. IsSymbol sym => NodeKey subj sym -> String
getId (NodeKey maybeN) = reflectSymbol (Proxy :: _ sym) <> rawNPostfix maybeN
    where
        rawNPostfix (Just n) = show n
        rawNPostfix Nothing = ""


getSubject :: forall subj sym. K.IsSubject subj => IsSymbol sym => NodeKey subj sym -> K.Subject_
getSubject _ = K.reflectSubject (Proxy :: _ subj)


-- FIXME: `Belongs`?
class (K.Extends parent subj, K.IsSubject parent, K.IsSubject subj, IsSymbol id) <= Respresents parent subj id
instance (K.Extends parent subj, K.IsSubject parent, K.IsSubject subj, IsSymbol id) => Respresents parent subj id