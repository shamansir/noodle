module Blessed.Internal.NodeKey where

import Prelude (identity, ($), (<>))

import Data.Newtype (class Newtype)
import Data.Symbol (class IsSymbol, reflectSymbol, reifySymbol)
import Data.Maybe (Maybe(..))
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)


import Data.Argonaut.Encode (class EncodeJson)

import Blessed.Internal.BlessedSubj as K



data NodeKey (kind :: K.Subject) (symbol :: Symbol) = NodeKey
newtype RawNodeKey = RawNodeKey { subject :: K.Subject_, id :: String }


derive instance Newtype RawNodeKey _
derive newtype instance EncodeJson RawNodeKey


raw :: K.Subject_ -> String -> RawNodeKey
raw subject id = RawNodeKey { subject, id }


infixl 6 make as <^>
infixl 6 type NodeKey as <^>


make :: forall subj sym. K.IsSubject subj => IsSymbol sym => Proxy subj -> Proxy sym -> NodeKey subj sym
make _ _ = NodeKey


rawify :: forall subj sym. K.IsSubject subj => IsSymbol sym => NodeKey subj sym -> RawNodeKey
rawify NodeKey = raw (K.reflectSubject (Proxy :: _ subj)) (reflectSymbol (Proxy :: _ sym))


-- reify' :: K.Subject_ -> String -> NodeId subj sym
-- reify' =


-- test :: NodeId K.Screen "foo"
-- test = NodeId


-- test2 :: K.Screen <^> "foo"
-- test2 = NodeId


process :: K.Ext K.Process <^> ""
process = NodeKey


toString :: forall subj sym. K.IsSubject subj => IsSymbol sym => NodeKey subj sym -> String
toString _ = reflectSymbol (Proxy :: _ sym) <> ":" <> K.toString (K.reflectSubject (Proxy :: _ subj))