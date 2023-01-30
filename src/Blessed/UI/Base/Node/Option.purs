module Blessed.UI.Base.Node.Option where


import Prim.Row as R
import Type.Proxy (Proxy(..))
import Type.Row (type (+))
import Data.Symbol (class IsSymbol)

import Data.Argonaut.Encode (class EncodeJson)

import Blessed.Internal.BlessedSubj (Subject, Node)
import Blessed.Internal.Core (Attribute, option) as C
import Blessed.Internal.NodeKey (class Respresents)



type OptionsRow :: Row Type -> Row Type
type OptionsRow r =
    ( screen :: String
    -- , parent :: Maybe String
    -- , children :: Array String
    )
type OptionsU = OptionsRow ()
type Options = Record (OptionsU)



type NodeAttribute subj id r e = C.Attribute subj id (OptionsRow + r) e


nodeOption
    :: forall subj id a r r' sym e
     . Respresents Node subj id
    => EncodeJson a
    => IsSymbol sym
    => R.Cons sym a r' (OptionsRow + r)
    => Proxy sym -> a -> NodeAttribute subj id r e
nodeOption = C.option


screen
    :: forall (subj :: Subject) (id :: Symbol) r e
     . Respresents Node subj id
    => String -> NodeAttribute subj id ( screen :: String | r ) e
screen = nodeOption (Proxy :: _ "screen")
