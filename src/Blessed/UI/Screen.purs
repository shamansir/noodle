module Blessed.UI.Screen where

import Prelude


import Type.Row (type (+))
import Prim.Row (class Union, class Nub)
import Record as Record
import Prim.Row as R
import Data.Argonaut.Encode (class EncodeJson)
import Data.Codec.Argonaut as CA
import Type.Proxy (Proxy(..))
import Data.Symbol (class IsSymbol)
import Data.Maybe (Maybe(..))


import Blessed.Core.Key (Key)
import Blessed.Internal.Command (Command, call, arg) as C
import Blessed.Internal.Core (Attribute, property, Node, NodeAnd, node, nodeAnd, class Events, CoreEvent(..), handler, Handler) as C
import Blessed.Internal.BlessedOp (BlessedOp)
import Blessed.Internal.JsApi (NodeId) as C
import Blessed.Internal.JsApi (Kind(..)) as Kind
import Blessed.Internal.BlessedOp (perform) as Op
import Blessed.UI.Box as Box


import Data.Symbol (reflectSymbol, class IsSymbol)


data Event
    = Init
    | Key (Array Key)
    | Other


instance events :: C.Events Event where
    initial = Init
    convert Init = "init"
    convert (Key key) = "key"
    convert Other = "?"
    toCore _ = C.CoreEvent
    fromCore _ = Nothing


type OptionsRow :: Row Type -> Row Type
type OptionsRow r =
    ( title :: String
    , smartCSR :: Boolean
    | Box.OptionsRow + r
    )
type OptionsU = OptionsRow ()
type Options = Record (OptionsU)


{- default :: Options
default =
    Record.merge
        { title : ""
        , smartCSR : false
        }
        Box.default -}


{- define ∷ forall (r ∷ Row Type)
    . Union r (OptionsRow ()) (OptionsRow ())
    ⇒ Nub r ((OptionsRow ()))
    ⇒ Record r → Options
define rec =
    Record.merge rec default -}


type ScreenAttribute r = C.Attribute (OptionsRow + r) Event
type ScreenHandler r = C.Handler r Event


screenProperty :: forall a r r' sym. EncodeJson a => IsSymbol sym => R.Cons sym a r' r => Proxy sym -> a -> ScreenAttribute r
screenProperty = C.property


screenHandler :: forall r. Event -> ScreenHandler r
screenHandler = C.handler


--draggable :: forall r e. Boolean -> Prop ( draggable :: Boolean | r ) e

title ∷ forall r. String -> ScreenAttribute ( title :: String | r )
title = screenProperty ( Proxy :: Proxy "title" )


smartCSR ∷ forall r. Boolean -> ScreenAttribute ( smartCSR :: Boolean | r )
smartCSR = screenProperty ( Proxy :: Proxy "smartCSR" )


key :: forall r. Array Key -> ScreenHandler r
key = screenHandler <<< Key


screen :: forall r. String -> C.Node ( OptionsRow + r ) Event
screen name = C.node Kind.Screen name


screenAnd :: forall r. String -> C.NodeAnd ( OptionsRow + r ) Event
screenAnd name = C.nodeAnd Kind.Screen name


render :: forall m. C.NodeId -> BlessedOp m
render nodeId =
    Op.perform nodeId $ C.call "render" [ ]
