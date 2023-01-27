module Blessed.UI.Lists.ListTable.Option where

import Type.Row (type (+))
import Prim.Row as R
import Data.Argonaut.Encode (class EncodeJson)
import Type.Proxy (Proxy(..))
import Data.Symbol (class IsSymbol)


import Blessed.Core.FgBg (FgBgOption)

import Blessed.Internal.Core (Attribute, option) as C


import Blessed.UI.Lists.List.Event (Event)
import Blessed.UI.Lists.List.Option (OptionsRow) as List


type OptionsRow r =
    ( rows :: Array (Array String)
    , pad :: Int
    , noCellBorders :: Boolean
    , style_header :: Array (FgBgOption ())
    , style_cell :: Array (FgBgOption ())
    | r
    )
type Options = Record (OptionsRow ())


type ListTableAttribute r e = C.Attribute (List.OptionsRow + OptionsRow + r) e


ltOption :: forall a r r' sym e. EncodeJson a => IsSymbol sym => R.Cons sym a r' (OptionsRow + r) => Proxy sym -> a -> ListTableAttribute r e
ltOption = C.option


rows :: forall r e. Array (Array String) -> ListTableAttribute ( rows :: Array (Array String) | r ) e
rows = ltOption (Proxy :: _ "rows")


pad :: forall r e. Int -> ListTableAttribute ( pad :: Int | r ) e
pad = ltOption (Proxy :: _ "pad")


noCellBorders :: forall r e. Boolean -> ListTableAttribute ( noCellBorders :: Boolean | r ) e
noCellBorders = ltOption (Proxy :: _ "noCellBorders")


style_header :: forall r e. Array (FgBgOption ()) -> ListTableAttribute ( style_header :: Array (FgBgOption ()) | r ) e
style_header = ltOption (Proxy :: _ "style_header")


style_cell :: forall r e. Array (FgBgOption ()) -> ListTableAttribute ( style_cell :: Array (FgBgOption ()) | r ) e
style_cell = ltOption (Proxy :: _ "style_cell")
