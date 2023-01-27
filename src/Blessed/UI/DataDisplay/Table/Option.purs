module Blessed.UI.DataDisplay.Table.Option where


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
    , fillCellBorders :: Boolean
    , style_header :: Array (FgBgOption ())
    , style_cell :: Array (FgBgOption ())
    | r
    )
type Options = Record (OptionsRow ())


type TableAttribute r e = C.Attribute (List.OptionsRow + OptionsRow + r) e


tableOption :: forall a r r' sym e. EncodeJson a => IsSymbol sym => R.Cons sym a r' (OptionsRow + r) => Proxy sym -> a -> TableAttribute r e
tableOption = C.option


rows :: forall r e. Array (Array String) -> TableAttribute ( rows :: Array (Array String) | r ) e
rows = tableOption (Proxy :: _ "rows")


pad :: forall r e. Int -> TableAttribute ( pad :: Int | r ) e
pad = tableOption (Proxy :: _ "pad")


noCellBorders :: forall r e. Boolean -> TableAttribute ( noCellBorders :: Boolean | r ) e
noCellBorders = tableOption (Proxy :: _ "noCellBorders")


fillCellBorders :: forall r e. Boolean -> TableAttribute ( fillCellBorders :: Boolean | r ) e
fillCellBorders = tableOption (Proxy :: _ "fillCellBorders")


style_header :: forall r e. Array (FgBgOption ()) -> TableAttribute ( style_header :: Array (FgBgOption ()) | r ) e
style_header = tableOption (Proxy :: _ "style_header")


style_cell :: forall r e. Array (FgBgOption ()) -> TableAttribute ( style_cell :: Array (FgBgOption ()) | r ) e
style_cell = tableOption (Proxy :: _ "style_cell")
