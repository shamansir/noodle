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


type TableAttribute r = C.Attribute (List.OptionsRow + OptionsRow + r) Event


tableOption :: forall a r r' sym. EncodeJson a => IsSymbol sym => R.Cons sym a r' (OptionsRow + r) => Proxy sym -> a -> TableAttribute r
tableOption = C.option


rows :: forall r. Array (Array String) -> TableAttribute ( rows :: Array (Array String) | r )
rows = tableOption (Proxy :: _ "rows")


pad :: forall r. Int -> TableAttribute ( pad :: Int | r )
pad = tableOption (Proxy :: _ "pad")


noCellBorders :: forall r. Boolean -> TableAttribute ( noCellBorders :: Boolean | r )
noCellBorders = tableOption (Proxy :: _ "noCellBorders")


fillCellBorders :: forall r. Boolean -> TableAttribute ( fillCellBorders :: Boolean | r )
fillCellBorders = tableOption (Proxy :: _ "fillCellBorders")


style_header :: forall r. Array (FgBgOption ()) -> TableAttribute ( style_header :: Array (FgBgOption ()) | r )
style_header = tableOption (Proxy :: _ "style_header")


style_cell :: forall r. Array (FgBgOption ()) -> TableAttribute ( style_cell :: Array (FgBgOption ()) | r )
style_cell = tableOption (Proxy :: _ "style_cell")
