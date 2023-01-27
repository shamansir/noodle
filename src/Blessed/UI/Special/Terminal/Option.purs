module Blessed.UI.Special.Terminal.Option where

import Prelude



import Type.Row (type (+))
import Prim.Row as R
import Data.Argonaut.Core (Json)
import Data.Argonaut.Encode (class EncodeJson)
import Type.Proxy (Proxy(..))
import Data.Symbol (class IsSymbol)


import Blessed.Core.FgBg (FgBgOption)
import Blessed.Core.Cursor (Cursor)
import Blessed.Core.Terminal (Terminal)

import Blessed.Internal.Core (Attribute, option) as C


import Blessed.UI.Lists.List.Event (Event)
import Blessed.UI.Lists.List.Option (OptionsRow) as List


type OptionsRow r =
    ( handler :: Unit -- TODO
    , shell :: String
    , args :: Array Json
    , cursor :: Cursor
    , terminal :: Terminal
    | r
    )
type Options = Record (OptionsRow ())


type TerminalAttribute r e = C.Attribute (List.OptionsRow + OptionsRow + r) e


terminalOption :: forall a r r' sym e. EncodeJson a => IsSymbol sym => R.Cons sym a r' (OptionsRow + r) => Proxy sym -> a -> TerminalAttribute r e
terminalOption = C.option


handler :: forall r e. Unit -> TerminalAttribute ( handler :: Unit | r ) e
handler = terminalOption (Proxy :: _ "handler")


shell :: forall r e. String -> TerminalAttribute ( shell :: String | r ) e
shell = terminalOption (Proxy :: _ "shell")


args :: forall r e. Array Json -> TerminalAttribute ( args :: Array Json | r ) e
args = terminalOption (Proxy :: _ "args")


cursor :: forall r e. Cursor -> TerminalAttribute ( cursor :: Cursor | r ) e
cursor = terminalOption (Proxy :: _ "cursor")


terminal :: forall r e. Terminal -> TerminalAttribute ( terminal :: Terminal | r ) e
terminal = terminalOption (Proxy :: _ "terminal")
