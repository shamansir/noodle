module Data.Text.Output.Blessed where

import Prelude

import Color as Color

import Type.Proxy (Proxy(..))

import Data.Array (intersperse) as Array
import Data.Newtype (unwrap)
import Data.Either (Either(..)) as E
import Data.String (joinWith) as String
import Data.Text.Format (Tag(..), Format(..), Align(..))
import Data.Text.Output (OutputKind, class Renderer, layout, Support)
import Data.Text.Output (Support(..), perform) as S
import Data.Text.Doc (Doc)
import Data.Text.Doc as D


foreign import data Blessed :: OutputKind


blessed = Proxy :: _ Blessed


layoutB = layout blessed


instance Renderer Blessed where

    supported :: Proxy Blessed -> Tag -> Support
    supported _ = case _ of
        Empty -> S.Full
        Plain _ -> S.Full
        Newline -> S.Full
        Format (Fg _) _ -> S.Full
        Format (Bg _) _ -> S.Full
        Format Bold _ -> S.Full
        Format Underline _ -> S.Full
        Format Blink _ -> S.Full
        Format Inverse _ -> S.Full
        Format Invisible _ -> S.Full
        Format _ _ -> S.Text
        Split _ _ -> S.Partly
        Align _ _ -> S.Full
        Pair _ _ -> S.Full
        Para _ -> S.Full
        Nest _ _ -> S.Full
        Join _ _ -> S.Full
        Date _ -> S.None -- FIXME
        Time _ -> S.None -- FIXME
        List _ _ _ -> S.None -- FIXME
        Table _ -> S.None
        Link _ _ -> S.None -- FIXME
        Definition _ _ -> S.None -- FIXME
        LinkTo _ _ -> S.None -- FIXME
        Hr -> S.None -- FIXME
        Image _ _ -> S.None -- FIXME
        Property _ _ -> S.None -- FIXME
        Comment _ -> S.None -- FIXME
        Macro _ -> S.None -- FIXME
        Task _ _ -> S.None -- FIXME

    layout :: Proxy Blessed -> Tag -> Doc
    layout = const $ case _ of
        Empty -> D.nil
        Plain str -> D.text str
        Newline -> D.break
        Format (Fg (E.Left colorStr)) tag -> wrap (colorStr <> "-fg") tag
        Format (Fg (E.Right color)) tag -> wrap (Color.toHexString color <> "-fg") tag
        Format (Bg (E.Left colorStr)) tag -> wrap (colorStr <> "-bg") tag
        Format (Bg (E.Right color)) tag -> wrap (Color.toHexString color <> "-bg") tag
        Format Bold tag -> wrap "bold" tag
        Format Underline tag -> wrap "underline" tag
        Format Blink tag -> wrap "blink" tag
        Format Inverse tag -> wrap "inverse" tag
        Format Invisible tag -> wrap "invisible" tag
        Format _ tag -> layout blessed tag -- other format's are not supported
        Split tagA tagB -> layout blessed tagA <> D.text "{|}" <> layout blessed tagB
        Align Left tag -> wrap "left" tag
        Align Right tag -> wrap "right" tag
        Align Center tag -> wrap "center" tag
        Pair tagA tagB -> layout blessed tagA <> layout blessed tagB
        Para tags -> D.stack $ layout blessed <$> tags
        Nest i tags -> D.nest' (unwrap i) $ layout blessed <$> tags
        Join tag tags -> D.folddoc (<>) $ layout blessed <$> Array.intersperse tag tags
        Date date -> D.nil -- FIXME
        Time time -> D.nil -- FIXME
        List bullet start items -> D.nil -- FIXME
        Table items -> D.nil
        Link text url -> D.nil -- FIXME
        Definition dt dd -> D.nil -- FIXME
        LinkTo tag ftn -> D.nil -- FIXME
        Hr -> D.nil -- FIXME
        Image tag url -> D.nil -- FIXME
        Property prop value -> D.nil -- FIXME
        Comment tag -> D.nil -- FIXME
        Macro tag -> D.nil -- FIXME
        Task status tag -> D.nil -- FIXME
        where
            wrap cmd tag = D.bracket "{" (D.text cmd) "}" <> layout blessed tag <> D.bracket "{/" (D.text cmd) "}"


render :: Tag -> String
render = layout blessed >>> D.render { break : D.Space, indent : D.Empty }
-- render = S.perform blessed { break : D.Space, indent : D.Empty }