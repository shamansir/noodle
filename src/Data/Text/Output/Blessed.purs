module Data.Text.Output.Blessed where

import Prelude

import Color as Color

import Type.Proxy (Proxy(..))

import Data.Array (intersperse, mapWithIndex) as Array
import Data.Newtype (unwrap)
import Data.Maybe (fromMaybe)
import Data.Enum (toEnum)
import Data.Either (Either(..)) as E
import Data.Tuple (curry, uncurry)
import Data.Tuple.Nested ((/\), type (/\))
import Data.String (joinWith) as String
import Data.Text.Format (Tag(..), Format(..), Align(..), bulletSym)
import Data.Text.Output (OutputKind, class Renderer, layout, Support)
import Data.Text.Output (Support(..), perform) as S
import Data.Text.Doc (Doc, (<+>))
import Data.Text.Doc as D
import Data.DateTime (DateTime(..))
import Data.DateTime as DateTime
import Data.Date (Date(..), canonicalDate) as Dt
import Data.Date as Date
import Data.Time (Time(..)) as Tm
import Data.Time.Component (Hour(..), Minute(..), Millisecond(..)) as Tm
import Data.Time as Time
import Data.Formatter.DateTime as FDT

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
        Date _ -> S.Partly
        Time _ -> S.Partly
        List _ _ _ -> S.Text
        Table _ -> S.None
        Link _ _ -> S.Partly
        Definition _ _ -> S.Text
        LinkTo _ _ -> S.Text
        Hr -> S.Text
        Image _ _ -> S.Text
        Property _ _ -> S.Text
        Comment _ -> S.Text
        Macro _ -> S.Text
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
        Date date -> case FDT.formatDateTime "YYYY-DD-MM" $ DateTime date zeroTime of
                        E.Right success -> D.text success
                        E.Left _ -> D.text "<Date?>"
        Time time -> case FDT.formatDateTime "HH-DD-MM" $ DateTime zeroDate time of
                        E.Right success -> D.text success
                        E.Left _ -> D.text "<Time?>"
        List bullet Empty items ->
            D.nest' 1 $ uncurry D.mark <$> b bullet <$> Array.mapWithIndex (/\) (layout blessed <$> items)
        List bullet start items ->
            D.nest' 0 $ -- FIXME: support levels from `Nest`
                [ layout blessed start
                , D.nest' 1 $ uncurry D.mark <$> b bullet <$> Array.mapWithIndex (/\) (layout blessed <$> items)
                ]
        Table items -> D.nil
        Link text url -> layout blessed text <+> D.bracket "(" (D.text url) ")"
        Definition dt dd -> D.break <> layout blessed dt <+> D.text "::" <+> layout blessed dd <> D.break
        LinkTo tag ftn -> layout blessed tag <+> D.bracket "[" (D.text $ show $ unwrap ftn) "]"
        Hr -> D.text "----------"
        Image tag url -> layout blessed tag <+> D.bracket "(" (D.text url) ")"
        Property prop value -> D.break <> D.text prop <+> D.text "::" <+> D.text value <> D.break
        Comment tag -> D.break <> D.text tag
        Macro str -> D.text str
        Task status tag -> D.nil -- FIXME
        where
            b bullet (index /\ doc) = bulletSym index bullet /\ doc
            zeroDate = Dt.canonicalDate (fromMaybe bottom $ toEnum 0) bottom bottom
            zeroTime = Tm.Time bottom bottom bottom bottom
            wrap cmd tag = D.bracket "{" (D.text cmd) "}" <> layout blessed tag <> D.bracket "{/" (D.text cmd) "}"


render :: Tag -> String
render = layout blessed >>> D.render { break : D.Space, indent : D.Empty }
-- render = S.perform blessed { break : D.Space, indent : D.Empty }