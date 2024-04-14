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
import Data.Text.Format (Tag(..), Format(..), Align(..), Timing(..), Term(..), Url(..), MacroCode(..), bulletPrefix)
import Data.Text.Output (OutputKind, class Renderer, layout, Support)
import Data.Text.Output (Support(..), perform) as S
import Data.Text.Doc (Doc, (<+>))
import Data.Text.Doc as D
import Data.DateTime (DateTime(..)) as DT
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
        Format format _ ->
            case format of
                Fg _ -> S.Full
                Bg _ -> S.Full
                Bold -> S.Full
                Underline -> S.Full
                Blink -> S.Full
                Inverse -> S.Full
                Invisible -> S.Full
                Define _ -> S.Text
                Link _ -> S.Partly
                Image _ -> S.Text
                LinkTo _ -> S.Partly
                Comment -> S.Text
                _ -> S.Text -- TODO
        Split _ _ -> S.Partly
        Align _ _ -> S.Full
        Pair _ _ -> S.Full
        Para _ -> S.Full
        Nest _ _ -> S.Full
        Join _ _ -> S.Full
        Timing _ -> S.Partly
        List _ _ _ -> S.Text
        Table _ -> S.None
        Hr -> S.Text
        Property _ _ -> S.Text
        Macro _ -> S.Text
        Task _ _ -> S.None -- FIXME

    layout :: Proxy Blessed -> Tag -> Doc
    layout = const $ case _ of
        Empty -> D.nil
        Plain str -> D.text str
        Newline -> D.break
        Format format tag ->
            case format of
                Fg (E.Left colorStr) -> wrap (colorStr <> "-fg") tag
                Fg (E.Right color) -> wrap (Color.toHexString color <> "-fg") tag
                Bg (E.Left colorStr) -> wrap (colorStr <> "-bg") tag
                Bg (E.Right color) -> wrap (Color.toHexString color <> "-bg") tag
                Bold -> wrap "bold" tag
                Underline -> wrap "underline" tag
                Blink -> wrap "blink" tag
                Inverse -> wrap "inverse" tag
                Invisible -> wrap "invisible" tag
                Define (Term dt) -> let dd = tag in D.break <> layout blessed dt <+> D.text "::" <+> layout blessed dd <> D.break
                LinkTo ftn -> layout blessed tag <+> D.bracket "[" (D.text $ show $ unwrap ftn) "]"
                Link (Url url) -> let title = tag in layout blessed title <+> D.bracket "(" (D.text url) ")"
                Image (Url url) -> let title = tag in layout blessed title <+> D.bracket "(" (D.text url) ")"
                Comment -> D.break <> layout blessed tag
                _ -> layout blessed tag -- other format's are not supported
        Split tagA tagB -> layout blessed tagA <> D.text "{|}" <> layout blessed tagB
        Align Left tag -> wrap "left" tag
        Align Right tag -> wrap "right" tag
        Align Center tag -> wrap "center" tag
        Pair tagA tagB -> layout blessed tagA <> layout blessed tagB
        Para tags -> D.stack $ layout blessed <$> tags
        Nest i tags -> D.nest' (unwrap i) $ layout blessed <$> tags
        Join tag tags -> D.folddoc (<>) $ layout blessed <$> Array.intersperse tag tags
        Timing timing ->
            case timing of
                Date date -> case FDT.formatDateTime "YYYY-DD-MM" $ DT.DateTime date zeroTime of
                        E.Right success -> D.text success
                        E.Left _ -> D.text "<Date?>"
                Time time -> case FDT.formatDateTime "HH:mm:ss" $ DT.DateTime zeroDate time of
                        E.Right success -> D.text success
                        E.Left _ -> D.text "<Time?>"
                DateTime datetime -> case FDT.formatDateTime "YYYY-DD-MM HH:mm:ss" datetime of
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
        Hr -> D.text "----------"
        Property prop value -> D.break <> D.text (unwrap prop) <+> D.text "::" <+> D.text (unwrap value) <> D.break
        Macro (MacroCode str) -> D.text str
        Task status tag -> D.nil -- FIXME
        where
            b bullet (index /\ doc) = bulletPrefix index bullet /\ doc
            zeroDate = Dt.canonicalDate (fromMaybe bottom $ toEnum 0) bottom bottom
            zeroTime = Tm.Time bottom bottom bottom bottom
            wrap cmd tag = D.bracket "{" (D.text cmd) "}" <> layout blessed tag <> D.bracket "{/" (D.text cmd) "}"


render :: Tag -> String
render = layout blessed >>> D.render { break : D.Space, indent : D.Empty }
-- render = S.perform blessed { break : D.Space, indent : D.Empty }