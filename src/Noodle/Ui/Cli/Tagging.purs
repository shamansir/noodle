module Noodle.Ui.Cli.Tagging where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Symbol (class IsSymbol)
-- import Data.SProxy (reflect)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Array (foldl)

import Noodle.Id as Id


import Data.Text.Format (Tag)
import Data.Text.Format as T

import Noodle.Ui.Cli.Palette as Palette
import Noodle.Ui.Cli.Palette.Item (crepr) as C
import Noodle.Ui.Cli.Palette.Item (Item, fullInfo) as Palette
import Noodle.Ui.Cli.Palette.Mark (class Mark, mark)
import Noodle.Ui.Cli.Palette.Set.X11 as X11
import Noodle.Ui.Cli.Palette.Set.Pico8 as Pico

import Noodle.Ui.Cli.Tagging.Target (class StatusLineInfo, class ShortChannelLabel)
import Noodle.Ui.Cli.Tagging.Target (shortLabel, statusLine, docs) as Info


inlet :: forall i repr. IsSymbol i => Mark repr => ShortChannelLabel repr => Int -> Id.Inlet i -> Maybe repr -> Tag
inlet idx inletId = inlet' idx $ Id.inletR inletId


inlet' :: forall repr. Mark repr => ShortChannelLabel repr => Int -> Id.InletR -> Maybe repr -> Tag
inlet' idx inletId (Just repr) =
    T.fgcs (mark repr) $ Info.shortLabel repr -- "⋱" <> show idx <> "⋰" <> Info.short repr
inlet' idx inletId Nothing = T.s "⋱" <> (T.s $ show idx) <> T.s "⋰"


inletInfoBox :: forall i. IsSymbol i => Id.Inlet i -> Tag
inletInfoBox inletId =
    T.fgcs (C.crepr Palette.inletId) $ Id.inletName inletId


inletStatusLine :: forall f i repr. IsSymbol i => IsSymbol f => Mark repr => StatusLineInfo repr => Id.Family f -> Int -> Id.Inlet i -> Maybe repr -> Tag
inletStatusLine family idx inletId = inletStatusLine' (Id.familyR family) idx $ Id.inletR inletId


inletStatusLine' :: forall repr. Mark repr => StatusLineInfo repr => Id.FamilyR -> Int -> Id.InletR -> Maybe repr -> Tag
inletStatusLine' (Id.FamilyR { family }) idx inletId (Just repr) =
    -- TODO: show node id and group as well
    (T.fgcs (C.crepr Palette.familyName) family) <> T.s " " <> (T.fgcs (C.crepr Palette.inletId) $ Id.inletRName inletId) <> T.s " " <> (T.fgcs (mark repr) $ Info.statusLine repr) -- "⋱" <> show idx <> "⋰" <> Info.short repr
inletStatusLine' familyR idx inletId Nothing =
    T.s "⋱" <> (T.s $ show idx) <> T.s "⋰"


outlet ::forall o repr. IsSymbol o => Mark repr => ShortChannelLabel repr => Int -> Id.Outlet o -> Maybe repr -> Tag
outlet idx outletId = outlet' idx $ Id.outletR outletId


outlet' :: forall repr. Mark repr => ShortChannelLabel repr => Int -> Id.OutletR -> Maybe repr -> Tag
outlet' idx outletId (Just repr) =
    T.fgcs (mark repr) $ Info.shortLabel repr -- "⋱" <> show idx <> "⋰" <> Info.short repr
    -- Info.short repr -- "⋰" <> show idx <> "⋱" <> Info.short repr
outlet' idx outletId Nothing = T.s "⋰" <> (T.s $ show idx) <> T.s "⋱"


outletInfoBox :: forall o. IsSymbol o => Id.Outlet o -> Tag
outletInfoBox outletId =
    T.fgcs (C.crepr Palette.outletId) $ Id.outletName outletId


outletStatusLine ::forall f o repr. IsSymbol f => IsSymbol o => Mark repr => StatusLineInfo repr => Id.Family f -> Int -> Id.Outlet o -> Maybe repr -> Tag
outletStatusLine family idx outletId = outletStatusLine' (Id.familyR family) idx $ Id.outletR outletId


outletStatusLine' :: forall repr. Mark repr => StatusLineInfo repr => Id.FamilyR -> Int -> Id.OutletR -> Maybe repr -> Tag
outletStatusLine' (Id.FamilyR { family }) idx outletId (Just repr) =
    -- TODO: show group as well
    (T.fgcs (C.crepr Palette.familyName) family) <> T.s " " <> (T.fgcs (C.crepr Palette.outletId) $ Id.outletRName outletId) <> T.s " " <> (T.fgcs (mark repr) $ Info.statusLine repr) -- "⋱" <> show idx <> "⋰" <> Info.short repr
    --T.fgcs (mark repr) $ Info.full repr -- "⋱" <> show idx <> "⋰" <> Info.short repr
    -- Info.short repr -- "⋰" <> show idx <> "⋱" <> Info.short repr
outletStatusLine' familyR idx outletId Nothing = T.s "⋰" <> (T.s $ show idx) <> T.s "⋱"


{- TODO
nodeLabel :: forall f. IsSymbol f => Id.Family f -> Tag
nodeLabel family =
    let color = mark $ Hydra.toGroup family
    in T.bgc (C.crepr Palette.nodeBg) $ T.fgc color $ T.s $ Id.reflectFamily family


nodeMouseOver :: forall f. IsSymbol f => Id.Family f -> Tag
nodeMouseOver =
    familyShortInfo


familyMouseOver :: forall f. IsSymbol f => Id.Family f -> Tag
familyMouseOver =
    familyShortInfo

-}

{- T.fgcs (C.crepr Palette.familyName) (reflect family)
    <> T.s " ==== "
    <> -}


removeButtonOut ∷ Tag
removeButtonOut =
    T.fgcs (C.crepr Pico.blue) "⨯"


removeButtonOver ∷ Tag
removeButtonOver =
    T.fgcs (C.crepr Pico.red) "⨯" -- "╳"


removeInfoBox ∷ Tag
removeInfoBox =
    T.fgcs (C.crepr Pico.red) "remove"


removeStatusLine :: forall f. IsSymbol f => Id.Family f -> Tag
removeStatusLine family =
    T.fgcs (C.crepr Pico.red) "remove" <> T.s " " <> (T.fgcs (C.crepr Palette.familyName) $ Id.family $ Id.familyR family)


{- TODO
libraryItem :: Id.FamilyR -> Tag
libraryItem familyR =
    let color = mark $ Hydra.toGroupR familyR
    in T.fgc color $ T.s $ Id.reflectFamilyR familyR


glslFnItem :: H.GlslFn -> Tag
glslFnItem (H.GlslFn (kind /\ _ /\ glslFn)) =
    T.fgc (C.crepr Pico.blue) $ T.s $ HFn.name glslFn
    -}

    {-
    let color = mark $ Hydra.toGroupR familyR
    in T.fgc color $ T.s $ Id.reflectFamilyR familyR
    -}

paletteItem :: Palette.Item -> Tag
paletteItem item =
    T.bg item.repr (T.s "      ") <> T.s " " <> T.fg item.repr (T.s $ Palette.fullInfo item)


-- Commands

toolkit :: String -> Tag
toolkit = T.fgc (C.crepr Palette.toolkit) <<< T.s


version :: Number -> Tag
version = T.fgc (C.crepr Palette.version) <<< T.s <<< show


family :: String -> Tag
family = T.fgc (C.crepr Palette.familyName) <<< T.s


nodeId :: String -> Tag
nodeId = T.fgc (C.crepr Palette.nodeId) <<< T.s


operator :: String -> Tag
operator = T.fgc (C.crepr Palette.operator) <<< T.s


comment :: String -> Tag
comment = T.fgc (C.crepr Palette.operator) <<< T.s


value :: String -> Tag
value = T.fgc (C.crepr Palette.value) <<< T.s


coord :: Int -> Tag
coord = T.fgc (C.crepr Palette.coord) <<< T.s <<< show


inletIdx :: Int -> Tag
inletIdx = T.fgc (C.crepr Palette.inletIdx) <<< T.s <<< show


outletIdx :: Int -> Tag
outletIdx = T.fgc (C.crepr Palette.outletIdx) <<< T.s <<< show


inletId :: String -> Tag
inletId = T.fgc (C.crepr Palette.inletId) <<< T.s


outletId :: String -> Tag
outletId = T.fgc (C.crepr Palette.outletId) <<< T.s


buttonToggle ::String -> Boolean -> Tag
buttonToggle repr true = T.fgc (C.crepr Palette.positive) $ T.s repr
buttonToggle repr false = T.fgc (C.crepr Palette.neutral) $ T.s repr


buttonConnection :: Either String Int -> Tag
buttonConnection (Left s) = T.fgc (C.crepr Palette.negative) $ T.s s
buttonConnection (Right 0) = T.fgc (C.crepr Palette.neutral) $ T.s "0"
buttonConnection (Right n) = T.fgc (C.crepr Palette.positive) $ T.s $ show n


outletHover :: Tag
outletHover = T.fgc (C.crepr Palette.neutral) $ T.s "🮧"


outletSelect :: Tag
outletSelect = T.fgc (C.crepr Palette.positive) $ T.s "~"


inletHover :: Tag
inletHover = T.fgc (C.crepr Palette.neutral) $ T.s "🮦"


infoNode :: forall repr. Mark repr => StatusLineInfo repr => repr -> Tag
infoNode repr =
    T.fgcs (mark repr) $ Info.statusLine repr


selected :: String -> Tag
selected = T.fgc (C.crepr Palette.positive) <<< T.s


{- TODO
familyDocs :: forall f. IsSymbol f => Id.Family f -> Tag
familyDocs family =
    let familyGroup = Hydra.toGroup family
    in T.fgcs (mark familyGroup) (show familyGroup)
        <> T.s " " <> familySignature (HFn.KnownFn $ reflect family)


familyShortInfo :: forall f. IsSymbol f => Id.Family f -> Tag
familyShortInfo family =
    let familyGroup = Hydra.toGroup family
    -- in T.bgc (C.crepr Palette.groupBg) (T.fgcs (mark familyGroup) (Info.statusLine familyGroup))
    in T.s "/" <> T.fgcs (mark familyGroup) (Info.statusLine familyGroup) <> T.s "/"
        <> T.s " " <> familySignature (HFn.KnownFn $ reflect family)


familySignature :: HFn.KnownFn -> Tag
familySignature knownFn =
    case (HFn.possiblyToFn knownFn :: Maybe (HFn.FnS H.FnArg H.FnOut)) of
        Just (name /\ args /\ outs) ->
            -- TODO: add familyDocs
            T.fgcs (C.crepr Palette.familyName) name
            <> T.s " -> "
            <> foldl (<>) (T.s "") (tagArgument <$> args)
            <> T.s " -> "
            <> foldl (<>) (T.s "") (tagOut <$> outs)
            <> T.s " // "
            <> T.fgcs (C.crepr Pico.lavender) (Info.docs knownFn)
        Nothing -> T.s "?"
    where
        tagArgument :: HFn.Argument H.FnArg -> Tag
        tagArgument arg = T.s "<" <>
            case HFn.argValue arg of
                _ ->
                    T.fgcs (C.crepr Pico.darkGreen) (HFn.argName arg)
                    <> T.s "::"
                    <> tagArgValue (HFn.argValue arg)
            <> T.s "> "
        tagOut :: HFn.Output H.FnOut -> Tag
        tagOut out = T.s "(" <>
            case HFn.outValue out of
                _ ->
                    T.fgcs (C.crepr Pico.darkGreen) (HFn.outName out)
                    <> T.s "::"
                    <> tagOutValue (HFn.outValue out)
            <> T.s ") "
        tagArgValue :: H.FnArg -> Tag
        tagArgValue val =
            T.fgc (mark val) $ T.s $ Info.docs val
        tagOutValue :: H.FnOut -> Tag
        tagOutValue val =
            T.fgc (mark val) $ T.s $ Info.docs val
-}