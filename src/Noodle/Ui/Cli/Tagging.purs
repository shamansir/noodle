module Noodle.Ui.Cli.Tagging where

import Prelude

import Type.Proxy (Proxy(..))

import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
-- import Data.SProxy (reflect)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Array (foldl)




import Data.Text.Format (Tag)
import Data.Text.Format as T

import Noodle.Id as Id
import Noodle.Fn.ToFn (Fn, class ToFn, class PossiblyToFn, possiblyToFn, toFn, FnS)
import Noodle.Fn.ToFn (Argument, Output, argValue, argName, outName, outValue) as Fn

import Noodle.Ui.Cli.Palette as Palette
import Noodle.Ui.Cli.Palette.Item (crepr) as C
import Noodle.Ui.Cli.Palette.Item (Item, fullInfo) as Palette
import Noodle.Ui.Cli.Palette.Mark (class Mark, mark)
import Noodle.Ui.Cli.Palette.Set.X11 as X11
import Noodle.Ui.Cli.Palette.Set.Pico8 as Pico
import Noodle.Ui.Cli.Tagging.At (class At, at) as Tagged
import Noodle.Ui.Cli.Tagging.At (StatusLine, ChannelLabel, Documentation, statusLine, channelLabel, documentation) as At


inlet :: forall i repr. IsSymbol i => Mark repr => Tagged.At At.ChannelLabel repr => Int -> Id.Inlet i -> Maybe repr -> Tag
inlet idx inletId = inlet' idx $ Id.inletR inletId


inlet' :: forall repr. Mark repr => Tagged.At At.ChannelLabel repr  => Int -> Id.InletR -> Maybe repr -> Tag
inlet' idx inletId (Just repr) =
    T.fgc (mark repr) $ At.channelLabel repr -- "⋱" <> show idx <> "⋰" <> Info.short repr
inlet' idx inletId Nothing = T.s "⋱" <> (T.s $ show idx) <> T.s "⋰"


inletInfoBox :: forall i. IsSymbol i => Id.Inlet i -> Tag
inletInfoBox inletId =
    T.fgcs (C.crepr Palette.inletId) $ Id.inletName inletId


inletStatusLine :: forall f i repr. IsSymbol i => IsSymbol f => Mark repr => Tagged.At At.StatusLine repr => Id.Family f -> Int -> Id.Inlet i -> Maybe repr -> Tag
inletStatusLine family idx inletId = inletStatusLine' (Id.familyR family) idx $ Id.inletR inletId


inletStatusLine' :: forall repr. Mark repr => Tagged.At At.StatusLine repr => Id.FamilyR -> Int -> Id.InletR -> Maybe repr -> Tag
inletStatusLine' (Id.FamilyR { family }) idx inletId (Just repr) =
    -- TODO: show node id and group as well
    (T.fgcs (C.crepr Palette.familyName) family) <> T.s " " <> (T.fgcs (C.crepr Palette.inletId) $ Id.inletRName inletId) <> T.s " " <> (T.fgc (mark repr) $ At.statusLine repr) -- "⋱" <> show idx <> "⋰" <> Info.short repr
inletStatusLine' familyR idx inletId Nothing =
    T.s "⋱" <> (T.s $ show idx) <> T.s "⋰"


outlet ::forall o repr. IsSymbol o => Mark repr => Tagged.At At.ChannelLabel repr => Int -> Id.Outlet o -> Maybe repr -> Tag
outlet idx outletId = outlet' idx $ Id.outletR outletId


outlet' :: forall repr. Mark repr => Tagged.At At.ChannelLabel repr => Int -> Id.OutletR -> Maybe repr -> Tag
outlet' idx outletId (Just repr) =
    T.fgc (mark repr) $ At.channelLabel repr -- "⋱" <> show idx <> "⋰" <> Info.short repr
    -- Info.short repr -- "⋰" <> show idx <> "⋱" <> Info.short repr
outlet' idx outletId Nothing = T.s "⋰" <> (T.s $ show idx) <> T.s "⋱"


outletInfoBox :: forall o. IsSymbol o => Id.Outlet o -> Tag
outletInfoBox outletId =
    T.fgcs (C.crepr Palette.outletId) $ Id.outletName outletId


outletStatusLine ::forall f o repr. IsSymbol f => IsSymbol o => Mark repr => Tagged.At At.StatusLine repr => Id.Family f -> Int -> Id.Outlet o -> Maybe repr -> Tag
outletStatusLine family idx outletId = outletStatusLine' (Id.familyR family) idx $ Id.outletR outletId


outletStatusLine' :: forall repr. Mark repr => Tagged.At At.StatusLine repr => Id.FamilyR -> Int -> Id.OutletR -> Maybe repr -> Tag
outletStatusLine' (Id.FamilyR { family }) idx outletId (Just repr) =
    -- TODO: show group as well
    (T.fgcs (C.crepr Palette.familyName) family) <> T.s " " <> (T.fgcs (C.crepr Palette.outletId) $ Id.outletRName outletId) <> T.s " " <> (T.fgc (mark repr) $ At.statusLine repr) -- "⋱" <> show idx <> "⋰" <> Info.short repr
    --T.fgcs (mark repr) $ Info.full repr -- "⋱" <> show idx <> "⋰" <> Info.short repr
    -- Info.short repr -- "⋰" <> show idx <> "⋱" <> Info.short repr
outletStatusLine' familyR idx outletId Nothing = T.s "⋰" <> (T.s $ show idx) <> T.s "⋱"


nodeLabel :: forall f. IsSymbol f => Mark (Id.Family f) => Id.Family f -> Tag
nodeLabel family =
    T.bgc (C.crepr Palette.nodeBg) $ T.fgc (mark family) $ T.s $ Id.family $ Id.familyR family


nodeMouseOver
    :: forall f grp arg out
     . IsSymbol f
    => FamilyHasDocs arg out grp f
    => Proxy grp -> Proxy arg -> Proxy out
    -> Id.Family f
    -> Tag
nodeMouseOver =
    familyShortInfo


familyMouseOver
    :: forall f grp arg out
     . IsSymbol f
    => FamilyHasDocs arg out grp f
    => Proxy grp -> Proxy arg -> Proxy out
    -> Id.Family f
    -> Tag
familyMouseOver =
    familyShortInfo


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


libraryItem :: Mark Id.FamilyR => Id.FamilyR -> Tag
libraryItem familyR =
    T.fgc (mark familyR) $ T.s $ Id.family familyR


{- TODO
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


tkVersion :: Number -> Tag
tkVersion = T.fgc (C.crepr Palette.tkVersion) <<< T.s <<< show


ndfVersion :: Number -> Tag
ndfVersion = T.fgc (C.crepr Palette.ndfVersion) <<< T.s <<< show


family :: String -> Tag
family = T.fgc (C.crepr Palette.familyName) <<< T.s


someGroup :: String -> Tag
someGroup = T.fgc (C.crepr Palette.someGroup) <<< T.s


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


type_ :: String -> Tag
type_ = T.fgc (C.crepr Palette.type_) <<< T.s


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


infoNode :: forall repr. Mark repr => Tagged.At At.StatusLine repr => repr -> Tag
infoNode repr =
    T.fgc (mark repr) $ At.statusLine repr


selected :: String -> Tag
selected = T.fgc (C.crepr Palette.positive) <<< T.s



familyDocs
    :: forall f grp arg out
     . IsSymbol f
    => FamilyHasDocs arg out grp f
    => Proxy grp -> Proxy arg -> Proxy out
    -> Id.Family f
    -> Tag
familyDocs pgrp parg pout family =
    let (familyGroup :: grp) = Id.groupOf family
    in T.fgcs (mark familyGroup) (show familyGroup)
        <> T.s " " <> familySignature pgrp parg pout family


class
    ( PossiblyToFn arg out (Id.Family f)
    , Tagged.At At.Documentation (Id.Family f)
    , Mark arg, Tagged.At At.Documentation arg
    , Mark out, Tagged.At At.Documentation out
    , Mark grp, Show grp, Id.FamilyGroup grp, Tagged.At At.StatusLine grp
    ) <= FamilyHasDocs arg out grp f


instance
    ( PossiblyToFn arg out (Id.Family f)
    , Tagged.At At.Documentation (Id.Family f)
    , Mark arg, Tagged.At At.Documentation arg
    , Mark out, Tagged.At At.Documentation out
    , Mark grp, Show grp, Id.FamilyGroup grp, Tagged.At At.StatusLine grp
    ) => FamilyHasDocs arg out grp f



familyShortInfo
    :: forall f grp arg out
     . IsSymbol f
    => FamilyHasDocs arg out grp f
    => Proxy grp -> Proxy arg -> Proxy out
    -> Id.Family f
    -> Tag
familyShortInfo pgrp parg pout family =
    let (familyGroup :: grp) = Id.groupOf family
    -- in T.bgc (C.crepr Palette.groupBg) (T.fgcs (mark familyGroup) (Info.statusLine familyGroup))
    in T.s "/" <> T.fgc (mark familyGroup) (At.statusLine familyGroup) <> T.s "/"
        <> T.s " " <> familySignature pgrp parg pout family


familySignature
    :: forall grp arg out f
     . FamilyHasDocs arg out grp f
    => Proxy grp -> Proxy arg -> Proxy out
    -> Id.Family f
    -> Tag
familySignature _ _ _ family =
    case (possiblyToFn family :: Maybe (FnS arg out)) of
        Just (name /\ args /\ outs) ->
            -- TODO: add familyDocs
            T.fgcs (C.crepr Palette.familyName) name
            <> T.s " -> "
            <> foldl (<>) (T.s "") (tagArgument <$> args)
            <> T.s " -> "
            <> foldl (<>) (T.s "") (tagOut <$> outs)
            <> T.s " // "
            <> T.fgc (C.crepr Pico.lavender) (At.documentation family)
        Nothing -> T.s "?"
    where
        tagArgument :: Fn.Argument arg -> Tag
        tagArgument arg = T.s "<" <>
            case Fn.argValue arg of
                _ ->
                    T.fgcs (C.crepr Pico.darkGreen) (Fn.argName arg)
                    <> T.s "::"
                    <> tagArgValue (Fn.argValue arg)
            <> T.s "> "
        tagOut :: Fn.Output out -> Tag
        tagOut out = T.s "(" <>
            case Fn.outValue out of
                _ ->
                    T.fgcs (C.crepr Pico.darkGreen) (Fn.outName out)
                    <> T.s "::"
                    <> tagOutValue (Fn.outValue out)
            <> T.s ") "
        tagArgValue :: arg -> Tag
        tagArgValue val =
            T.fgc (mark val) $ At.documentation val
        tagOutValue :: out -> Tag
        tagOutValue val =
            T.fgc (mark val) $ At.documentation val