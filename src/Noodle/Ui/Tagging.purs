module Noodle.Ui.Tagging where

import Prelude

import Type.Proxy (Proxy(..))

import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
-- import Data.SProxy (reflect)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Array (foldl)
import Data.Newtype (unwrap)
import Data.Bifunctor (bimap)
import Data.UniqueHash as UH


import Data.Text.Format (Tag)
import Data.Text.Format as T

import Noodle.Id as Id
import Noodle.Fn.Signature (Signature, class PossiblyToSignature, SignatureS, possiblyToSignature)
import Noodle.Fn.Signature (Argument, Output, argValue, argName, outName, outValue) as Sig
import Noodle.Raw.Node (NodeChanges) as Raw
import Noodle.Raw.Fn.Updates as Updates
import Noodle.Fn.Generic.Updates (fromRecord) as Updates
import Noodle.Toolkit (ToolkitKey, class IsToolkit, class MarkToolkit, class HasChRepr, markGroup, markFamily, groupOf)
import Noodle.Repr.ValueInChannel (ValueInChannel)
import Noodle.Repr.ValueInChannel (resolve) as ViC
import Noodle.Fn.Shape.Temperament (Temperament(..))

import Noodle.Ui.Cli.Palette as Palette
import Noodle.Ui.Palette.Item (colorOf, reprOf) as C
import Noodle.Ui.Palette.Item (Item, fullInfo) as Palette
import Noodle.Ui.Palette.Set.X11 as X11
import Noodle.Ui.Palette.Set.Pico8 as Pico
import Noodle.Ui.Tagging.At (class At, at) as Tagged
import Noodle.Ui.Tagging.At (StatusLine, ChannelLabel, Documentation, InfoNode, statusLine, channelLabel, documentation, infoNode) as At


inlet :: forall chrepr. Tagged.At At.ChannelLabel chrepr  => Int -> Id.InletR -> ValueInChannel chrepr -> Tag
inlet idx inletId =
    ViC.resolve
        { accept : \repr ->
            At.channelLabel repr -- "⋱" <> show idx <> "⋰" <> Info.short repr
            -- FIXME: show other values in ViC
        , decline
        , missingKey
        , empty
        }
    where
        missingKey key = T.s "⋱" <> T.fgcs (C.colorOf X11.lightyellow) "?" <> T.s "⋰"
        decline = T.s "⋱" <> T.fgcs (C.colorOf X11.red2) "x" <> T.s "⋰"
        empty = T.s "⋱" <> (T.s $ show idx) <> T.s "⋰"
    -- TODO : from `inletId`` :: -- T.fgc (C.colorOf Palette.inletId) <<< T.s


inletInfoBox :: Id.InletR -> Tag
inletInfoBox inletR =
    T.fgcs (C.colorOf Palette.inletId) $ Id.inletRName inletR


inletStatusLine :: forall chrepr. Tagged.At At.StatusLine chrepr => Id.FamilyR -> Int -> Id.InletR -> ValueInChannel chrepr -> Tag
inletStatusLine familyR idx inletId =
    -- TODO: show node id and group as well
    ViC.resolve
        { accept : \repr ->
            (T.fgcs (C.colorOf Palette.familyName) $ Id.family familyR) <> T.space <> (T.fgcs (C.colorOf Palette.inletId) $ Id.inletRName inletId) <> T.space <> At.statusLine repr -- "⋱" <> show idx <> "⋰" <> Info.short repr
            -- FIXME: show other values in ViC
        , decline
        , missingKey
        , empty
        }
    where
        missingKey key = T.s "⋱" <> T.fgcs (C.colorOf X11.lightyellow) "?" <> T.s "⋰"
        decline = T.s "⋱" <> T.fgcs (C.colorOf X11.red2) "x" <> T.s "⋰"
        empty = T.s "⋱" <> (T.s $ show idx) <> T.s "⋰"


outlet :: forall chrepr. Tagged.At At.ChannelLabel chrepr => Int -> Id.OutletR -> ValueInChannel chrepr -> Tag
outlet idx outletId =
    ViC.resolve
        { accept : \repr ->
            At.channelLabel repr -- "⋱" <> show idx <> "⋰" <> Info.short repr
            -- FIXME: show other values in ViC
        , decline
        , missingKey
        , empty
        }
    where
        missingKey _ = T.s "⋰" <> T.fgcs (C.colorOf X11.lightyellow) "?" <> T.s "⋱"
        decline = T.s "⋰" <> T.fgcs (C.colorOf X11.red2) "x" <> T.s "⋱"
        empty = T.s "⋰" <> (T.s $ show idx) <> T.s "⋱"


outletInfoBox :: Id.OutletR -> Tag
outletInfoBox outletR =
    T.fgcs (C.colorOf Palette.outletId) $ Id.outletRName outletR


outletStatusLine :: forall chrepr. Tagged.At At.StatusLine chrepr => Id.FamilyR -> Int -> Id.OutletR -> ValueInChannel chrepr -> Tag
outletStatusLine familyR idx outletId =
    -- TODO: show node id and group as well
    ViC.resolve
        { accept : \repr ->
            (T.fgcs (C.colorOf Palette.familyName) $ Id.family familyR) <> T.space <> (T.fgcs (C.colorOf Palette.outletId) $ Id.outletRName outletId) <> T.space <> At.statusLine repr -- "⋱" <> show idx <> "⋰" <> Info.short repr
            -- FIXME: show other values in ViC
        , decline
        , missingKey
        , empty
        }
    where
        missingKey _ = T.s "⋰" <> T.fgcs (C.colorOf X11.lightyellow) "?" <> T.s "⋱"
        decline = T.s "⋰" <> T.fgcs (C.colorOf X11.red2) "x" <> T.s "⋱"
        empty = T.s "⋰" <> (T.s $ show idx) <> T.s "⋱"


nodeLabel :: forall tk. MarkToolkit tk => Proxy tk -> Id.FamilyR -> Tag
nodeLabel ptk familyR =
    T.bgc (C.colorOf Palette.nodeBg) $ T.fgc (markFamily ptk (groupOf ptk familyR) familyR) $ T.s $ Id.family familyR


nodeStatusLine :: forall tk strepr chrepr. MarkToolkit tk => Tagged.At At.StatusLine chrepr => HasChRepr tk chrepr => Proxy tk -> Id.NodeR -> Raw.NodeChanges strepr chrepr -> Tag
nodeStatusLine ptk nodeR = _fnOnelineSignature (Proxy :: _ At.StatusLine) ptk (Right nodeR) <<< Updates.orderedToSignature nodeR <<< Updates.fromRecord


nodeDocumentation :: forall tk strepr chrepr. MarkToolkit tk => Tagged.At At.Documentation chrepr => HasChRepr tk chrepr => Proxy tk -> Id.NodeR -> Raw.NodeChanges strepr chrepr -> Tag
nodeDocumentation ptk nodeR = _fnOnelineSignature (Proxy :: _ At.Documentation) ptk (Right nodeR) <<< Updates.orderedToSignature nodeR <<< Updates.fromRecord


{- T.fgcs (C.colorOf Palette.familyName) (reflect family)
    <> T.s " ==== "
    <> -}


removeButtonOut ∷ Tag
removeButtonOut =
    T.fgcs (C.colorOf Pico.blue) "⨯"


removeButtonOver ∷ Tag
removeButtonOver =
    T.fgcs (C.colorOf Pico.red) "⨯" -- "╳"


removeInfoBox ∷ Tag
removeInfoBox =
    T.fgcs (C.colorOf Pico.red) "remove"


removeStatusLine :: Id.FamilyR -> Tag
removeStatusLine familyR =
    T.fgcs (C.colorOf Pico.red) "remove" <> T.space <> (T.fgcs (C.colorOf Palette.familyName) $ Id.family familyR)


libraryItem :: forall (tk :: ToolkitKey). MarkToolkit tk => Proxy tk -> Id.FamilyR -> Tag
libraryItem ptk familyR =
    T.fgc (markFamily ptk (groupOf ptk familyR) familyR) $ T.s $ Id.family familyR


{- TODO
glslFnItem :: H.GlslFn -> Tag
glslFnItem (H.GlslFn (kind /\ _ /\ glslFn)) =
    T.fgc (C.colorOf Pico.blue) $ T.s $ HFn.name glslFn
    -}

    {-
    let color = mark $ Hydra.toGroupR familyR
    in T.fgc color $ T.s $ Id.reflectFamilyR familyR
    -}

paletteFg :: Palette.Item -> Tag
paletteFg item = T.fg (C.reprOf item) (T.s item.label)


paletteBg :: Palette.Item -> Tag
paletteBg item = T.bg (C.reprOf item) (T.s item.label)


{-
paletteItem :: Palette.Item -> Tag
paletteItem item =
    T.bg item.repr (T.s "      ") <> T.space <> T.fg item.repr (T.s $ Palette.fullInfo item)
-}


-- Commands

toolkit :: String -> Tag
toolkit = T.fgc (C.colorOf Palette.toolkit) <<< T.s


tkVersion :: Number -> Tag
tkVersion = T.fgc (C.colorOf Palette.tkVersion) <<< T.s <<< show


ndfVersion :: Number -> Tag
ndfVersion = T.fgc (C.colorOf Palette.ndfVersion) <<< T.s <<< show


family :: String -> Tag
family = T.fgc (C.colorOf Palette.familyName) <<< T.s


someGroup :: String -> Tag
someGroup = T.fgc (C.colorOf Palette.someGroup) <<< T.s


nodeId :: String -> Tag
nodeId = T.fgc (C.colorOf Palette.nodeId) <<< T.s


operator :: String -> Tag
operator = T.fgc (C.colorOf Palette.operator) <<< T.s


comment :: String -> Tag
comment = T.fgc (C.colorOf Palette.comment) <<< T.s


documentation :: String -> Tag
documentation = T.fgc (C.colorOf Palette.operator) <<< T.s


markerSymbol :: String -> Tag
markerSymbol = T.fgc (C.colorOf Pico.darkGrey) <<< T.s


value :: String -> Tag
value = T.fgc (C.colorOf Palette.value) <<< T.s


coord :: Int -> Tag
coord = T.fgc (C.colorOf Palette.coord) <<< T.s <<< show


inletIdx :: Int -> Tag
inletIdx = T.fgc (C.colorOf Palette.inletIdx) <<< T.s <<< show


outletIdx :: Int -> Tag
outletIdx = T.fgc (C.colorOf Palette.outletIdx) <<< T.s <<< show


inletId :: String -> Tag
inletId = T.fgc (C.colorOf Palette.inletId) <<< T.s


outletId :: String -> Tag
outletId = T.fgc (C.colorOf Palette.outletId) <<< T.s


type_ :: String -> Tag
type_ = T.fgc (C.colorOf Palette.type_) <<< T.s


buttonToggle ::String -> Boolean -> Tag
buttonToggle repr true = T.fgc (C.colorOf Palette.positive) $ T.s repr
buttonToggle repr false = T.fgc (C.colorOf Palette.neutral) $ T.s repr


buttonConnection :: Either String Int -> Tag
buttonConnection (Left s) = T.fgc (C.colorOf Palette.negative) $ T.s s
buttonConnection (Right 0) = T.fgc (C.colorOf Palette.neutral) $ T.s "0"
buttonConnection (Right n) = T.fgc (C.colorOf Palette.positive) $ T.s $ show n


outletHover :: Tag
outletHover = T.fgc (C.colorOf Palette.outlet) $ T.s "⌃"


outletSelect :: Tag
outletSelect = T.fgc (C.colorOf Palette.outletConnect) $ T.s "⇣" -- "↯"


inletHover :: Temperament -> Tag
inletHover temp =
    T.fgc (C.colorOf $ case temp of
        Cold -> Palette.inletCold
        Hot -> Palette.inletHot
    ) $ T.s "⌄"


infoNode :: forall repr. Tagged.At At.InfoNode repr => repr -> Tag
infoNode = At.infoNode


selected :: String -> Tag
selected = T.fgc (C.colorOf Palette.positive) <<< T.s


orderItem :: String -> Tag
orderItem = T.fgc (C.colorOf Palette.orderItem) <<< T.s


orderSplit :: String -> Tag
orderSplit = T.fgc (C.colorOf Palette.orderSplit) <<< T.s


filePath :: String -> Tag
filePath = T.fgc (C.colorOf Palette.filePath) <<< T.s


familyDocs
    :: forall tk chrepr
     . MarkToolkit tk
    => HasChRepr tk chrepr
    => Tagged.At At.Documentation chrepr
    => PossiblyToSignature tk (ValueInChannel chrepr) (ValueInChannel chrepr) Id.FamilyR
    => Proxy tk
    -> Id.FamilyR
    -> Tag
familyDocs ptk familyR =
    {- let group = groupOf ptk familyR
    in T.fgcs (markGroup ptk group) (Id.group group)
        <> T.space <> -} familyOnelineSignature (Proxy :: _ At.Documentation) ptk familyR


familyStatusLine
    :: forall tk chrepr
     . MarkToolkit tk
    => HasChRepr tk chrepr
    => Tagged.At At.StatusLine chrepr
    => PossiblyToSignature tk (ValueInChannel chrepr) (ValueInChannel chrepr) Id.FamilyR
    => Proxy tk
    -> Id.FamilyR
    -> Tag
familyStatusLine ptk familyR =
    let group = groupOf ptk familyR
    in T.s "/" <> T.fgcs (markGroup ptk group) (Id.group group) <> T.s "/"
        <> T.space <> familyOnelineSignature (Proxy :: _ At.StatusLine) ptk familyR


_fnOnelineSignature
    :: forall at tk chrepr
     . MarkToolkit tk
    => HasChRepr tk chrepr
    => Tagged.At at chrepr
    => Proxy at -> Proxy tk
    -> Either Id.FamilyR Id.NodeR
    -> Signature (ValueInChannel chrepr) (ValueInChannel chrepr)
    -> Tag
_fnOnelineSignature pat ptk eNodeR = unwrap >>> case _ of
    (name /\ args /\ outs) ->
        let
            -- tagName = T.fgcs (C.colorOf Palette.familyName) name
            tagGroup familyR = T.fgcs (markGroup ptk $ groupOf ptk familyR) $ Id.group $ groupOf ptk familyR
            tagFamily familyR = (T.fgcs (markFamily ptk (groupOf ptk familyR) familyR) $ Id.family familyR) <> T.s " " <> (T.wrap (operator "(") (operator ")") $ tagGroup familyR)
            prefix =
                case eNodeR of
                    Right nodeR -> tagFamily $ Id.familyOf nodeR
                    Left familyR -> tagFamily familyR
            postfix =
                case eNodeR of
                    Right nodeR -> operator "//" <> T.s " " <> (T.fgcs (C.colorOf Pico.darkerGrey) $ UH.toString $ Id.hashOf nodeR)
                    Left _ -> T.nil
        in prefix
            <> T.s " " <> operator "=>" <> T.s " "
            <> foldl (<>) (T.s "") (tagArgument <$> args)
            <> operator "->" <> T.s " "
            <> foldl (<>) (T.s "") (tagOut <$> outs)
            <> postfix
    where
        tagArgument :: Sig.Argument (ValueInChannel chrepr) -> Tag
        tagArgument arg = markerSymbol "<"
            <> T.fgcs (C.colorOf Pico.darkGreen) (Sig.argName arg)
            <>  (Sig.argValue arg # ViC.resolve
                    { accept : \inVal ->
                        operator "::" <> (Tagged.at pat) inVal
                     -- FIXME: show other values in ViC
                    , decline : T.nil
                    , missingKey : const T.nil
                    , empty : T.nil
                    }
                )
            <> markerSymbol ">" <> T.s " "
        tagOut :: Sig.Output (ValueInChannel chrepr) -> Tag
        tagOut out = markerSymbol "(" <> T.fgcs (C.colorOf Pico.darkGreen) (Sig.outName out) <>
            (Sig.outValue out # ViC.resolve
                { accept : \outVal ->
                    operator "::" <> (Tagged.at pat) outVal
                    -- FIXME: show other values in ViC
                , decline : T.nil
                , missingKey : const T.nil
                , empty : T.nil
                }
            )
            <> markerSymbol ")" <> T.s " "


familyOnelineSignature
    :: forall at tk chrepr
     . MarkToolkit tk
    => HasChRepr tk chrepr
    => Tagged.At at chrepr
    => PossiblyToSignature tk (ValueInChannel chrepr) (ValueInChannel chrepr) Id.FamilyR
    => Proxy at -> Proxy tk
    -> Id.FamilyR
    -> Tag
familyOnelineSignature pat ptk familyR =
    case (possiblyToSignature ptk familyR :: Maybe (Signature (ValueInChannel chrepr) (ValueInChannel chrepr))) of
        Just fn -> _fnOnelineSignature pat ptk (Left familyR) fn
        Nothing -> T.fgcs (C.colorOf X11.lightyellow) "?"