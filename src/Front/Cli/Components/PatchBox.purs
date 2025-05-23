module Cli.Components.PatchBox where

import Prelude

import Effect (Effect)

import Blessed as B
import Blessed.Core.Coord ((<+>), (<->))
import Blessed.Core.Coord as Coord
import Blessed.Core.Dimension as Dimension
import Blessed.Core.Offset as Offset
import Blessed.Internal.Core as Core
import Blessed.UI.Boxes.Box.Option as Box

import Cli.Keys as Key
import Cli.State (State)
import Cli.Style (patchBox, patchBoxBorder) as Style
import Cli.Components.Library as Library
import Cli.Class.CliFriendly (class CliFriendly)
import Cli.Class.CliRenderer (class CliLocator)

import Noodle.Id as Id
import Noodle.Repr.HasFallback (class HasFallback)
import Noodle.Repr.StRepr (class StRepr)
import Noodle.Repr.ValueInChannel (ValueInChannel)
import Noodle.Repr.Tagged (class ValueTagged) as VT
import Noodle.Toolkit (Toolkit, class MarkToolkit)
import Noodle.Toolkit (class HoldsFamilies, class FromToPatchState) as Toolkit
import Noodle.Fn.Signature (class PossiblyToSignature)

import Cli.Components.NodeBox.InletIndicator as InletIndicator
import Cli.Components.NodeBox.OutletIndicator as OutletIndicator


component
    :: forall loc tk ps fs strepr chrepr
     . CliLocator loc
    => HasFallback chrepr
    => VT.ValueTagged chrepr
    => PossiblyToSignature tk (ValueInChannel chrepr) (ValueInChannel chrepr) Id.FamilyR
    => Toolkit.FromToPatchState tk ps strepr
    => Toolkit.HoldsFamilies strepr chrepr Effect fs
    => CliFriendly tk fs chrepr Effect
    => Toolkit tk fs strepr chrepr Effect
    -> Core.Blessed (State loc tk ps fs strepr chrepr Effect)
component toolkit =
    B.boxAnd Key.patchBox

        [ Box.top $ Offset.calc $ Coord.center <+> Coord.px 1
        , Box.left $ Offset.center
        , Box.width $ Dimension.percents 100.0
        , Box.height $ Dimension.calc $ Coord.percents 100.0 <-> Coord.px 1
        , Box.content "Patch"
        , Box.tags true
        , Style.patchBox
        , Style.patchBoxBorder
        ]

        [ Library.component toolkit
        , InletIndicator.component
        , OutletIndicator.component
        ]

        $ \_ -> do
            pure unit