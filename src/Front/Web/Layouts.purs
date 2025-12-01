module Web.Layouts where

import Prelude

import Blessed.Internal.BlessedSubj (Node)
import Data.Array (length, range) as Array
import Data.FunctorWithIndex (mapWithIndex)
import Data.Int (toNumber) as Int
import Data.Set (Set)
import Data.Set (toUnfoldable) as Set
import Front.Shared.Panels as Panels
import Front.Shared.StatusBarCells as SBC
import Noodle.Raw.Fn.Shape (InletDefRec, OutletDefRec)
import Play (Play, (~*))
import Play as Play
import Web.Components.StatusBar as SB
import Web.Layer (TargetLayer(..))



data UiPart
    = SidePanelButton Int Panels.Which
    | SidePanel Int Panels.Which
    | StatusBarSection Int SBC.Which
    | Background
    | Library
    | Nodes
    | SidePanelsButtons
    | StatusBarSections
    | SidePanels
    | StatusBarDoc
    | PatchesBar
    | Top
    | Middle
    | StatusBar
    -- | Canvas


type UiParams =
    { size :: { width :: Number, height :: Number }
    , sidePanels :: Set Panels.Which
    , statusBarSections :: Int
    }


{-
noodleUI :: TargetLayer -> CState.UiMode -> UiParams -> Play UiPart
noodleUI targetLayer =
    case _ of
        CState.OnlyCanvas _ -> _onlyCanvas
        CState.CanvasFullyVisible -> _fullLayout
        CState.TransparentOverlay _ -> _fullLayout
        CState.SolidOverlay _ -> _fullLayout


_onlyCanvas :: UiParams -> Play UiPart
_onlyCanvas { size } =
    Play.i Canvas
        ~* Play.width  size.width
        ~* Play.height size.height -}


noodleUI :: UiParams -> Play UiPart
noodleUI = _fullLayout


_fullLayout :: UiParams -> Play UiPart
_fullLayout params =
    let
        topBarHeight = 45.0
        statusBarPadding = 5.0
        statusBarHeight = 25.0 + statusBarPadding * 2.0
        sidePanelButtonWidth = 32.0
        sidePanelButtonHeight = 20.0
        libraryWidth = 150.0
        sidePanelWidth = 350.0

        sidePanelButton n which =
          Play.i (SidePanelButton n which)
            ~* Play.width  sidePanelButtonWidth
            ~* Play.height sidePanelButtonHeight
        spButtons = mapWithIndex sidePanelButton Panels.allPanels -- params.sidePanelButtons

        statusBarSection n which =
          Play.i (StatusBarSection n which)
            ~* (Play.width $ SB.cellWidth which)
            ~* Play.heightGrow
        sbSections = mapWithIndex statusBarSection SBC.allCells

        sidePanel n which =
          Play.i (SidePanel n which)
            ~* Play.widthGrow
            ~* Play.heightGrow
        sidePanels = mapWithIndex sidePanel $ Set.toUnfoldable params.sidePanels
    in Play.i Background
        ~* Play.width params.size.width
        ~* Play.height params.size.height
        ~* Play.topToBottom
        ~* Play.with
            [ Play.i Top
                ~* Play.widthGrow
                ~* (Play.height topBarHeight)
                ~* Play.with
                    [ Play.i PatchesBar
                        ~* Play.widthGrow
                        ~* Play.heightGrow
                    , Play.i SidePanelsButtons
                        ~* Play.widthFit
                        ~* Play.heightGrow
                        ~* Play.childGap 4.0
                        ~* Play.with spButtons
                    ]
            , Play.i Middle
                ~* Play.widthGrow
                ~* Play.heightGrow
                ~* Play.with
                    [ Play.i Library
                        ~* Play.width libraryWidth
                        ~* Play.heightGrow
                        -- ~* Play.padding { top : 0.0, left : 10.0, right : 5.0, bottom : 5.0 }
                    , Play.i Nodes
                        ~* Play.widthGrow
                        ~* Play.heightGrow
                    , Play.i SidePanels
                        ~* Play.width sidePanelWidth
                        ~* Play.heightGrow
                        ~* Play.topToBottom
                        ~* Play.with sidePanels
                ]
            , Play.i StatusBar
                ~* Play.widthGrow
                ~* Play.height statusBarHeight
                ~* Play.with
                [ Play.i StatusBarDoc
                    ~* Play.widthGrow
                    ~* Play.heightGrow
                , Play.i StatusBarSections
                    ~* Play.widthFit
                    ~* Play.heightGrow
                    ~* Play.childGap 4.0
                    ~* Play.with sbSections
                ]
            ]


data NodeButton
    = RemoveButton
    | CollapseButton
    | ControlButton


data NodePart
    = Title
    | TitleArea -- Title + Paddings
    | TitlePadding
    | Inlet Int InletDefRec
    | InletName
    | InletConnector
    | Outlet Int OutletDefRec
    | OutletName
    | OutletConnector
    | NodeBackground
    | FunctionalArea -- Inlets + BodyBackground + Outlets
    | BodyConstraint -- Body: fits body to min width
    | BodyGrow -- area that grows to fill space for the buttons
    | Inlets
    | Outlets
    | BodyAndButtons -- BodyConstraint (Body) + BodyGrow + ButtonsArea
    | Body
    | ButtonsArea
    | Button NodeButton


derive instance Eq NodeButton
derive instance Eq NodePart


type NodeParams =
    { inlets :: Array InletDefRec
    , outlets :: Array OutletDefRec
    , bodyWidth :: Number
    , bodyHeight :: Number
    }


horzNodeUI :: NodeParams -> Play NodePart
horzNodeUI params =
    let
        titleWidth = 16.0
        channelsHeight = 20.0
        -- bodyWidth = 700.0 -- try 300.0 to see how it fits
        -- bodyHeight = 120.0
        channelWidth = 70.0
        connectorWidth = 15.0
        -- inletsCount = 5
        --outletsCount = 7
        buttonSide = 20.0
        minBodyWidth = 150.0

        inlet n def =
            Play.i (Inlet n def)
            ~* Play.width channelWidth
            ~* Play.heightGrow
            ~* Play.with
                [ Play.i InletConnector
                    ~* Play.width connectorWidth
                    ~* Play.heightGrow
                , Play.i InletName
                    ~* Play.widthGrow
                    ~* Play.heightGrow
                ]
        inlets = mapWithIndex inlet params.inlets

        outlet n def =
            Play.i (Outlet n def)
            ~* Play.width channelWidth
            ~* Play.heightGrow
            ~* Play.with
                [ Play.i OutletConnector
                    ~* Play.width connectorWidth
                    ~* Play.heightGrow
                , Play.i OutletName
                    ~* Play.widthGrow
                    ~* Play.heightGrow
                ]
        outlets = mapWithIndex outlet params.outlets

        buttons =
            [ Play.i (Button RemoveButton)
                ~* Play.widthGrow
                ~* Play.height buttonSide
            , Play.i (Button CollapseButton)
                ~* Play.widthGrow
                ~* Play.height buttonSide
            ]

    in Play.i NodeBackground
        ~* Play.widthFit
        ~* Play.heightFit
        ~* Play.leftToRight
        ~* Play.with
            [ Play.i TitleArea
                ~* Play.width titleWidth
                ~* Play.heightFit
                ~* Play.topToBottom
            ~* Play.with
                [ Play.i (Button ControlButton) -- TitlePadding
                    ~* Play.widthGrow
                    ~* Play.height channelsHeight
                , Play.i Title
                    ~* Play.widthGrow
                    ~* Play.height params.bodyHeight
                , Play.i TitlePadding
                    ~* Play.widthGrow
                    ~* Play.height channelsHeight
                ]
            , Play.i FunctionalArea
                ~* Play.widthFit
                ~* Play.heightFit
                ~* Play.topToBottom
                ~* Play.padding{ top : 0.0, left : 5.0, right : 0.0, bottom : 0.0 }
                ~* Play.with
                    [ Play.i Inlets
                        ~* Play.widthFit
                        ~* Play.height channelsHeight
                        ~* Play.with inlets
                    , Play.i BodyAndButtons
                        ~* Play.widthFitGrow
                        ~* Play.height params.bodyHeight
                        ~* Play.with
                            [ Play.i BodyConstraint
                                ~* Play.widthFitMin minBodyWidth
                                ~* Play.height params.bodyHeight
                                ~* Play.with
                                    [ Play.i Body
                                        ~* Play.width  params.bodyWidth
                                        ~* Play.height params.bodyHeight
                                    ]
                            , Play.i BodyGrow
                                ~* Play.widthGrow
                                ~* Play.height params.bodyHeight
                            , Play.i ButtonsArea
                                ~* Play.width 30.0
                                ~* Play.heightGrow
                                ~* Play.childGap 5.0
                                ~* Play.topToBottom
                                ~* Play.with buttons
                            ]
                    , Play.i Outlets
                        ~* Play.widthFit
                        ~* Play.height channelsHeight
                        ~* Play.with outlets
                    ]
                ]


vertNodeUI :: NodeParams -> Play NodePart
vertNodeUI params =
    let
        titleHeight = 30.0
        channelNameMinWidth = 100.0
        paddingWidth = channelNameMinWidth + connectorWidth -- TODO: auto-fit
        channelHeight = 20.0
        connectorWidth = 15.0

        inlet n def =
            Play.i (Inlet n def)
            ~* Play.widthFit
            ~* Play.heightFit
            ~* Play.with
                [ Play.i InletName
                    ~* Play.width  channelNameMinWidth
                    ~* Play.height channelHeight
                , Play.i InletConnector
                    ~* Play.width connectorWidth
                    ~* Play.heightGrow
                ]
        inlets = mapWithIndex inlet params.inlets

        outlet n def =
            Play.i (Outlet n def)
            ~* Play.widthFit
            ~* Play.heightFit
            ~* Play.with
                [ Play.i OutletConnector
                    ~* Play.width connectorWidth
                    ~* Play.heightGrow
                , Play.i OutletName
                    ~* (Play.width  channelNameMinWidth)
                    ~* (Play.height channelHeight)
                ]
        outlets = mapWithIndex outlet params.outlets

        -- exampleWidth  = bodyWidth + (2.0 * paddingWidth) + 50.0
        -- exampleHeight = max (bodyHeight + 50.0) (max (Int.toNumber inletsCount * channelHeight) (Int.toNumber outletsCount * channelHeight) + titleHeight)

    in Play.i NodeBackground
        ~* Play.widthFit
        ~* Play.heightFit
        ~* Play.topToBottom
        ~* Play.with
            [ Play.i TitleArea
                ~* Play.widthFit
                ~* Play.height titleHeight
                ~* Play.leftToRight
                ~* Play.with
                    [ Play.i TitlePadding
                        ~* Play.width paddingWidth
                        ~* Play.heightGrow
                    , Play.i Title
                        ~* Play.width  params.bodyWidth
                        ~* Play.height titleHeight
                    , Play.i TitlePadding
                        ~* Play.width paddingWidth
                        ~* Play.heightGrow
                    ]

            , Play.i FunctionalArea
                ~* Play.widthFit
                ~* Play.heightFit
                ~* Play.leftToRight
                ~* Play.with
                    [ Play.i Inlets
                        ~* Play.widthFit
                        ~* Play.heightFit
                        ~* Play.topToBottom
                        ~* Play.with inlets
                    , Play.i BodyAndButtons
                        ~* Play.width params.bodyWidth
                        ~* Play.heightFitGrow
                        ~* Play.with
                            [ Play.i Body
                                ~* Play.width  params.bodyWidth
                                ~* Play.height params.bodyHeight
                            ]
                    , Play.i Outlets
                        ~* Play.widthFit
                        ~* Play.heightFit
                        ~* Play.topToBottom
                        ~* Play.with outlets
                    ]
            ]
