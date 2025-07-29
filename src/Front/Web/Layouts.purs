module Web.Layouts where

import Prelude

import Data.Array (range) as Array
import Data.Int (toNumber) as Int

import Web.Components.AppScreen.State (UiMode(..)) as CState
import Web.Layer (TargetLayer(..))


import Play (Play, (~*))

import Play as Play


data UiPart
    = SidePanelButton Int
    | StatusBarSecion Int
    | SidePanel Int
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
    | Canvas


type UiParams =
    { size :: { width :: Number, height :: Number }
    , sidePanelButtons :: Int
    , statusBarSections :: Int
    }


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
        ~* Play.height size.height


_fullLayout :: UiParams -> Play UiPart
_fullLayout params =
    let
        topBarHeight = 20.0
        statusBarHeight = 30.0
        sidePanelButtonSize = 20.0
        libraryWidth = 150.0
        sidePanelWidth = 150.0

        sidePanelButton n =
          Play.i (SidePanelButton n)
            ~* Play.width  sidePanelButtonSize
            ~* Play.height sidePanelButtonSize
        spButtons = sidePanelButton <$> Array.range 0 params.sidePanelButtons

        statusBarSection n =
          Play.i (StatusBarSecion n)
            ~* (Play.width $ Int.toNumber n * 15.0)
            ~* Play.heightGrow
        sbSections = statusBarSection <$> Array.range 0 params.statusBarSections

        sidePanel n =
          Play.i (SidePanel n)
            ~* Play.widthGrow
            ~* Play.heightGrow
        sidePanels = sidePanel <$> Array.range 0 3
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


data NodePart
    = Title
    | TitleArea -- Title + Paddings
    | TitlePadding
    | Inlet Int
    | InletName
    | InletConnector
    | Outlet Int
    | OutletName
    | OutletConnector
    | NodeBackground
    | BodyArea -- Body + Inlets + Outltets
    | Inlets
    | Outlets
    | BodyBackground
    | Body


type NodeParams =
  { outletsCount :: Int
  , inletsCount :: Int
  , bodyWidth :: Number
  , bodyHeight :: Number
  }


horzNodeUI :: NodeParams -> Play NodePart
horzNodeUI params =
    let
        titleWidth = 30.0
        channelsHeight = 20.0
        -- bodyWidth = 700.0 -- try 300.0 to see how it fits
        -- bodyHeight = 120.0
        channelWidth = 70.0
        connectorWidth = 15.0
        -- inletsCount = 5
        --outletsCount = 7

        inlet n =
            Play.i (Inlet n)
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
        inlets = inlet <$> Array.range 0 params.inletsCount

        outlet n =
            Play.i (Outlet n)
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
        outlets = outlet <$> Array.range 0 params.outletsCount

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
                [ Play.i TitlePadding
                    ~* Play.widthGrow
                    ~* Play.height channelsHeight
                , Play.i Title
                    ~* Play.widthGrow
                    ~* Play.height params.bodyHeight
                , Play.i TitlePadding
                    ~* Play.widthGrow
                    ~* Play.height channelsHeight
                ]
            , Play.i BodyArea
                ~* Play.widthFit
                ~* Play.heightFit
                ~* Play.topToBottom
                ~* Play.with
                    [ Play.i Inlets
                        ~* Play.widthFit
                        ~* Play.height channelsHeight
                        ~* Play.with inlets
                    , Play.i BodyBackground
                        ~* Play.widthFitGrow
                        ~* Play.height params.bodyHeight
                        ~* Play.with
                            [ Play.i Body
                                ~* Play.width  params.bodyWidth
                                ~* Play.height params.bodyHeight
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

        inlet n =
            Play.i (Inlet n)
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
        inlets = inlet <$> Array.range 0 params.inletsCount

        outlet n =
            Play.i (Outlet n)
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
        outlets = outlet <$> Array.range 0 params.outletsCount

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

            , Play.i BodyArea
                ~* Play.widthFit
                ~* Play.heightFit
                ~* Play.leftToRight
                ~* Play.with
                    [ Play.i Inlets
                        ~* Play.widthFit
                        ~* Play.heightFit
                        ~* Play.topToBottom
                        ~* Play.with inlets
                    , Play.i BodyBackground
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
