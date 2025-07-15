module Web.Layouts where

import Prelude

import Data.Array (range) as Array
import Data.Int (toNumber) as Int


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


type UiParams =
    { size :: { width :: Number, height :: Number }
    , sidePanelButtons :: Int
    , statusBarSecions :: Int
    }


noodleUI :: Play UiPart
noodleUI =
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
        spButtons = sidePanelButton <$> Array.range 0 5

        statusBarSection n =
          Play.i (StatusBarSecion n)
            ~* (Play.width $ Int.toNumber n * 15.0)
            ~* Play.heightGrow
        sbSections = statusBarSection <$> Array.range 0 3

        sidePanel n =
          Play.i (SidePanel n)
            ~* Play.widthGrow
            ~* Play.heightGrow
        sidePanels = sidePanel <$> Array.range 0 3

    in Play.i Background
        ~* Play.width 1000.0
        ~* Play.height 1000.0
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


horzNodeUI :: Play NodePart
horzNodeUI =
    let
        titleWidth = 30.0
        channelsHeight = 20.0
        bodyWidth = 700.0 -- try 300.0 to see how it fits
        bodyHeight = 120.0
        channelWidth = 70.0
        connectorWidth = 15.0
        inletsCount = 5
        outletsCount = 7

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
        inlets = inlet <$> Array.range 0 inletsCount

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
        outlets = outlet <$> Array.range 0 outletsCount

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
                    ~* Play.height bodyHeight
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
                        ~* Play.height bodyHeight
                        ~* Play.with
                            [ Play.i Body
                                ~* Play.width  bodyWidth
                                ~* Play.height bodyHeight
                            ]
                    , Play.i Outlets
                        ~* Play.widthFit
                        ~* Play.height channelsHeight
                        ~* Play.with outlets
                    ]
                ]


vertNodeUI :: Play NodePart
vertNodeUI =
    let
        titleHeight = 30.0
        bodyWidth = 300.0
        bodyHeight = 400.0 -- try values less that 100.0 to see how it fits
        channelNameMinWidth = 100.0
        paddingWidth = channelNameMinWidth + connectorWidth -- TODO: auto-fit
        channelHeight = 20.0
        connectorWidth = 15.0
        inletsCount = 5
        outletsCount = 7

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
        inlets = inlet <$> Array.range 0 5

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
        outlets = outlet <$> Array.range 0 7

        exampleWidth  = bodyWidth + (2.0 * paddingWidth) + 50.0
        exampleHeight = max (bodyHeight + 50.0) (max (Int.toNumber inletsCount * channelHeight) (Int.toNumber outletsCount * channelHeight) + titleHeight)

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
                        ~* Play.width  bodyWidth
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
                        ~* Play.width bodyWidth
                        ~* Play.heightFitGrow
                        ~* Play.with
                            [ Play.i Body
                                ~* Play.width  bodyWidth
                                ~* Play.height bodyHeight
                            ]
                    , Play.i Outlets
                        ~* Play.widthFit
                        ~* Play.heightFit
                        ~* Play.topToBottom
                        ~* Play.with outlets
                    ]
            ]