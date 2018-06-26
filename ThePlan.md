We separate the `Network`, which just defines a structure for some patch network, from `RunningNetwork`, which holds this structure and _also_ manages all the effects happening inside.
Currently `UI` tries to control the flow and does subscribing and unsubscribing, by performing the effects happening, it seems to be wrong. `UI` should subscribe all the data flow once and call methods without controlling the result. Summarizing data flow should be passed to UI as event intended to be subscribed to, include inlets/outlets data, handle node processing etc.

The Flow is: all inlet and outlet data flowing through the network, including the results of processing for nodes, so renderer would be able just to subscribe to it once and forever till the application would be stopped, updating the UI properly on every change. Should be similar to Subscriptions in Elm, probably, so it will be possible to subscribe to a specific node or inlet without triggering the broadcasting for all the data.

So, user should be able both to construct the `Network` before running The Flow, and modify it (same-named methods, but with `'`?) after running it. For example, `addNode`, `connect`, `disconnect`, when applied to "static" `Network`, just modify its structure, while `addNode'`, `connect'`, `disconnect'` (they operate on `RunningNetwork`) do the same, but also produce the effect. Subscriptions are happening inside the `Rpd` system.

Even when user runs the `Rpd` system w/o any renderer, data should be sent to inlets, processing in the nodes should run, etc.

Technically it means `Subsribers` and `Cancelers` are all managed inside the `RunningNetwork`, instead of the renderer. So `RunningNetwork` produces `FRP` effect. `Renderer` receives `TheFlow` along with the `Network` instance, so it will be able to react to changes inside it. It produces `DOM` effect of course.

```purescript
colorNode :: NodeDef d
colorNode =
    NodeDef
        { name : "color"
        , process : process
        , type : -- userType ??? optional field ???
        , render : "html" -> ...
        }
        [ intInlet "r"
        , intInlet "g"
        , intInlet "b"
        ]
        [ colorOutlet "color"
        ]
    where
        process = ()
...

render "html" colorNode == ???
render node =
    case node.def.type of
        "color" ... ?

colorInlet :: InletDef d
colorInlet =
    InletDef
    { accept : accept
    , adapt : adapt
    , show : show
    , render : "html" -> ...
    }
    where
        adapt =
        accept (Color _) = True
        accept _ = False
        show (Color _) = ...
        show _ = "Error"


render "html" intInlet ==

addNode 0 colorNode
addInlet 0 0 intInlet
send 0 0 (interval (Milliseconds 5) 10)
connect 0 0 0 2



typeclass Renderer Html =
    render (Node | Inlet | Outlet)
        case node.type ...
    renderData d =



data BuilderContext
    = BCPatch PatchId
    | BCNode NodePath
    |

data NetworkBuilder = NetworkBuilder BuilderContext

newPatch 'Test'
addNode 'aaa'
addNode' colorNode
addNode'' colorNode { name = 'CLR' }
addNode'' paletteNode { processF = ... }
addInlet 0 'ff'
send 0 0 (interval 5 foo)
addInlet' colorInlet
addInlet'' colorInlet { label = 'a' } -- may fail
addOutlet 0 'sum'
connect 0 0 1 0
```


purescript-behaviors is now 100% FFI-free, updated for 0.12 (thanks @jusrin00!), and I've broken out the event implementation as a separate library
https://pursuit.purescript.org/packages/purescript-behaviors/7.0.0 …
https://pursuit.purescript.org/packages/purescript-event/1.2.4 …
