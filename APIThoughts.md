
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

empty
    # newPatch 'Test'
    # addNode 'aaa'
    # addNode' colorNode
    # addNode'' colorNode { name = 'CLR' }
    # addNode'' paletteNode { processF = ... }
    # addInlet 0 'ff'
    -- let lastNodePath <- getLastNode
    # send 0 0 (interval 5 foo)
    # addInlet' colorInlet
    # addInlet'' colorInlet { label = 'a' } -- may fail
    # addOutlet 0 'sum'
    # connect 0 0 1 0


reusableNode nw =
    addNode '...' nw
       # addInlet 0 'ff'


do
    addNode 'aaa'
    let lastNode = getLastNode -- actually path
    addInlet lastNode
    setProcess lastNode (\ -> ...)
    pure nw


withLastNode (\node ->
    ...
    ...
)

```









```elm
-- https://elmlang.slack.com/archives/C0K777211/p1547472512008400
> graph = emptyAudioGraph
AudioGraph {
  connections = [],
  nodes = Dict.fromList [
    ("_output", Node { id = NodeID "_output", params = Dict.fromList [], type_ = Output })
  ]
} : AudioGraph

> osc = NodeID.fromString "myOsc" |> createOscillatorNode |> updateParam "frequency" (Frequency 440.0)
Node {
  id = NodeID "myOsc",
  params = Dict.fromList [
    ("detune",Value 0),
    ("frequency",Frequency 440),
    ("waveform",Waveform "sine")
  ],
  type_ = Oscillator
} : Node

> gain = NodeID.fromString "myGain" |> createGainNode |> updateParam "gain" (Value 0.5)
Node {
  id = NodeID "myGain",
  params = Dict.fromList [
    ("gain",Value 0.5)
  ],
  type_ = Gain
} : Node

> List.foldl addNode graph [osc, gain]
AudioGraph {
  connections = [],
  nodes = Dict.fromList [
    ("_output", Node {
      id = NodeID "_output",
      params = Dict.fromList [],
      type_ = Output }),
    ("myGain",Node {
      id = NodeID "myGain",
      params = Dict.fromList [("gain",Value 0.5)],
      type_ = Gain }),
    ("myOsc",Node {
      id = NodeID "myOsc",
      params = Dict.fromList [
        ("detune",Value 0),
        ("frequency",Frequency 440),
        ("waveform",Waveform "sine")
      ],
      type_ = Oscillator })
  ]
} : AudioGraph
```
