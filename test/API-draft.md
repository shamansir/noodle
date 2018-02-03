What would be cool to have in API:

```purescript
let
	myPatch = patch “MyPatch”
	sumNode = node SumNode
	inletA = getInlet “a” sumNode
	inletB = getInlet “b” sumNode
	myCustomNode = node CustomNode “Custom”
		|> addInlet (inlet NumberChannel “a” |> allow
            [ StringChannel fromString ])
		|> addInlet (inlet NumberChannel “b” |> default 10)
		|> addOutlet (outlet NumberChannel “out “)
		|> process (\inlets -> { out: inlets.a * inlets.b })
in
	inletA |> send 10 |> send 20
    inletB |> send 10 |> send 10 |> send 5
    myCustomNode |> getInlet "a" |> send "12" |> send 11
    myCustomNode |> getOutlet "out" |> connect (sumNode |> getInlet "a")
    myPatch |> addNode sumNode
    myCustomNode |> getInlet "b" |> send 13
	network = init |> addPatch myPatch
	run network

network.messages ==
    signal with [ CreateNetwork, AddPatch …, AddNode …,  ]

log network.messages = "CreateNetwork\nAdd Patch ...\nAddNode ...\n"

network.data ==
    signal with:
        [ Flow "MyPatch" "sumNode" "a" 10
        , Flow "MyPatch" "sumNode" "a" 20
        , Flow "MyPatch" "sumNode" "b" 10
        , Flow "MyPatch" "sumNode" "sum" 30
        , Flow "MyPatch" "sumNode" "b" 10
        , Flow "MyPatch" "sumNode" "sum" 30
        , Flow "MyPatch" "sumNode" "b" 5
        , Flow "MyPatch" "sumNode" "sum" 25
        , Flow "MyPatch" "customNode" "a" 12
        , Flow "MyPatch" "customNode" "out" 120
        , Flow "MyPatch" "customNode" "a" 11
        , Flow "MyPatch" "customNode" "out" 110
        , Flow "MyPatch" "customNode" "b" 13
        , Flow "MyPatch" "customNode" "out" 143
        , Flow "MyPatch" "sumNode" "a" 143
        , Flow "MyPatch" "sumNode" "sum" 148
        ]
```

Read some file -> Translate it into messages -> Build the network
