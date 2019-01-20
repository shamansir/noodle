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
