module Noodle.Node2.Path where


import Noodle.Id (NodeId, Input', Output')


data InNode f i o
    = NodeP (NodeId f)
    | InputP (NodeId f) (Input' i)
    | OutputP (NodeId f) (Output' o)