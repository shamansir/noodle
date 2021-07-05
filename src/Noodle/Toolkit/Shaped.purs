module Noodle.Toolkit.Shaped where


import Noodle.Toolkit as T
import Noodle.Node (Node)
import Noodle.Node.Shape as Node


type Toolkit d = T.Toolkit d (Node.Shape d)


type Renderer d view = T.Renderer d (Node.Shape d) view