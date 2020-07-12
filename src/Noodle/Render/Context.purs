module Noodle.Render.Context where


import Noodle.Util (Position)

import Noodle.Render.Toolkit (Renderer)


type Context d c n view =
    { mousePos :: Position
    , renderer :: Renderer d c n view
    }


