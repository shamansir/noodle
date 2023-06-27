module Osc01 where


import Prelude (Unit, discard, (#), ($))
import Prelude (show) as Core

import Effect (Effect)
import Effect.Console as Console


import Toolkit.Hydra2.Lang

example :: Program Unit
example = do

    s0 # initCam

    a # show
    a # setBins 3

    osc (n 60.0) (n 0.1) (n 0.0)
        # modulate (src s0) (n 2.0)
        # saturate (n 0.7)
        # pixelate (n 10.0) (n 15.0)
        # scale (fn $ \_ -> a # fft 0)
        # out o0

    o0 # render


main :: Effect Unit
main =
    Console.log $ Core.show example