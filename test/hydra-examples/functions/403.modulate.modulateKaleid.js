modulateKaleid( texture, nSides = 4 )

// default
osc(9,-0.1,0.1)
  .modulateKaleid(osc(11,0.5,0),50)
  .scale(0.1,0.3)
  .modulate(noise(5,0.1))
  .mult(solid(1,1,0.3))
  .out(o0)

  osc(10,0.1,2)
  .modulateKaleid(osc(16).kaleid(999),1)
  .out(o0)