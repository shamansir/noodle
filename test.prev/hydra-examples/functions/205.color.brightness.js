brightness( amount = 0.4 )

// default
osc(20,0,2)
  .brightness( () => Math.sin(time) )
  .out(o0)

// scaling noise value to 0-1
noise().brightness(1).color(0.5,0.5,0.5).out(o0)