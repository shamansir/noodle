pixelate( pixelX = 20, pixelY = 20 )

// default
noise().pixelate(20,20).out(o0)

noise().pixelate(2000,1).out(o0)

noise()
  .mult(osc(10,0.25,1))
  .scrollY(1,0.25)
  .pixelate([100,40,20,70].fast(0.25))
  .modulateRotate(src(o0).scale(0.5),0.125)
  .diff(src(o0).rotate([-0.05,0.05].fast(0.125)))
    .out(o0)