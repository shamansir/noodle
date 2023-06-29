mask( texture )

// default
gradient(5).mask(voronoi(),3,0.5).invert([0,1]).out(o0)

// mask is transparent; compare with mult
osc()
  .layer(osc(30,0.1,2).mask(shape(4)))
  .out(o0)

// algae pulse
osc(10,-0.25,1).color(0,0,1).saturate(2).kaleid(50)
  .mask(noise(25,2).modulateScale(noise(0.25,0.05)))
  .modulateScale(osc(6,-0.5,2).kaleid(50))
  .mult(osc(3,-0.25,2).kaleid(50))
  .scale(0.5,0.5,0.75)
  .out(o0)