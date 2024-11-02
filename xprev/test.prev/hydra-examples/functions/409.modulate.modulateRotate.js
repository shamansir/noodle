modulateRotate( texture, multiple = 1, offset )

// wormhole
voronoi(100,3,5)
  .modulateRotate(osc(1,0.5,0).kaleid(50).scale(0.5),15,0)
  .mult(osc(50,-0.1,8).kaleid(9))
  .out(o0)

osc().modulateRotate(shape(999,0.3,0.5),1.57).out(o0)