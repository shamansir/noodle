modulateScrollX( texture, scrollX = 0.5, speed )

// default
voronoi(25,0,0)
  .modulateScrollX(osc(10),0.5,0)
  .out(o0)

// different scroll and speed
voronoi(25,0,0)
  .modulateScrollX(osc(10),0.5,0.25)
  .out(o0)