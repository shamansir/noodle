shape( sides = 3, radius = 0.3, smoothing = 0.01 )

// triangle
shape(3,0.5,0.001).out(o0)

// ellipse
shape(100,0.5,0.001).out(o0)

// inverting blurry circle
shape(100,0.01,1).invert(()=>Math.sin(time)*2).out(o0)

// a... rainbow ball?
shape(5,0.5,0.1).repeat(19,19)
  .mult(osc(10,1,2))
  .rotate( ({time}) => time%360 )
  .scrollX(1,-0.25)
  .mult(shape(15,0.3,0.01)
  .rotate( ({time}) => time%360 )
  .scrollX(1,-0.25))
  .out(o0)