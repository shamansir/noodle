repeatX( reps = 3, offset )

// default
shape().repeatX(3.0, 0.0).out()

osc(5,0,1)
  .rotate(1.57)
  .repeatX([1,2,5,10], ({time}) => Math.sin(time))
  .out()