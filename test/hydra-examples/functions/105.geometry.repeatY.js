repeatY( reps = 3, offset )

// default
shape().repeatY(3.0, 0.0).out()

osc(5,0,1)
  .repeatY([1,2,5,10], ({time}) => Math.sin(time))
  .out()