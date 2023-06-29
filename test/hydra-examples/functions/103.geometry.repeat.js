repeat( repeatX = 3, repeatY = 3, offsetX, offsetY )

// default
shape().repeat(3.0, 3.0, 0.0, 0.0).out()

// dogtooth factory
shape(1.25,0.5,0.25)
  .repeat(3, 3)
  .scale(2)
  .repeat(5, 5, () => Math.sin(time), () => Math.sin(time/2))
  .out(o0)