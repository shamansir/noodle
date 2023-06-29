scrollX( scrollX = 0.5, speed )

// default
osc(10,0,1).scrollX(0.5,0).out(o0)

// x position
osc(10,0,1).scrollX([0,0.25,0.5,0.75,1].fast(4),0).out(o0)

// scroll speed
gradient(1).scrollX(0, () => Math.sin(time*0.05)*0.05 ).out(o0)

gradient(0.125)
  .scrollX(0, ({time}) => Math.sin(time*0.05)*0.05 )
  .scrollY(0, ({time}) => Math.sin(time*0.01)*-0.07 )
  .pixelate([5,2,10],[15,8])
  .scale(0.15)
  .modulate(noise(1,0.25))
  .out()