scale( amount = 1.5, xMult = 1, yMult = 1, offsetX = 0.5, offsetY = 0.5 )

// default
shape().scale(1.5,1,1).out(o0)

// flower
shape().scale(1.5,[0.25,0.5,0.75,1].fast(0.25),[3,2,1])
  .invert([0,1].fast(0.25))
  .kaleid(5)
  .kaleid(12)
  .scale( ()=>Math.sin(time/5)*0.5 )
  .rotate(1,1)
  .out(o0)