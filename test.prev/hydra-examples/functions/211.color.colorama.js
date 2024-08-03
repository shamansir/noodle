colorama( amount = 0.005 )

// // 20Hz oscillator source
// // color sequence of Red, Green, Blue, White, Black
// // colorama sequence of 0.005, 0.5, 1.0 at 1/8 speed
// // output to buffer o0
osc(20)
  .color([1,0,0,1,0],[0,1,0,1,0],[0,0,1,1,0])
  .colorama([0.005,0.33,0.66,1.0].fast(0.125))
  .out(o0)

// negative value is less harsh
osc(30,0.1,1).colorama(-0.1).out(o0)