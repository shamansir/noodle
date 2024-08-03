// Oscillator
// osc( frequency, speed, offset )

osc(
  60, 0.01, 0
).out();


// Solid Layer
// solid( Red, Green, Blue)

solid(
  0, 0, 255
).out();


// Shape
// shape( sides, blur, scale )

shape( 3, 0, 0.8 )
.out();


// Noise
// noise( scale, speed )

noise(
  50, 0.1
).out();


// Voronoi
// voronoi( scale, speed )

voronoi(
  10, 0.1
).out();


// Gradient
// gradient( speed )

gradient(
  0
).out();
