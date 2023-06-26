// Start webcam
s0.initCam();

// send webcam image to buffer o0
src(s0)
.out(o0);

// send oscillator to buffer o2
osc(
  60, 0, 0
).out(o2);

// take buffer o0
// blend it using diff
// send to buffer o1
src( o0 )
.diff(
  o2, 1
)
.out( o1 );

// render buffer o1
render( o1 );
