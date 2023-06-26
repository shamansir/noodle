s0.initCam();

a.show();
a.setBins(3);

osc( 60, 0.1, 0 )
.modulate(
  src(s0) , 2
)
.saturate( 0.7 )
.pixelate( 10 , 15 )
.scale(
  () => a.fft[0]
).out( o0 );

render(o0);
