a.show();
a.setBins(4);

shape( 3 )
.rotate(

  () => Math.PI * mouse.x / 180

)
.repeatX( 3 )
.repeatY( 3 )
.scale(

  () => a.fft[0]*4

)
.out( o0 );

render(o0);
