noise( scale = 10, offset = 0.1 )

// default
noise(10, 0.1).out(o0)

// noise interpolating between different scales and offsets
noise( () => Math.sin(time/10)*50 , () => Math.sin(time/2)/500 )
.out(o0)