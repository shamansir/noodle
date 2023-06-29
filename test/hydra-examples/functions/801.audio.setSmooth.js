setSmooth( smooth = 0.4 )

// default
a.setSmooth(0.8)
osc().modulate(noise(3),()=>a.fft[0]).out(o0)