setScale( scale = 10 )

// the smaller the scale is, the bigger the output is
a.setScale(5)
osc().modulate(noise(3),()=>a.fft[0]).out(o0)