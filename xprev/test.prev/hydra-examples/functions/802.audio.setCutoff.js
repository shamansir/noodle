setCutoff( cutoff = 2 )

// threshold
a.setCutoff(4)
osc().modulate(noise(3),()=>a.fft[0]).out(o0)