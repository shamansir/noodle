setBins( numBins = 4 )

// change color with hissing noise
a.setBins(8)
osc(60,0.1,()=>a.fft[7]*3).modulate(noise(3),()=>a.fft[0]).out(o0)