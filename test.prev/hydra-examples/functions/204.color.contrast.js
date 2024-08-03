contrast( amount = 1.6 )

// 20Hz oscillator with contrast interpolating between 0.0-5.0
osc(20).contrast( () => Math.sin(time) * 5 ).out(o0)