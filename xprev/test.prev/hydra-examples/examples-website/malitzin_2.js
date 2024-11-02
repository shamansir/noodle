// licensed with CC BY-NC-SA 4.0 https://creativecommons.org/licenses/by-nc-sa/4.0/
//CNDSD
//http://malitzincortes.net/
//crazy squares

shape(4, (0.01, ()=> 0.2 + a.fft[2]),1)
.mult(osc(1, 1).modulate(osc(5).rotate(1.4,1),3))
.color(1,2,4)
.saturate(0.2)
.luma(1.2,0.05, (5, ()=> 2 + a.fft[3]))
.scale(0.6, ()=> 0.9 + a.fft[3])
.diff(o0)// o0
.out(o0)// o1
