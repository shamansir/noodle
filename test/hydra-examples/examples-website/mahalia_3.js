// licensed with CC BY-NC-SA 4.0 https://creativecommons.org/licenses/by-nc-sa/4.0/
// by Mahalia H-R
// IG: mm_hr_

shape(()=>Math.sin(time)+1*3, .5,.01)
.repeat(5,3, ()=>a.fft[0]*2, ()=>a.fft[1]*2)
.scrollY(.5,0.1)
.layer(
  src(o1)
  .mask(o0)
  .luma(.01, .1)
  .invert(.2)
)
.modulate(o1,.02)
.out(o0)

osc(40, 0.09, 0.9)
.color(.9,0,5)
.modulate(osc(10).rotate(1, 0.5))
.rotate(1, 0.2)
.out(o1)

render(o0)
// https://hydra.ojack.xyz/?sketch_id=mahalia_3