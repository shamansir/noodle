// licensed with CC BY-NC-SA 4.0 https://creativecommons.org/licenses/by-nc-sa/4.0/
// Velvet Pool
// by Mahalia H-R
// IG: mm_hr_


noise()
.color(() => a.fft[2]*2,0,.6)
.modulate(noise(() => a.fft[0]*10))
.scale(()=> a.fft[2]*5)
.layer(
  src(o0)
  .mask(osc(10).modulateRotate(osc(),90,0))
  .scale(() => a.fft[0]*2)
  .luma(0.2,0.3)
)
.blend(o0)
.out(o0)

osc()
.modulate(noise(() => a.fft[1]+5))
.color(1,0,0)
.out(o1)

src(o0)
.modulate(o1)
.layer(
  src(o1)
  .mask(o1)
  .saturate(7)
)
.modulateRotate(o1)
.rotate(({time}) => time%360*0.05)
.out(o2)

render(o2)

// https://hydra.ojack.xyz/?sketch_id=mahalia_2