// licensed with CC BY-NC-SA 4.0 https://creativecommons.org/licenses/by-nc-sa/4.0/
// Mahalia H-R
// IG: @mm_hr_

shape(20,0.1,0.01)
  .scale(() => Math.sin(time)*3)
  .repeat(() => Math.sin(time)*10)
  .modulateRotate(o0)
  .scale(() => Math.sin(time)*2)
  .modulate(noise(2,0))
  .rotate(0.1, 0.9)
.out(o0)

src(o0)
.modulate(osc(500,0,0))
.out(o1)

src(o1)
.modulateKaleid(voronoi(() => Math.sin(time)*3,0.1,0.01),() => Math.sin(time)*3)
.scale(() => Math.sin(time)*3)
.out(o2)

render(o2)
