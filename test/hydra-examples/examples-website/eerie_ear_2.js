// licensed with CC BY-NC-SA 4.0 https://creativecommons.org/licenses/by-nc-sa/4.0/
//ee_5 . FUGITIVE GEOMETRY VHS . audioreactive shapes and gradients
// e_e // @eerie_ear
//
s= ()=>
  shape(4)
.scrollX([-0.5,-0.2,0.3,-0.1,-0.1].smooth(0.1).fast(0.3))
.scrollY([0.25,-0.2,0.3,-0.1,0.2].smooth(0.9).fast(0.15))
//
solid()
.add(gradient(3,0.05).rotate(0.05,-0.2).posterize(2).contrast(0.6),[1,0,1,0.5,0,0.6].smooth(0.9))
.add(s())
.mult(s().scale(0.8).scrollX(0.01).scrollY(-0.01).rotate(0.2,0.06).add(gradient(3).contrast(0.6),[1,0,1,0.5].smooth(0.9),0.5).mult(src(o0).scale(0.98),()=>a.fft[0]*9)
     )
.diff(s().modulate(shape(500)).scale([1.7,1.2].smooth(0.9).fast(0.05)))
.add(gradient(2).invert(),()=>a.fft[2])
.mult(gradient(()=>a.fft[3]*8))
.blend(src((o0),()=>a.fft[1]*40))
.add(voronoi(()=>a.fft[1],()=>a.fft[3],()=>a.fft[0]).thresh(0.7).posterize(2,4).luma(0.9).scrollY(1,()=>a.fft[0]/30).colorama(3).thresh(()=>a.fft[1]).scale(()=>a.fft[3]*2),()=>a.fft[0]/2)
  .out()
//
speed= 1

a.setSmooth(0.96)
