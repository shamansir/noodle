// licensed with CC BY-NC-SA 4.0 https://creativecommons.org/licenses/by-nc-sa/4.0/
//Glitch River
//Flor de Fuego
//https://flordefuego.github.io/
voronoi(8,1)
.mult(osc(10,0.1,()=>Math.sin(time)*3).saturate(3).kaleid(200))
.modulate(o0,0.5)
.add(o0,0.8)
.scrollY(-0.01)
.scale(0.99)
.modulate(voronoi(8,1),0.008)
.luma(0.3)
.out()


speed = 0.1
