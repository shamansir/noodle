// licensed with CC BY-NC-SA 4.0 https://creativecommons.org/licenses/by-nc-sa/4.0/
//tropical juice
//by Ritchse
//instagram.com/ritchse

voronoi(2,0.3,0.2).shift(0.5)
.modulatePixelate(voronoi(4,0.2),32,2)
.scale(()=>1+(Math.sin(time*2.5)*0.05))
.diff(voronoi(3).shift(0.6))
.diff(osc(2,0.15,1.1).rotate())
.brightness(0.1).contrast(1.2).saturate(1.2)
	.out()
speed = 0.8

// https://hydra.ojack.xyz/?sketch_id=ritchse_2