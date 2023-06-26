// licensed with CC BY-NC-SA 4.0 https://creativecommons.org/licenses/by-nc-sa/4.0/
// disintegration
// by Ritchse
// instagram.com/ritchse

osc(5,.1).modulate(noise(6),.22).diff(o0)
  	.modulateScrollY(osc(2).modulate(osc().rotate(),.11))
	.scale(.72).color(0.99,1.014,1)
  	.out()

// https://hydra.ojack.xyz/?sketch_id=ritchse_4