// licensed with CC BY-NC-SA 4.0 https://creativecommons.org/licenses/by-nc-sa/4.0/
//Flor de Fuego

// https://hydra.ojack.xyz/?sketch_id=example_0

shape(200,0.5,1.5)
.scale(0.5,0.5)
.color([0.5,2].smooth(1),0.3,0)
.repeat(2,2)
.modulateScale(osc(3,0.5),-0.6)
.add(o0,0.5)
.scale(0.9)
.out()
