// licensed with CC BY-NC-SA 4.0 https://creativecommons.org/licenses/by-nc-sa/4.0/
// @naoto_hieda
osc(20, 0.1, 0).color(0, 1, 2).rotate(1.57/2).out(o1)
osc(30, 0.01, 0).color(2, 0.7, 1).modulate(o1, 0).add(o1,1).modulatePixelate(o1,1,10).out(o0)
// https://hydra.ojack.xyz/?sketch_id=naoto_0