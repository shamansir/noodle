// licensed with CC BY-NC-SA 4.0 https://creativecommons.org/licenses/by-nc-sa/4.0/
// acid bus seat
// by Will Humphreys
// https://github.com/TheWispy

osc(105).color(0.5,0.1,0.8).rotate(0.11, 0.1).modulate(osc(10).rotate(0.3).add(o0, 0.1)).add(osc(20,0.01,1).color(0,0.8,1)).out(o0)
osc(50,0.05, 0.7).color(1,0.7,0.5).diff(o0).modulate(o1,0.05).out(o1)
render(o1)

// https://hydra.ojack.xyz/?sketch_id=example_13