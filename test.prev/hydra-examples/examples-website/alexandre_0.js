// licensed with CC BY-NC-SA 4.0 https://creativecommons.org/licenses/by-nc-sa/4.0/
// "the-wall"
// Alexandre Rangel
// www.alexandrerangel.art.br/hydra.html

speed=.0222
osc(48,-.1,0).thresh([.3,.7].fast(.75),0).color(0,0,1)

.add(
    osc(28,.1,0).thresh([.3,.7].fast(.75),0).rotate(3.14/4)
    .color(1,0,0)
    .modulateScale( osc(64,-.01,0).thresh([.3,.7].fast(.75),0) )
)
.diff(
    osc(28,.1,0).thresh([.3,.7].fast(.5),0).rotate(3.14/2)
    .color(1,0,1)
    .modulateScale( osc(64,-.015,0).thresh([.3,.7].fast(.5),0) )
)
.modulateRotate( osc(54,-.005,0).thresh([.3,.7].fast(.25),0) )
.modulateScale( osc(44,-.020,0).thresh([.3,.7].fast(.25),0) )
.colorama( ()=>Math.sin(time/27)*.01222+9.89)
.scale(2.122)

.out()
// https://hydra.ojack.xyz/?sketch_id=alexandre_0