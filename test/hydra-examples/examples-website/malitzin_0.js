// licensed with CC BY-NC-SA 4.0 https://creativecommons.org/licenses/by-nc-sa/4.0/
//CNDSD
//http://malitzincortes.net/
// sand spirals

osc(3, 0.01, 0.4)
.color(1.2,1.2,1.3)
.saturate(0.4)
.modulateRepeat(osc(2),1, 2, 4, 3)
.modulateKaleid(osc(12,0.05,0),1)
.luma (0.4)
.rotate(4, 0.1,0)
.modulate(o0, () => mouse.y *0.0002 )
.scale(1).diff(o1)
.out(o0)
