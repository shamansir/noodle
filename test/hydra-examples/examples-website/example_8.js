// licensed with CC BY-NC-SA 4.0 https://creativecommons.org/licenses/by-nc-sa/4.0/
// by Rodrigo Velasco
// https://yecto.github.io/

osc(107, 0, 0.7).color(1, 0, 1).rotate(0, -0.08).modulateRotate(o1, 0.4).out(o0)
osc(33).rotate(2, 0.8).modulateRotate(o0, () => (a.fft[0]*2)).out(o1)
