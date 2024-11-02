setFunction( options )

// from https://www.shadertoy.com/view/XsfGzn
setFunction({
    name: 'chroma',
    type: 'color',
    inputs: [
      ],
    glsl: `
     float maxrb = max( _c0.r, _c0.b );
     float k = clamp( (_c0.g-maxrb)*5.0, 0.0, 1.0 );
     float dg = _c0.g;
     _c0.g = min( _c0.g, maxrb*0.8 );
     _c0 += vec4(dg - _c0.g);
     return vec4(_c0.rgb, 1.0 - k);
  `})
  osc(60,0.1,1.5).chroma().out(o0)