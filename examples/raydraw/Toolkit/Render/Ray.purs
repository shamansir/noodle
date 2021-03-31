module RayDraw.Toolkit.Render where 

import Data.Unit (Unit)
import Effect (Effect)
import Effect.Console (log)

foreign import renderNativeRay :: Number -> Effect Unit


vertexShader :: String
vertexShader = """
        vec3 mod289(vec3 x) { return x - floor(x * (1.0 / 289.0)) * 289.0; }
        vec2 mod289(vec2 x) { return x - floor(x * (1.0 / 289.0)) * 289.0; }
        vec3 permute(vec3 x) { return mod289(((x*34.0)+1.0)*x); }

        float snoise(vec2 v) {
            // vec2 v = vec2(inp);
            vec4 C = vec4(0.211324865405187,  // (3.0-sqrt(3.0))/6.0
                                0.366025403784439,  // 0.5*(sqrt(3.0)-1.0)
                                -0.577350269189626,  // -1.0 + 2.0 * C.x
                                0.024390243902439); // 1.0 / 41.0
            vec2 i  = floor(v + dot(v, C.yy) );
            vec2 x0 = v -   i + dot(i, C.xx);
            vec2 i1;
            i1 = (x0.x > x0.y) ? vec2(1.0, 0.0) : vec2(0.0, 1.0);
            vec4 x12 = x0.xyxy + C.xxzz;
            x12.xy -= i1;
            i = mod289(i); // Avoid truncation effects in permutation
            vec3 p = permute( permute( i.y + vec3(0.0, i1.y, 1.0 ))
                + i.x + vec3(0.0, i1.x, 1.0 ));

            vec3 m = max(0.5 - vec3(dot(x0,x0), dot(x12.xy,x12.xy), dot(x12.zw,x12.zw)), 0.0);
            m = m*m ;
            m = m*m ;
            vec3 x = 2.0 * fract(p * C.www) - 1.0;
            vec3 h = abs(x) - 0.5;
            vec3 ox = floor(x + 0.5);
            vec3 a0 = x - ox;
            m *= 1.79284291400159 - 0.85373472095314 * ( a0*a0 + h*h );
            vec3 g;
            g.x  = a0.x  * x0.x  + h.x  * x0.y;
            g.yz = a0.yz * x12.xz + h.yz * x12.yw;
            return 130.0 * dot(m, g);
        }

        uniform float posX;
        uniform float posY;
        varying vec2 vUv;
        varying float noise;
        varying float color1Intensity;
        varying float color2Intensity;

        void main() {
            vUv = uv;

            float i = floor(position.y * 1.0) * 1.0;
            float f = fract(position.x * position.x * 1.0) * 1.0;
            // noise = mix(snoise(i), snoise(i + .20), smoothstep(0., 1., f));
            float timeDisplacement = (sin(posX) + 1.0) / 2.0;
            float timeDisplacement2 = (cos(posY) + 1.0) / 2.0;
            noise = snoise(uv * 1.7 + timeDisplacement * 0.1);
            float noise2 = snoise(uv * 1.7 + timeDisplacement2 * 0.1);
            color1Intensity = snoise(uv * 1.7 + timeDisplacement2 * 0.08);
            color2Intensity = snoise(uv * 1.8 + timeDisplacement * 0.12);

            vec3 pos = position;
            // pos.xyz += noise * 12.0;
            pos.x += noise * 12.0;
            pos.y += noise * 12.0;
            pos.z += noise2 * 5.0;

            gl_Position = projectionMatrix * modelViewMatrix * vec4(pos, 1.0);
        }
"""

fragmentShader :: String 
fragmentShader = """
        float lighten(float base, float blend) {
            return max(blend,base);
        }

        vec3 lighten(vec3 base, vec3 blend) {
            return vec3(lighten(base.r,blend.r),lighten(base.g,blend.g),lighten(base.b,blend.b));
        }

        vec3 lighten(vec3 base, vec3 blend, float opacity) {
            return (lighten(base, blend) * opacity + base * (1.0 - opacity));
        }

        varying vec2 vUv;
        varying float noise;
        varying float color1Intensity;
        varying float color2Intensity;

        void main() {
            vec3 color = vec3(noise);
            color = mix(color, vec3(1.0), 0.4);
            color = mix(color, vec3(1., 0.2, 0.5), 1.0 - (vUv.x * 0.6 + 0.1));
            color = mix(color, vec3(0.5, 0.2, 1.0), vUv.y / 2.0 + 0.25);
            color = mix(color, vec3(0.7, 0.4, 1.0), color1Intensity * 0.5);
            color = lighten(color, vec3(0.5, 0.2, 0.8), 0.9);

            gl_FragColor = vec4(color, 1.0);
        }
"""

renderRay :: Number -> Effect Unit
renderRay x = do 
    log ("calling renderRay")