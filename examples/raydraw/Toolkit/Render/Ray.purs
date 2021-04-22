module RayDraw.Toolkit.Render where 

import Prelude

import Effect (Effect)
import RayDraw.Toolkit.Value (RgbaColor(..))

foreign import renderNativeRay :: Vector3 -> Vector3 -> Vector3 -> String -> String -> Effect Unit

foreign import data Vector3 :: Type

foreign import createVec3 :: Number -> Number -> Number -> Vector3


vertexShader :: String
vertexShader = """        
    attribute float distance;

    varying float vDistance;
    varying vec3 vNormal;
    varying vec3 vViewPosition;

    void main() {

        vec4 mvPosition = modelViewMatrix * vec4( position, 1.0 );

        vDistance = distance;
        vNormal = normalize( normalMatrix * normal );
        vViewPosition = - mvPosition.xyz;

        gl_Position = projectionMatrix * mvPosition;

    }
"""

fragmentShader :: String 
fragmentShader = """
    uniform float fraction;

    varying float vDistance;
    varying vec3 vNormal;
    varying vec3 vViewPosition;
    
    uniform vec3 colorA;
    uniform vec3 colorB;
    uniform vec3 colorC;

    void main() {

        if ( vDistance > fraction ) discard;

        float h = 0.5;
        vec3 colorAB = mix(colorA, colorB, vDistance / h);
        vec3 colorBC = mix(colorB, colorC, (vDistance - h)/(1.0 - h));
        vec3 color = mix(colorAB, colorBC, step(h, vDistance));

        // hack in a fake pointlight at camera location, plus ambient
        vec3 normal = normalize( vNormal );
        vec3 lightDir = normalize( vViewPosition );

        //shadow
        //float dotProduct = max( dot( normal, lightDir ), .7 ) + 0.2;
        float dotProduct = 1.0;

        // trick to make the clipped ends appear solid
        gl_FragColor = ( gl_FrontFacing ) ? vec4( color * dotProduct, 1.0 ) : vec4( color, 1.0 );

    }
"""

renderRay :: RgbaColor -> RgbaColor -> RgbaColor -> Effect Unit
renderRay col1 col2 col3 = do 
    renderNativeRay (colorToVec col1) (colorToVec col2) (colorToVec col3) fragmentShader vertexShader

colorToVec :: RgbaColor -> Vector3
colorToVec (RgbaColor { r, g, b, a}) = createVec3 r g b