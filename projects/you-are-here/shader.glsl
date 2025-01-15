#version 300 es

precision highp float;

uniform vec2 iResolution;
uniform sampler2D iChannel0; // blur-y.glsl filter: linear wrap: clamp
uniform sampler2D iChannel1; // buffer-a.glsl filter: linear wrap: clamp
uniform int drawIndex;

out vec4 fragColor;

void mainImage(out vec4 a, in vec2 b);

void main() {
    mainImage(fragColor, gl_FragCoord.xy);
}

#ifdef GL_ES
precision mediump float;
#endif

/*

    You Are Here
    ------------

    I created this sofa for the room in my Revision 2021 Animated Gif
    entry, and went a little over the top with detailing so felt it
    deserved its own 4k Graphics entry!
    
    This is modified a little but from the compo version, I've added
    feet, and made the wood shinier.
    
    Massive thanks to yx for sharing her framework, Blossom, without
    which I probably wouldn't have got this into the competition:
    
    https://github.com/lunasorcery/Blossom
    
    The edge wrinkles are created by modulating the radius of corners
    and smooth unions, which is a technique Blackle demonstrated for
    the welds in 'Bending an SDF':
    
    https://www.shadertoy.com/view/3llfRl
    
    Surface wrinkles use the same modulation, but as a height map; and
    the cloth texture is created by warping the whole model with a high
    frequency sine.
    
    Wood grain is concentric circles warped with more sines, which also
    alters the specular response, as shown by LucaRood:
    
    https://twitter.com/LucaRood/status/1375369105327464448
 
    Clouds are stylised as I knew I couldn't make some that looked
    as realistic as the models. They are formed by swirling space in
    a hexagon arrangement, at different scales, over the top of a simple
    light-to-dark blob.
 
*/

vec3 aces(vec3 x) {
  const float a = 2.51;
  const float b = 0.03;
  const float c = 2.43;
  const float d = 0.59;
  const float e = 0.14;
  return clamp((x * (a * x + b)) / (x * (c * x + d) + e), 0.0, 1.0);
}

// colour grading from tropical trevor's scripts
// https://github.com/trevorvanhoof/ColorGrading
float Luma(vec3 color) { return dot(color, vec3(0.2126, 0.7152, 0.0722)); }

void mainImage(out vec4 fragColor, in vec2 fragCoord) {
  vec2 uv = fragCoord.xy / iResolution.xy;
  vec3 col;

  if (drawIndex == 0) {
    col = texture(iChannel0, uv).rgb;
  } else {
    col = texture(iChannel1, uv).rgb;
  }

  vec3 uGain = vec3(1.8);
  vec3 uLift = vec3(.002,-.003,.007)/3.;
  vec3 uOffset = vec3(.00,.00,.00);
  vec3 uGamma = vec3(-.25);
  col = pow(max(vec3(0.0), col * (1.0 + uGain - uLift) + uLift + uOffset), max(vec3(0.0), 1.0 - uGamma));
  col = pow( col, vec3(1./2.2) );
  col = aces(col);

  fragColor = vec4(col, 1);
}

