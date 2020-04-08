// framebuffer size: 512x512

precision highp float;

uniform vec2 iResolution;
uniform float iTime;

uniform sampler2D iChannel0; // images/grey-noise-small.png filter: nearest wrap: repeat
uniform vec2 iChannel0Size;


varying vec3 eye;
varying vec3 dir;

void mainImage(out vec4 a, in vec2 b);

void main() {
    mainImage(gl_FragColor, gl_FragCoord.xy);
}


/*

    FBM Domain Warp Texture
    -----------------------

	Based on "bp Turbulence" by blackpolygon https://shadertoy.com/view/Ml3SRf

	I've picked a nice looking area of the texture for the default loop,
    if you get bored of that, press 'l' and it'll show more variation.

*/


#define PI 3.14159265359
#define TWO_PI 6.28318530718

float random (in vec2 _st) { 
    #ifdef GIF_EXPORT
    	return fract(sin(dot(_st.xy, vec2(12.9898,78.233))) * 43758.54531237);
   	#endif
    vec2 uv = _st / iChannel0Size.xy;
   // uv.y = 1. - uv.y;
    return texture2D(iChannel0, uv).r;
}

// Based on Morgan McGuire @morgan3d
// https://www.shadertoy.com/view/4dS3Wd
float noise (in vec2 _st) {
    vec2 i = floor(_st);
    vec2 f = fract(_st);

    // Four corners in 2D of a tile
    float a = random(i);
    float b = random(i + vec2(1.0, 0.0));
    float c = random(i + vec2(0.0, 1.0));
    float d = random(i + vec2(1.0, 1.0));

    vec2 u = f * f * (3. - 2.0 * f);

    return mix(a, b, u.x) + 
            (c - a)* u.y * (1. - u.x) + 
            (d - b) * u.x * u.y;
}

#define NUM_OCTAVES 9

float fbm ( in vec2 _st) {
    float v = 0.0;
    float a = 0.5;
    vec2 shift = vec2(20.0);
    // Rotate to reduce axial bias
    mat2 rot = mat2(cos(0.5), sin(0.5), 
                    -sin(0.5), cos(0.50));
    for (int i = 0; i < NUM_OCTAVES; ++i) {
        v += a * noise(_st);
        _st = rot * _st * 2.2 + shift;
        a *= 0.5;
    }
    return v;
}

const int KEY_L = 76;

vec3 pattern(vec2 st) {
    
    #ifdef GIF_EXPORT
        st = st.yx * .42;
    	st.x -= .015;
        st += vec2(9.24,9.-.07);
   	#else
        st *= .3;
        st += vec2(-6.7,4.7);// + ((iMouse.xy / iResolution.xy) - .5) * -1.5;
   	#endif
    
    vec3 color = vec3(0.);
    vec2 a = vec2(0.);
    vec2 b = vec2(0.);
    vec2 c = vec2(60.,800.);
    
    a.x = fbm( st);
    //return vec3(pow(a.x*3., 10.));
    a.y = fbm( st + vec2(1.0));
    
    b.x = fbm( st + 4.*a);
    b.y = fbm( st);

    float fTime = 0.;

    //c.x = fbm( sin(mod(st + 7.0 * b + fTime, 1.) * PI * 2.)*.3 );
   // c.y = fbm( sin(mod(st + 3.944 * b + fTime, 1.) * PI * 2.)*.3 );

    c.x = fbm( sin(mod(st + 7.0 * b + fTime, 1.) * PI * 2.) );
    c.y = fbm( sin(mod(st + 3.944 * b + fTime, 1.) * PI * 2.) );

    float f = fbm(st+b+c);
    //return vec3(f);

    f *= 1.3;
    f = pow(f, 3.)*1.9;

    
    color = mix(vec3(0.445,0.002,0.419), vec3(1.000,0.467,0.174), clamp((f*f),0.2, 1.0));
    color = mix(color, vec3(0.413,0.524,0.880), clamp(length(c.x),0.480, 0.92));
    color *= f * 1.9;
    
    return color;
}

void mainImage( out vec4 fragColor, in vec2 fragCoord ) {
    vec2 ir = iResolution.xy;
    vec2 p = (fragCoord.xy - 0.5*ir.xy )/min(ir.x,ir.y);
   // p.x /= (ir.x/ir.y) / (640./360.);
    p *= 1.5;
    // p /= 20.;
    fragColor = vec4(pattern(p), 1.);
}
