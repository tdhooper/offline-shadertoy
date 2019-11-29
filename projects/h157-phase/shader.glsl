precision highp float;

uniform vec2 iResolution;
uniform float iTime;
uniform vec4 iMouse;

// Adapted from https://www.shadertoy.com/view/WdB3Dw

#define PI 3.14159265359
#define fTime mod(iTime / 3.5, 1.)


// https://www.shadertoy.com/view/ll2GD3
vec3 pal( in float t, in vec3 a, in vec3 b, in vec3 c, in vec3 d ) {
    return a + b*cos( 6.28318*(c*t+d) );
}
vec3 spectrum(float n) {
    return pal( n, vec3(0.5,0.5,0.5),vec3(0.5,0.5,0.5),vec3(1.0,1.0,1.0),vec3(0.0,0.33,0.67) );
}

// HG_SDF
void pR(inout vec2 p, float a) {
    p = cos(a)*p + sin(a)*vec2(p.y, -p.x);
}

float map(vec3 p) {
    return length(p) - .5;
}

mat3 calcLookAtMatrix(vec3 ro, vec3 ta, vec3 up) {
    vec3 ww = normalize(ta - ro);
    vec3 uu = normalize(cross(ww,up));
    vec3 vv = normalize(cross(uu,ww));
    return mat3(uu, vv, ww);
}

const float ITER = 400.;
const float FUDGE_FACTORR = .25;
const float INTERSECTION_PRECISION = .0001;
const float MAX_DIST = 4.;

void main() {
    vec2 p = (-iResolution.xy + 2. * gl_FragCoord.xy) / iResolution.y;
    
    vec3 pos;
    float rayLength = 0.;
    float dist = 0.;

    vec3 origin = vec3(0,.0,2.9);
    
    vec2 rot = vec2(.525,-.41);
    vec2 im = (iMouse.xy / iResolution.xy) * 2. - 1.;
    if (im.x > -1. && im.y > -1.) {
       rot += im;
    }
    pR(origin.zy, rot.y*1.5);
    pR(origin.zx, rot.x*1.5);
    
    mat3 camMat = calcLookAtMatrix(origin, vec3(0), vec3(0,1,0));
    vec3 rd = normalize(camMat * vec3(p, 4.));

    vec3 color = vec3(10,0,12)*.0007;
    vec3 c;    

    for (float i = 0.; i < ITER; i++) {

        // Step a little slower so we can accumilate glow
        rayLength += max(INTERSECTION_PRECISION, abs(dist) * FUDGE_FACTORR);
        pos = origin + rd * rayLength;

        // warp space
        float w = smoothstep(0., .2, fTime) - pow(smoothstep(.2, 1., fTime), 1.);
        float q = smoothstep(0., .2, fTime) * .5 + smoothstep(.2, 1., fTime) * .5;
        pos += sin((pos) * mix(10., 100., fTime)) * mix(.0, .05, w);
        
        dist = map(pos);

        // Add a lot of light when we're really close to the surface
        c = vec3(max(0., .001 - abs(dist)) * .5);
        c *= vec3(1.4,2.1,1.7); // blue green tint

        // Accumilate some purple glow for every step
        c += vec3(.6,.25,.7) * FUDGE_FACTORR / 160.;
        c *= smoothstep(20., 7., length(pos));

        // Fade out further away from the surface
        c *= smoothstep(.01, .0, dist) * .5;
        
        // Vary colour as we move through space
        c *= spectrum(length(sin(pos - vec3(.45,0,0))) * 10. - .6 - fTime*5.);

        color += c;
        
        if (rayLength > MAX_DIST) {
            break;
        }
    }

    // Tonemapping and gamma
    color = pow(color, vec3(1. / 1.8)) * 2.;
    color = pow(color, vec3(2.)) * 3.;
    color = pow(color, vec3(1. / 2.2));


    gl_FragColor = vec4(color,1);
}