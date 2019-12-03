precision highp float;

uniform vec2 iResolution;
uniform float iTime;
uniform vec4 iMouse;

uniform sampler2D iChannel0; // /images/h157-small.png
uniform vec2 iChannel0Size;

uniform sampler2D iChannel1; // /images/h157-globe.png
uniform vec2 iChannel1Size;

uniform sampler2D iChannel2; // /images/globe.png
uniform vec2 iChannel2Size;


// Adapted from https://www.shadertoy.com/view/WdB3Dw

#define PI 3.14159265359
#define fTime mod(iTime / 3.5, 1.)
// #define fTime mod(iTime / 3.5 - 1., 3.)
//#define LOOP

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

vec2 warpLogo(vec2 uv) {
    float l = length(uv);
    l /= 20.;
    l -= fTime;
    // l -= .7;
    l += 1.;
    l = 1. - l;

    float ww = smoothstep(.05, .15, l*.75) - smoothstep(.16, .5, l*.75);
    float w = smoothstep(0., .2, l) - smoothstep(.2, 1., l);
    float wt = smoothstep(.2, .3, fTime) - smoothstep(.3, 1., fTime);

    uv += sin(l * 150.) * ww * vec2(-1,1) * .02 * (1. - length(uv) / 3.);
    // uv += sin(uv * 100. + fTime * 100. * vec2(-1,1)) * w * vec2(-1,1) * .01;


    // uv += sin(l * 50.) * .01 * w * vec2(-1,1);

    // return uv;

    // vec2 po = vec2(length(uv), atan(uv.x, uv.y));
    uv += sin(uv * 10. + fTime * 100. * vec2(-1,1)) * .01 * wt * (1. - length(uv) / 3.);

    // uv.y += clamp((1. - length(uv)/2.), 0., 1.);
    // uv.y += 1.;

    return uv;
}

vec4 texture2Dc(sampler2D s, vec2 uv) {
    if (uv.x < 0. || uv.y < 0. || uv.x > 1. || uv.y > 1.) {
        return vec4(0);
    }
    return texture2D(s, uv);
}

float drawLogo(vec2 uv) {

    uv.x -= .2;
    uv.y += .2;
    uv *= 1.4;

    vec2 uvv = uv;

    uv = uvv;
    
    uv.x += .675;
    uv.y -= .24;
    uv *= 1.89;
    uv /= vec2(1, -iChannel2Size.y / iChannel2Size.x) * 2.;
    uv += .5;
    vec4 tex3 = texture2Dc(iChannel2, uv);
    // return tex3.a;

    uv = uvv;
    uv /= vec2(1, -iChannel0Size.y / iChannel0Size.x) * 2.;
    uv += .5;
    vec4 tex = texture2D(iChannel0, uv);

    // return tex.a;

    return max(tex.a, tex3.a);

    uv = uvv;
    uv.y -= .23;
    uv.x += .105;
    uv *= .91;
    uv /= vec2(1, -iChannel1Size.y / iChannel1Size.x) * 2.;
    uv += .5;
    vec4 tex2 = texture2D(iChannel1, uv);


    return min(tex2.r, 1.-tex3.a);


    // float globe = (1.-tex2.r) / 2.;


    // return globe * 2.;

    // return max(tex.a, globe);
}

void main() {
    vec2 p = (-iResolution.xy + 2. * gl_FragCoord.xy) / iResolution.x;

    // gl_FragColor = texture2Dc(iChannel2, p); return;
    p.x += .14;
    p.y -= .03;

    p.x += .28;
    p.y += .03;
    p *= 1.83;

    vec3 pos;
    float rayLength = 0.;
    float dist = 0.;

    vec3 origin = vec3(0,.0,2.9);
    
    vec2 rot = vec2(.525,-.41);
    pR(origin.zy, rot.y*1.5);
    pR(origin.zx, rot.x*1.5);
    
    mat3 camMat = calcLookAtMatrix(origin, vec3(0), vec3(0,1,0));
    vec3 rd = normalize(camMat * vec3(p, 4.));

    vec3 color = vec3(10,0,12)*.0007;
    color *= 0.;
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

    p = warpLogo(p);

    p /= 1.83;
    p.x -= .28;
    p.y -= .03;

    float logo = drawLogo(p);

    color = mix(color, vec3(1), logo);

    // color = mix(vec3(0), vec3(1), logo);

    gl_FragColor = vec4(color,1);
}