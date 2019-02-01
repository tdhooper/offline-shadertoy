#ifdef GL_ES
precision mediump float;
#endif

uniform float iTime;

uniform vec2 iResolution;
uniform vec4 iMouse;

uniform float guiLead;
uniform float guiRadius;

void mainImage(out vec4 a, in vec2 b);

void main() {
    mainImage(gl_FragColor, gl_FragCoord.xy);
}

#ifdef GL_ES
precision mediump float;
#endif


/* SHADERTOY FROM HERE */

// --------------------------------------------------------
// HG_SDF
// https://www.shadertoy.com/view/Xs3GRB
// --------------------------------------------------------

#define PI 3.14159265359

void pR(inout vec2 p, float a) {
    p = cos(a)*p + sin(a)*vec2(p.y, -p.x);
}

float smax(float a, float b, float r) {
    vec2 u = max(vec2(r + a,r + b), vec2(0));
    return min(-r, max (a, b)) + length(u);
}


// --------------------------------------------------------
// Spectrum colour palette
// IQ https://www.shadertoy.com/view/ll2GD3
// --------------------------------------------------------

vec3 pal( in float t, in vec3 a, in vec3 b, in vec3 c, in vec3 d ) {
    return a + b*cos( 6.28318*(c*t+d) );
}

vec3 spectrum(float n) {
    return pal( n, vec3(0.5,0.5,0.5),vec3(0.5,0.5,0.5),vec3(1.0,1.0,1.0),vec3(0.0,0.33,0.67) );
}


// --------------------------------------------------------
// Main SDF
// https://www.shadertoy.com/view/wsfGDS
// --------------------------------------------------------

vec4 inverseStereographic(vec3 p, out float k) {
    k = 2.0/(1.0+dot(p,p));
    return vec4(k*p,k-1.0);
}

float fTorus(vec4 p4) {
    float d1 = length(p4.xy) / length(p4.zw) - 1.;
    float d2 = length(p4.zw) / length(p4.xy) - 1.;
    float d = d1 < 0. ? -d1 : d2;
    d /= PI;
    return d;
}

float fixDistance(float d, float k) {
    float sn = sign(d);
    d = abs(d);
    d = d / k * 1.82;
    d += 1.;
    d = pow(d, .5);
    d -= 1.;
    d *= 5./3.;
    d *= sn;
    return d;
}

float time;

float map(vec3 p) {
    float k;
    vec4 p4 = inverseStereographic(p,k);

    pR(p4.zy, time * -PI / 2.);
    pR(p4.xw, time * -PI / 2.);

    // A thick walled clifford torus intersected with a sphere

    float d = fTorus(p4);
    d = abs(d);
    d -= .2;
    d = fixDistance(d, k);
    d = smax(d, length(p) - 1.85, .2);

    return d;
}


// --------------------------------------------------------
// Rendering
// --------------------------------------------------------

mat3 calcLookAtMatrix(vec3 ro, vec3 ta, vec3 up) {
    vec3 ww = normalize(ta - ro);
    vec3 uu = normalize(cross(ww,up));
    vec3 vv = normalize(cross(uu,ww));
    return mat3(uu, vv, ww);
}

void mainImage(out vec4 fragColor, in vec2 fragCoord) {

    time = mod(iTime / 2., 1.);

    vec3 camPos = vec3(1.8, 5.5, -5.5) * 1.75;
    vec3 camTar = vec3(.0,0,.0);
    vec3 camUp = vec3(-1,0,-1.5);
    mat3 camMat = calcLookAtMatrix(camPos, camTar, camUp);
    float focalLength = 5.;
    vec2 p = (-iResolution.xy + 2. * gl_FragCoord.xy) / iResolution.y;

    vec3 rayDirection = normalize(camMat * vec3(p, focalLength));
    vec3 rayPosition = camPos;
    float rayLength = 0.;

    float distance = 0.;
    vec3 color = vec3(0);

    vec3 c;

    // Keep iteration count too low to pass through entire model,
    // giving the effect of fogged glass
    const float ITER = 82.;
    const float FUDGE_FACTORR = .8;
    const float INTERSECTION_PRECISION = .001;
    const float MAX_DIST = 20.;

    for (float i = 0.; i < ITER; i++) {

        // Step a little slower so we can accumilate glow
        rayLength += max(INTERSECTION_PRECISION, abs(distance) * FUDGE_FACTORR);
        rayPosition = camPos + rayDirection * rayLength;
        distance = map(rayPosition);

        // Add a lot of light when we're really close to the surface
        c = vec3(max(0., .01 - abs(distance)) * .5);
        c *= vec3(1.4,2.1,1.7); // blue green tint

        // Accumilate some purple glow for every step
        c += vec3(.6,.25,.7) * FUDGE_FACTORR / 160.;
        c *= smoothstep(20., 7., length(rayPosition));

        // Fade out further away from the camera
        float rl = smoothstep(MAX_DIST, .1, rayLength);
        c *= rl;

        // Vary colour as we move through space
        c *= spectrum(rl * 6. - .6);

        color += c;

        if (rayLength > MAX_DIST) {
            break;
        }
    }

    // Tonemapping and gamma
    color = pow(color, vec3(1. / 1.8)) * 2.;
    color = pow(color, vec3(2.)) * 3.;
    color = pow(color, vec3(1. / 2.2));

    fragColor = vec4(color, 1);
}
