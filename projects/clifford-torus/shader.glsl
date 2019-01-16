#ifdef GL_ES
precision mediump float;
#endif

uniform float iTime;

uniform vec2 iResolution;

uniform mat4 projection;
varying vec3 eye;
varying vec3 dir;
varying vec3 cameraForward;

uniform float guiColorScale;
uniform float guiColorOffset;
uniform bool guiColorFlip;


/* SHADERTOY FROM HERE */

// #define DEBUG

#define PI 3.14159265359

void pR(inout vec2 p, float a) {
    p = cos(a)*p + sin(a)*vec2(p.y, -p.x);
}

// Shortcut for 45-degrees rotation
void pR45(inout vec2 p) {
    p = (p + vec2(p.y, -p.x))*sqrt(0.5);
}

// http://www.neilmendoza.com/glsl-rotation-about-an-arbitrary-axis/
mat3 rotationMatrix(vec3 axis, float angle)
{
    axis = normalize(axis);
    float s = sin(angle);
    float c = cos(angle);
    float oc = 1.0 - c;
    
    return mat3(
        oc * axis.x * axis.x + c,           oc * axis.x * axis.y - axis.z * s,  oc * axis.z * axis.x + axis.y * s,
        oc * axis.x * axis.y + axis.z * s,  oc * axis.y * axis.y + c,           oc * axis.y * axis.z - axis.x * s,
        oc * axis.z * axis.x - axis.y * s,  oc * axis.y * axis.z + axis.x * s,  oc * axis.z * axis.z + c
    );
}


// Repeat space along one axis. Use like this to repeat along the x axis:
// <float cell = pMod1(p.x,5);> - using the return value is optional.
float pMod1(inout float p, float size) {
    float halfsize = size*0.5;
    float c = floor((p + halfsize)/size);
    p = mod(p + halfsize, size) - halfsize;
    return c;
}

vec2 pMod2(inout vec2 p, vec2 size) {
    vec2 c = floor((p + size*0.5)/size);
    p = mod(p + size*0.5,size) - size*0.5;
    return c;
}

vec3 pMod3(inout vec3 p, vec3 size) {
    vec3 c = floor((p + size*0.5)/size);
    p = mod(p + size*0.5, size) - size*0.5;
    return c;
}

float vmax(vec2 v) {
    return max(v.x, v.y);
}

float vmax(vec3 v) {
    return max(max(v.x, v.y), v.z);
}

float fBox(vec2 p, vec2 b) {
    vec2 d = abs(p) - b;
    return length(max(d, vec2(0))) + vmax(min(d, vec2(0)));
}

float fBox(vec3 p, vec3 b) {
    vec3 d = abs(p) - b;
    return length(max(d, vec3(0))) + vmax(min(d, vec3(0)));
}

float smin(float a, float b, float r) {
    vec2 u = max(vec2(r - a,r - b), vec2(0));
    return max(r, min (a, b)) - length(u);
}

float smax(float a, float b, float r) {
    vec2 u = max(vec2(r + a,r + b), vec2(0));
    return min(-r, max (a, b)) + length(u);
}

vec3 pal( in float t, in vec3 a, in vec3 b, in vec3 c, in vec3 d ) {
    return a + b*cos( 6.28318*(c*t+d) );
}

vec3 spectrum(float n) {
    return pal( n, vec3(0.5,0.5,0.5),vec3(0.5,0.5,0.5),vec3(1.0,1.0,1.0),vec3(0.0,0.33,0.67) );
}


// Torus in the XZ-plane
float fTorus(vec3 p, float smallRadius, float largeRadius) {
    return length(vec2(length(p.xz) - largeRadius, p.y)) - smallRadius;
}

void pModTorus(inout vec3 p, float smallRadius, float largeRadius) {
    vec2 xy = vec2(length(p.xz), p.y) - vec2(largeRadius,0);
    p = vec3(
        (atan(p.x, p.z) / PI) * .5 + .5,
        (atan(xy.y, xy.x) / PI) * .5 + .5,
        fTorus(p, smallRadius, largeRadius)
    );
} 

float time;
vec3 mcolor;

// Inverse stereographic projection of p,
// p4 lies onto the unit 3-sphere centered at 0.
// - Syntopia https://www.shadertoy.com/view/XdfGW4
vec4 inverseStereographic(vec3 p) {
    float r = length(p);
    vec4 p4 = vec4(2.*p,1.-r*r)*1./(1.+r*r);
    return p4;
}

// Distances get warped by the stereographic projection, this applies
// some hacky adjustments which makes them lipschitz continuous.

// I don't really understand why, and the numbers have been hand
// picked by comparing our 4D torus SDF to a usual 3D torus of the
// same size, see DEBUG.

// vec3 p
//   Original 3D domain, centered with the stereographic projection;
//   basically it should not be scaled or translated.

// vec3 d
//   SDF to fix, this should be applied after the last step of
//   modelling on the torus.

// vec3 threshold
//   The fix causes a blob artefact at the origin, so return the
//   original SDF when below this distance to the surface. Smaller
//   values result in a faster ray hit, but can cause more artefacts.

float fixDistance(vec3 p, float d, float threshold) {
    d *= PI;

    float od = d;
    float sn = sign(d);

    d = abs(d);
    d = d * dot(p, p) / 4.2;
    if (d > 1.) {
        d = pow(d, .5);
        d = (d - 1.) * 1.8 + 1.;
    }
    d *= sn;

    if (abs(d) < threshold) {
        d = od / PI;
    }

    return d;
}

float fTorus(vec4 p4, out vec2 uv) {

    // Torus distance
    float d = length(p4.xy) / length(p4.zw) - 1.;

    if (d > 0.) {
        // The distance outside the torus gets exponentially large
        // because of the stereographic projection. So use the inside
        // of an inverted torus for the outside distance.
        d = 1. - length(p4.zw) / length(p4.xy);
    }

    // Because of the projection, distances aren't lipschitz continuous,
    // so scale down the distance at the most warped point - the inside
    // edge of the torus such that it is 1:1 with the domain.
    d /= PI;

    // UV coordinates over the surface, from 0 - 1
    uv = (vec2(
        atan(p4.y, p4.x),
        atan(p4.z, p4.w)
    ) / PI) * .5 + .5;

    return d;
}

vec2 modelUv;
bool hit3DTorus = false;

float map(vec3 p) {

    // return length(p) - 1.;

    #ifdef DEBUG
        if (p.x < 0.) {
            hit3DTorus = true;
            return abs(fTorus(p.xzy, 1.002, 1.4163));
        }
    #endif

    vec4 p4 = inverseStereographic(p);

    // The inside-out rotation puts the torus at a different
    // orientation, so rotate to point it at back in the same
    // direction
    pR(p4.zy, time * -PI / 2.);

    // Rotate in 4D, turning the torus inside-out
    pR(p4.xw, time * -PI / 2.);

    vec2 uv;
    float d = fTorus(p4, uv);
    modelUv = uv;

    #ifdef DEBUG
        // d = abs(d);
        // d = fixDistance(p, d, 1.);
        // return d;
    #endif


    // Recreate domain to be wrapped around the torus surface
    // xy = surface / face, z = depth / distance
    vec3 pp = p;
    float uvScale = 2.25; // Magic number that makes xy distances the same scale as z distances
    p = vec3(uv * uvScale, d);


    d = abs(d);
    // return d;

    float d3 = fixDistance(pp, d-.3, .00001);

    d = fixDistance(pp, d-.2, .00001);

    d = smax(d, length(pp) - 1.85, .2);
    // d = max(d, length(pp) - 2.);
    // d = smin(-d, d3, .2);
    float d2= -d;

    // d = min(d, -d3);


    return d;

    float n = sqrt(2.) * 20.;
    float repeat = uvScale / n;

    pR45(p.xy);

    p.xy += repeat / 2.;

    // p.z = abs(p.z) - .15;
    // pMod1(p.z, .2);
    pMod2(p.xy, vec2(repeat));

    float ww = repeat * mix(.1, .6, smoothstep(3., 1., length(pp)));
    // d = length(p) - ww;
    d = fBox(p, vec3(ww,ww,ww*.5));

    // ww = repeat * .01;
    // d = fBox(p.xz, vec2(ww));
    // d = min(d, fBox(p.yz, vec2(ww)));
    // d = max(d, abs(p.z) - .1);
    d = fixDistance(pp, d, .01);

    // d = max(d, -d2);
    return d;
}

bool hitDebugPlane = false;

float mapDebug(vec3 p) {
    float d = map(p);
    #ifndef DEBUG
        return d;
    #endif
    float plane = min(abs(p.z), abs(p.y));
    hitDebugPlane = plane < abs(d);
    return hitDebugPlane ? plane : d;
}


// --------------------------------------------------------
// Rendering
// --------------------------------------------------------

vec3 calcNormal(vec3 p) {
  vec3 eps = vec3(.0001,0,0);
  vec3 n = vec3(
    map(p + eps.xyy) - map(p - eps.xyy),
    map(p + eps.yxy) - map(p - eps.yxy),
    map(p + eps.yyx) - map(p - eps.yyx)
  );
  return normalize(n);
}

const float ITER = 150.;
const float INTERSECTION_PRECISION = .001;
const float MAX_DIST = 20.;
const float FUDGE_FACTORR = .2;

mat3 calcLookAtMatrix(vec3 ro, vec3 ta, vec3 up) {
    vec3 ww = normalize(ta - ro);
    vec3 uu = normalize(cross(ww,up));
    vec3 vv = normalize(cross(uu,ww));
    return mat3(uu, vv, ww);
}

void main() {

    time = mod(iTime / 2., 100.);
    // time = .5;

    vec3 rayOrigin = eye;
    vec3 rayDirection = normalize(dir);
    vec3 rayPosition = rayOrigin;
    float rayLength = 0.;

    float distance = 0.;
    vec3 color = vec3(0);

    vec3 camPos = vec3(1.8, 5.5, -5.5) * 1.75;
    vec3 camTar = vec3(.0,0,.0);
    vec3 camUp = vec3(-1,0,-1.5);
    mat3 camMat = calcLookAtMatrix(camPos, camTar, camUp);
    float focalLength = 5.;
    vec2 p = (-iResolution.xy + 2. * gl_FragCoord.xy) / iResolution.y;
    rayDirection = normalize(camMat * vec3(p, focalLength));
    rayOrigin = camPos;
    rayPosition = rayOrigin;


    float h, t;
    vec3 c;

    for (float i = 0.; i < ITER; i++) {
        rayLength += max(INTERSECTION_PRECISION, h * FUDGE_FACTORR);
        rayPosition = rayOrigin + rayDirection * rayLength;
        distance = mapDebug(rayPosition);

        h = abs(distance * 4.);
        
        c = vec3(1.4,2.1,1.7) * pow(max(0., (.02 - h)) * 19.5, 10.) * 150.;
        c += vec3(.6,.25,.7) * .0125 * FUDGE_FACTORR;
        c *= smoothstep(10., 5., length(rayPosition));
        
        float ee = smoothstep(MAX_DIST, .1, rayLength);
        c *= spectrum(ee * 6. - .6);
        color += c * ee;


        // if (distance < .001) {
        //     vec3 normal = calcNormal(rayPosition);
        //     color = normal * .5 + .5;
        //     color = vec3(dot(normalize(vec3(1,.5,0)), normal) * .5 + .5);
        //     #ifdef DEBUG
        //         if (hitDebugPlane) {
        //             // Display distance
        //             float d = map(rayPosition);
        //             color = vec3(mod(abs(d) * 10., 1.));
        //             color *= spectrum(abs(d));
        //             color = mix(color, vec3(1), step(0., -d) * .25);
        //         } else if ( ! hit3DTorus) {
        //             // Color UVs
        //             float repeat = 1. / 20.;
        //             pMod2(modelUv, vec2(repeat));
        //             color -= color * vec3(0,1,0) * smoothstep(0., .001, abs(modelUv.x) - repeat * .4);
        //             color -= color * vec3(1,0,0) * smoothstep(0., .001, abs(modelUv.y) - repeat * .4);
        //         }
        //     #endif
        //     break;
        // }
        if (rayLength > MAX_DIST) {
            break;
        }
    }


    color = pow(color, vec3(1./1.8)) * 2.;
    color = pow(color, vec3(2.));
    
    color *= 3.;

    // #ifndef DEBUG
    //     float fog = pow(smoothstep(7.25, MAX_DIST, rayLength), .25);
    //     color = mix(color, vec3(0), fog);
    //     float f = guiColorFlip ? 1. : -1.;
    //     color = spectrum(f * (color.r * 2. - 1.) * guiColorScale + guiColorOffset);
    //     color *= mix(1., .025, fog);
    // #endif
    // color = 1. -color;
    color = pow(color, vec3(1. / 2.2)); // Gamma
    gl_FragColor = vec4(color, 1);
}
