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
uniform int guiMethod;
uniform float guiS;
uniform float guiE;
uniform float guiF;
uniform bool guiDebug;

#pragma glslify: distanceMeter = require(./distance-meter.glsl)

/* SHADERTOY FROM HERE */

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
vec4 inverseStereographic(vec3 p, out float k) {
  k = 2.0/(1.0+dot(p,p));
  return vec4(k*p,k-1.0);
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

// vec3 k
//   scale factor

// 0 - original
// 1 - MLA 1
// 2 - MLA 2

float fixDistance(vec3 p, float d, float k) {

    // return d;

    // return d / k;

    // #if DIST_FN == 0
    //     d *= PI;
    //     float od = d;
    // #endif

    
    float sn = sign(d);
    d = abs(d);

    if (guiMethod == 0) {
        d = d / k * 2.;
        d += 1.;
        d = pow(d, .5);
        d -= 1.;
        d *= 1.73;
    } else if (guiMethod == 1) {
        d = d / k * 3.607;
        d += 1.;
        d = pow(d, .5);
        d -= 1.;
        // d *= .85;
    } else {
        d = d / k * 4.468;
        d += 1.;
        d = pow(d, .5);
        d -= 1.;
        d *= .831;
    }

    d *= sn;

    // #if DIST_FN == 0
    //     if (abs(d) < .01) {
    //         d = od / PI;
    //     }
    // #endif

    return d;
}

float fTorus(vec4 p4, out vec2 uv) {

    float d, d1, d2;

    // Torus distance
    if (guiMethod == 0) {
        d1 = length(p4.xy) / length(p4.zw) - 1.;
        d2 = length(p4.zw) / length(p4.xy) - 1.;
        d = d1 < 0. ? d1 : -d2;
        d /= PI;
    } else if (guiMethod == 1) {
        // Distance from surface x^2 + y^2 = 0.5
        d1 = length(p4.xy)-.707;
        d2 = length(p4.zw)-.707;
        d = d1 < 0. ? d1 : -d2;
        d /= 1.275;
    } else {
        //vec4 q4 = vec4(0.707*p4.xy/length(p4.xy), p4.zw);
        vec4 q4 = vec4(p4.xy,sqrt(.5)*p4.zw/length(p4.zw));
        q4 = normalize(q4);
        d = distance(p4, q4);
        if (length(p4.xy) - .707 < 0.) {
            q4 = vec4(p4.zw,sqrt(.5)*p4.xy/length(p4.xy));
            q4 = normalize(q4);
            d = -distance(p4, q4.zwxy);
        }
        d /= .94;
    }
    
    // Because of the projection, distances aren't lipschitz continuous,
    // so scale down the distance at the most warped point - the inside
    // edge of the torus such that it is 1:1 with the domain.
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

    if (guiDebug) {
        // if (p.x < 0.) {
        //     hit3DTorus = true;
        //     return fTorus(p.xzy, 1.000, 1.4145);
        // }
    }

    float k;
    vec4 p4 = inverseStereographic(p,k);

    // The inside-out rotation puts the torus at a different
    // orientation, so rotate to point it at back in the same
    // direction
    // pR(p4.zy, time * -PI / 2. + PI/2.);

    // Rotate in 4D, turning the torus inside-out
    pR(p4.xw, time * -PI / 2. + PI/2.);

    vec2 uv;
    float d = fTorus(p4, uv);
    modelUv = uv;

    if (guiDebug) {
        // d = abs(d);
        d = fixDistance(p, d, k);
        return d;
    }

    // Recreate domain to be wrapped around the torus surface
    // xy = surface / face, z = depth / distance
    vec3 pp = p;
    float uvScale = 2.75; // Magic number that makes xy distances the same scale as z distances
    p = vec3(uv * uvScale, d);

    float n = 10.;
    float repeat = uvScale / n;

    p.xy += repeat / 2.;
    pMod2(p.xy, vec2(repeat));
    d = abs(p.z);
    d = length(p.xy) - repeat * .4;
    d = smax(d, abs(p.z) - .013, .01);

    // float sz = repeat * .2;
    // p.z = -abs(p.z);
    // d = fBox(p, vec3(sz));
    // p.z += sz * 2.;
    // d = min(d, fBox(p, vec3(sz)));

    // p.z += sz * 2.;
    // d = min(d, fBox(p, vec3(sz)));
    // p.z += .1;
    // d = min(d, fBox(p, vec3(repeat * .2)));
    // d = fBox(p, vec3(repeat * .2));

    // d = abs(p.z);

    d = fixDistance(pp, d, k);
    return d;
}

bool hitDebugPlane = false;

float mapDebug(vec3 p) {
    float d = map(p);
    // return d;
    if ( ! guiDebug) {
        return d;
    }
    float plane = min(abs(p.z), abs(p.y));
    // plane= abs(p.y);
    hitDebugPlane = plane < abs(d);
    hitDebugPlane = true;
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

const float ITER = 400.;
const float MAX_DIST = 12.;


float plot(float height, vec2 p, float y){
    float thick = .005;
    y *= height;
    return (
        smoothstep( y - thick, y, p.y) - 
        smoothstep( y, y + thick, p.y)
    );
}

void main() {

    time = mod(iTime / 2., 1.);
    // time = .45;
    // time = 0.;

    vec3 rayOrigin = eye;
    vec3 rayDirection = normalize(dir);
    vec3 rayPosition = rayOrigin;
    float rayLength = 0.;

    float distance = 0.;
    vec3 color = vec3(0);

    for (float i = 0.; i < ITER; i++) {
        rayLength += distance;
        rayPosition = rayOrigin + rayDirection * rayLength;
        distance = mapDebug(rayPosition);

        if (distance < .001) {
            vec3 normal = calcNormal(rayPosition);
            color = normal * .5 + .5;
            color = vec3(dot(normalize(vec3(1,.5,0)), normal) * .5 + .5);
            if (guiDebug) {
                if (hitDebugPlane) {
                    // Display distance
                    float d = map(rayPosition);
                    color = vec3(mod(abs(d) * 100., 1.));
                    color *= spectrum(abs(d)*10.);
                    color = mix(color, vec3(1), step(0., -d) * .25);
                    color = distanceMeter(d * 2., rayLength, rayDirection, rayOrigin);
                } else if ( ! hit3DTorus) {
                    // Color UVs
                    // float repeat = 1. / 20.;
                    // pMod2(modelUv, vec2(repeat));
                    // color -= color * vec3(0,1,0) * smoothstep(0., .001, abs(modelUv.x) - repeat * .4);
                    // color -= color * vec3(1,0,0) * smoothstep(0., .001, abs(modelUv.y) - repeat * .4);
                }
            }
            break;
        }
        if (rayLength > MAX_DIST) {
            break;
        }
    }

    if ( ! guiDebug) {
        float fog = pow(smoothstep(7.25, MAX_DIST, rayLength), .25);
        color = mix(color, vec3(0), fog);
        float f = guiColorFlip ? 1. : -1.;
        color = spectrum(f * (color.r * 2. - 1.) * guiColorScale + guiColorOffset);
        color *= mix(1., .025, fog);
    }

    color = pow(color, vec3(1. / 2.2)); // Gamma

    gl_FragColor = vec4(color, 1);
    return;

    color = mix(color, vec3(1,0,1), 1.);

    vec2 p = gl_FragCoord.xy / iResolution.xy;
    vec3 p3;
    float start = 0.;
    float end = 2000.;

    p3 = vec3(-.01, mix(start, end, 1.-p.x), 0);
    float d = map(p3) / end*1.5;

    p3 = vec3(.01, mix(start, end, 1.-p.x), 0);
    float d2 = map(p3) / end*1.5;

    end = 10.;

    p3 = vec3(-.01, mix(start, end, 1.-p.x), 0);
    float db = map(p3) / end*1.5;

    p3 = vec3(.01, mix(start, end, 1.-p.x), 0);
    float db2 = map(p3) / end*1.5;

    end = 3.;

    p3 = vec3(-.01, mix(start, end, 1.-p.x), 0);
    float dc = map(p3) / end*1.5;

    p3 = vec3(.01, mix(start, end, 1.-p.x), 0);
    float dc2 = map(p3) / end*1.5;


    p.y -= .5;
    color = mix(color, vec3(1), plot(1., p, d));
    color = mix(color, vec3(0), plot(1., p, d2));

    color = mix(color, vec3(1), plot(1., p, db));
    color = mix(color, vec3(0), plot(1., p, db2));

    color = mix(color, vec3(1), plot(1., p, dc));
    color = mix(color, vec3(0), plot(1., p, dc2));

    color = mix(color, vec3(1,0,0), plot(.5, p, 0.));
    color = pow(color, vec3(1. / 2.2)); // Gamma

    gl_FragColor = vec4(color, 1);
}
