#ifdef GL_ES
precision mediump float;
#endif

uniform float iTime;

uniform mat4 projection;
varying vec3 eye;
varying vec3 dir;
varying vec3 cameraForward;


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

vec4 istereographic(vec3 p, out float k) {
  k = 2.0/(1.0+dot(p,p));
  return vec4(k*p,k-1.0);
}

// TODO: To fix glitch, when we're on the inside,
// use the flipped/rotated distance

float map(vec3 p) {

    if (p.z > 0.) {
        // return length(p) - 1.215;
    }

    p.y -= 1.2;
    float s = 1.;

    // pR(p.xy, PI / -2.);
    // pR45(p.xz);

    if (p.y > 0.) {
        // pR(p.xz, PI / -2.);
    }

    p *= s;

    float d;
    vec3 pp = p;

    // float r = length(p);
    // vec4 p4 = vec4(2. * p, 1. - r * r) * 1. / (1. + r * r);
    float k;
    vec4 p4 = istereographic(p, k);
    pR(p4.zy, time * -PI / 2.);
    pR(p4.xw, time * -PI / 2.);

    // p4.xyw *= rotationMatrix(normalize(vec3(0,-1,1)), iTime);

    d = (length(p4.xy) / length(p4.zw)) - 1.;

    // d *= 5.;
    d *= -1.;
    float sg = sign(d);
    d = (d * dot(p, p)) / 4.2;
    if (d > 1.) {
        d = pow(d, .5);
        d = (d - 1.) * 1.8 + 1.;
    }
    // d *= sg;
    // d = pow(d * dot(p, p), .5) / PI * 3.;

    // if (d < 0.) {
    //     d *= -PI;
    //     d = mix(d, pow(d, 4.) / 6.4, step(1., d));
    //     d = -d;
    // }
    // d *= 1.5;
    // d = abs(d) - .0001;
    // return d / s;
    // d *= PI;
    // return mix(d, pow(d, 4.) / 6.4, step(1., d));
    // // return (d-.2) * 10.;
    // return (pow(d + .5, 10.)) * .5 - .5;
    // d = pow(d * dot(p, p), .5) / PI * 2.;
    // d *= d < 2. ? .5 : 1.;
    return d;

    vec2 uv = vec2(
        atan(p4.y, p4.x),
        atan(p4.z, p4.w)
    );
    uv += PI;
    uv /= PI * 2.;

    p = vec3(uv, d);

    float n = 10.;

    // p.xy += .5/n;

    pMod2(p.xy, vec2(1./n));
    d = length(p.xy) - (1./n) * .4;
    d = max(d, abs(p.z) - .05);

    // d = fBox(p, vec3(vec2((1./n) * .4), .05));

    pMod2(p.xy, vec2(1./n));
    mcolor = vec3(1.);
    mcolor -= vec3(0,1,0) * smoothstep(0., .005, abs(p.x) - (1./n) * .4);
    mcolor -= vec3(1,0,0) * smoothstep(0., .005, abs(p.y) - (1./n) * .4);

    if (p.z > .5) {
        d /= 5.;
    }

    return d / s;
}

bool debug = false;

float mapDebug(vec3 p) {
    float d = map(p);
    return d;
    float plane = abs(p.x);
    // debug = true;
    // return plane;
    debug = plane < abs(d);
    return debug ? plane : d;
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

const float ITER = 500.;

void main() {

  time = mod(iTime / 2., 2.);

  vec3 rayOrigin = eye;
  vec3 rayDirection = normalize(dir);
  vec3 rayPosition = rayOrigin;
  float rayLength = 0.;

  float distance = 0.;
  vec3 color = vec3(0);
  for (float i = 0.; i < ITER; i++) {
    rayLength += distance * 1.;
    rayPosition = rayOrigin + rayDirection * rayLength;
    distance = mapDebug(rayPosition);
    distance = abs(distance);
    if (distance < .001) {
      color = calcNormal(rayPosition) * .5 + .5;
      // color = mcolor;
      if (debug) {
        float d = map(rayPosition);
        color = vec3(mod(d, 1.));
        // color = mix(color, vec3(1,1,0), 1.-step(0., d - .04));
        color *= spectrum(abs(d) / 10.);
        // color = mix(color, vec3(1), step(0., -d));
      }
      break;
    }
  }

  gl_FragColor = vec4(color, 1);

}
