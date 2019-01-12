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

vec4 istereographic(vec3 p, out float k) {
  k = 2.0/(1.0+dot(p,p));
  return vec4(k*p,k-1.0);
}

// TODO: To fix glitch, when we're on the inside,
// use the flipped/rotated distance

float fTorus(vec3 p, vec4 p4, out vec2 uv) {

    float di = 1. - (length(p4.zw) / length(p4.xy));
    float d = (length(p4.xy) / length(p4.zw)) - 1.;

    if (d > 0.) {
        d = di;
    }

    float dj = d;

    float sn = sign(d);
    d = abs(d);
    d = (d * dot(p, p)) / 4.2;
    if (d > 1.) {
        d = pow(d, .5);
        d = (d - 1.) * 1.8 + 1.;
    }
    d *= sn;

    if (abs(d) < 1.) {
        d = dj / 3.;
    }

    uv = (vec2(
        atan(p4.y, p4.x),
        atan(p4.z, p4.w)
    ) / PI) * .5 + .5;

    return d;
}

float map(vec3 p) {

    float d;

    if (p.x < 0.) {
        // return length(p) - 1.;
    }

    float s = dot(p,p);

    // pR(p.xy, PI / -2.);
    // pR45(p.xz);

    float k;
    vec4 p4 = istereographic(p, k);

    pR(p4.zy, time * -PI / 2.);
    pR(p4.xw, time * -PI / 2.);

    vec2 uv;
    d = fTorus(p, p4, uv);
    // d = abs(d);
    // d -= .01;

    // return d;

    p = vec3(uv, d);

    float n = 10.;

    p.xy += .5/n;
    // p.x += .5/n;

    pMod2(p.xy, vec2(1./n));
    d = length(p.xy) - (1./n) * .4;
    // d *= s;
    d = smax(d, abs(p.z) - .02, .01);

    // d = fBox(p, vec3(vec2((1./n) * .4), .01));

    pMod2(p.xy, vec2(1./n));
    mcolor = vec3(1.);
    mcolor -= vec3(0,1,0) * smoothstep(0., .005, abs(p.x) - (1./n) * .4);
    mcolor -= vec3(1,0,0) * smoothstep(0., .005, abs(p.y) - (1./n) * .4);

    return d;
}

bool debug = false;

float mapDebug(vec3 p) {
    float d = map(p);
    return d;
    float plane = min(abs(p.z), abs(p.y));
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

const float ITER = 5000.;

void main() {

  time = mod(iTime / 2., 1.);

  vec3 rayOrigin = eye;
  vec3 rayDirection = normalize(dir);
  vec3 rayPosition = rayOrigin;
  float rayLength = 0.;

  float distance = 0.;
  vec3 color = vec3(0);
  for (float i = 0.; i < ITER; i++) {
    rayLength += distance * .5;
    rayPosition = rayOrigin + rayDirection * rayLength;
    distance = mapDebug(rayPosition);
    // distance = abs(distance);
    if (distance < .001) {
      vec3 normal = calcNormal(rayPosition);
      color = normal * .5 + .5;
      color = vec3(dot(vec3(1,0,0), normal) * .5 + .5);
      // color = mcolor;
      if (debug) {
        float d = map(rayPosition);
        color = vec3(mod(abs(d)*100., 1.));
        // color = mix(color, vec3(1,1,0), 1.-step(0., d - .04));
        color *= spectrum(abs(d*100.) / 10.);
        color = mix(color, vec3(1), step(0., -d) * .25);
      }
      break;
    }
    if (rayLength > 50.) {
      break;
    }
  }

  color = mix(color, vec3(0), pow(smoothstep(7., 12., rayLength), .25));
  color = pow(color, vec3(1. / 2.2));

  gl_FragColor = vec4(color, 1);

}
