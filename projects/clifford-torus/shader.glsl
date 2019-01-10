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

float map(vec3 p) {

    float s = 5.;

    // pR(p.xy, PI / -2.);
    pR45(p.xz);

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
    // d = abs(d) - .0001;

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


float map_(vec3 p) {
    float d;

    p = -p.yxz;


    pR(p.xy, PI/-2.);
    pR(p.yz, PI / -4.);

    // pR(p.yz, time * PI / 2.);
    p.y -= .25;

    vec3 ppp = p;
    pMod3(p, vec3(.05));
    float mask = length(p) - .025;
    p = ppp;

    
    float e = 2.;

    float s = dot(p,p);
    // s = .1;
    p /= s;

    p.y += e;

    pR(p.xy, time * PI / 1.);

    pModTorus(p, e, e * sqrt(2.));
    d = p.z;
    mcolor = p;

    // d = abs(d) - .0001;

    pMod2(p.xy, vec2(.1));
    d = fBox(p, vec3(.03,.03,.1));

    d *= s;

    // d = max(d, -mask);

    return d;

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
    rayLength += distance * .5;
    rayPosition = rayOrigin + rayDirection * rayLength;
    distance = map(rayPosition);
    if (distance < .001) {
      color = calcNormal(rayPosition) * .5 + .5;
      // color = mcolor;
      break;
    }
  }

  gl_FragColor = vec4(color, 1);

}
