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

// Repeat in three dimensions
vec3 pMod3(inout vec3 p, vec3 size) {
    vec3 c = floor((p + size*0.5)/size);
    p = mod(p + size*0.5, size) - size*0.5;
    return c;
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

float map(vec3 p) {

    p = -p.yxz;


    pR(p.xy, PI/-2.);
    pR(p.yz, PI / -4.);

    pR(p.yz, time * PI / 2.);
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

    pR(p.xy, time * PI / 2.);

    pModTorus(p, e, e * sqrt(2.));
    float d = p.z;
    mcolor = p;

    d = abs(d) - .0001;

    d *= s;

    d = max(d, -mask);

    return d;

}


// --------------------------------------------------------
// Rendering
// --------------------------------------------------------

vec3 calcNormal(vec3 p) {
  vec3 eps = vec3(.001,0,0);
  vec3 n = vec3(
    map(p + eps.xyy) - map(p - eps.xyy),
    map(p + eps.yxy) - map(p - eps.yxy),
    map(p + eps.yyx) - map(p - eps.yyx)
  );
  return normalize(n);
}

const float ITER = 100.;

void main() {

  time = mod(iTime / 2., 1.);

  vec3 rayOrigin = eye;
  vec3 rayDirection = normalize(dir);
  vec3 rayPosition = rayOrigin;
  float rayLength = 0.;

  float distance = 0.;
  vec3 color = vec3(0);
  for (float i = 0.; i < ITER; i++) {
    rayLength += distance * .25;
    rayPosition = rayOrigin + rayDirection * rayLength;
    distance = map(rayPosition);
    // color += .003;
    if (distance < .001) {
      color = calcNormal(rayPosition);
      color = mcolor;
      break;
    }
  }

  gl_FragColor = vec4(color, 1);

}
