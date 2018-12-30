precision mediump float;
uniform mat4 projection;
uniform vec2 iResolution;
uniform mat4 view;
uniform float iTime;

float time;

#define PI 3.14159265

void pR(inout vec2 p, float a) {
    p = cos(a)*p + sin(a)*vec2(p.y, -p.x);
}

// Shortcut for 45-degrees rotation
void pR45(inout vec2 p) {
    p = (p + vec2(p.y, -p.x))*sqrt(0.5);
}

// Repeat space along one axis. Use like this to repeat along the x axis:
// <float cell = pMod1(p.x,5);> - using the return value is optional.
float pMod1(inout float p, float size) {
    float halfsize = size*0.5;
    float c = floor((p + halfsize)/size);
    p = mod(p + halfsize, size) - halfsize;
    return c;
}

// Repeat in two dimensions
vec2 pMod2(inout vec2 p, vec2 size) {
    vec2 c = floor((p + size*0.5)/size);
    // c.x = clamp(c.x, -1., 0.);
    // c.y = clamp(c.y, -1., 0.);
    // p = mod(p + size*0.5,size) - size*0.5;
    p -= c * size;
    return c;
}

// Same, but mirror every second cell so all boundaries match
vec2 pModMirror2(inout vec2 p, vec2 size) {
    vec2 halfsize = size*0.5;
    vec2 c = floor((p + halfsize)/size);
    p = mod(p + halfsize, size) - halfsize;
    p *= mod(c,vec2(2))*2. - vec2(1);
    return c;
}


// Repeat around the origin by a fixed angle.
// For easier use, num of repetitions is use to specify the angle.
float pModPolar(inout vec2 p, float repetitions) {
    float angle = 2.*PI/repetitions;
    float a = atan(p.y, p.x) + angle/2.;
    float r = length(p);
    float c = floor(a/angle);
    a = mod(a,angle) - angle/2.;
    p = vec2(cos(a), sin(a))*r;
    // For an odd number of repetitions, fix cell index of the cell in -x direction
    // (cell index would be e.g. -5 and 5 in the two halves of the cell):
    if (abs(c) >= (repetitions/2.)) c = abs(c);
    return c;
}

float smin(float a, float b, float r) {
    vec2 u = max(vec2(r - a,r - b), vec2(0));
    return max(r, min (a, b)) - length(u);
}

float smax(float a, float b, float r) {
    vec2 u = max(vec2(r + a,r + b), vec2(0));
    return min(-r, max (a, b)) + length(u);
}

float smin(float a, float b) {
    return smin(a, b, .0);
}

float smax(float a, float b) {
    return smax(a, b, 0.);
}

float fOpDifferenceColumns(float a, float b, float r, float n) {
    a = -a;
    float m = min(a, b);
    //avoid the expensive computation where not needed (produces discontinuity though)
    if ((a < r) && (b < r)) {
        vec2 p = vec2(a, b);
        float columnradius = r*sqrt(2.)/n/2.0;
        columnradius = r*sqrt(2.)/((n-1.)*2.+sqrt(2.));

        pR45(p);
        p.y += columnradius;
        p.x -= sqrt(2.)/2.*r;
        p.x += -columnradius*sqrt(2.)/2.;

        if (mod(n,2.) == 1.) {
            p.y += columnradius;
        }
        pMod1(p.y,columnradius*2.);

        float result = -length(p) + columnradius;
        result = max(result, p.x);
        result = min(result, a);
        return -min(result, b);
    } else {
        return -m;
    }
}

float fBox(vec3 p, vec3 s) {
  p = abs(p) - s;
  return max(p.x, max(p.y, p.z));
}

float fBox(vec2 p, vec2 s) {
  p = abs(p) - s;
  return max(p.x, p.y);
}

void moveCam(inout vec3 p) {
  // p.z -= .2;
  // p.y += .2;
  p.x += 1.;
  pR(p.xz, time * PI / 2.);
  // p.y += time * 2. + .5;
}

float _map(vec3 p) {
  // 1 x 1 x 1 Box
  // p /= 3.;
  // p.y /= 2.;

  // moveCam(p);

  float floor = dot(abs(p), vec3(0,-1,0)) + .5;
  float midpoint = length(p) - .2;
  float d = 1e12;

  p += .5;
  p = mod(p, 1.) - .5;

  float th = .49;
  d = min(d, fBox(p, vec3(.55,th,th)));
  d = min(d, fBox(p, vec3(th,.55,th)));
  d = min(d, fBox(p, vec3(th,th,.55)));

  d = min(d, fBox(p, vec3(.49)));
  
  d = -d;
  // d = min(d, floor);
  d = min(d, midpoint);

  // d = 1e12;

  // d = min(d, length(p) - .04);
  return d;
}

float stair2d(vec2 p, vec2 size) {
  float g = dot(p + size.yx / 2., normalize(-size.yx));
  if (g > .001) {
    return g;
  }
  p.y -= floor((-p.x / size.x)) * size.y;
  p.x = mod(p.x, size.x) - size.x;
  return min(
    -p.y,
    max(-p.x, -p.y - size.y)
  );
}

// xyz: length, height, width
float stairPart(vec3 p, vec3 size, float steps) {
  vec2 stepSize = size.xy / vec2(steps - 1., steps);
  float d = stair2d(p.xy, stepSize);
  // st = max(st, dot(p.xy - stepSize.yx / 1.5, normalize(stepSize.yx)));
  float b = fBox(p.xz - vec2(size.z, 0), size.xz + vec2(size.z, 0));
  d = max(d, b);

  d = max(d, -p.y - size.y); // top step

  p.y -= size.y * 2.;
  float a = length(p.xy) - size.x;
  a = min(a, fBox(p.xy - vec2(0, size.x * 2.), vec2(size.x, size.x * 2.)));
  d = max(d, -a);

  d = fOpDifferenceColumns(d, -min(p.y * .5, p.x), .125, 3.);
  d = max(d, p.y);

  d = min(d, fBox(p + vec3(1.,size.y+stepSize.y/2.,-.5), vec3(.5 + size.z, stepSize.y / 2., .5 + size.z)));

  p.x -= size.x + size.z;
  p.y -= .01;
  d = min(d, length(p) - .05);
  // d = min(d, length(p.xz) - .02);

  return d;
}

float map(vec3 p) {
  // p = mod(p + .5, 1.) - .5;
  // moveCam(p);
  // return dot(p, vec3(0,-1,0));
  p.y -= .9;

  moveCam(p);

  float grid = _map(p);

  pModMirror2(p.xz, vec2(2));

  // pModMirror2(p.xz, vec2(1));

  p.y -= time * .5;
  // pR(p.xz, min(c.x, c.y) * PI / 2.);
  // pR(p.xz, c.y * PI / 2.);

  float steps = 5.;
  vec3 size = vec3(.5,.25,.15);
  size.x -= size.z;
  size.y += (size.y * 2.) / (steps * 2. - 1.) / 2.;
  // float d = fBox(p.xz, vec2(.4));
  float d = 1e12;
  vec3 ppp = p;

  p.y = mod(p.y, 2.);
  vec3 pp = p;

  // return stairPart(p + vec3(0, .5, -.5), size, steps);
  float s = 1e12;

  p.y += .5;
  s = min(s, stairPart(p + vec3(0, 0, -.5), size, steps));

  pR(p.xz, PI / 2.);
  p.y -= .5;
  s = min(s, stairPart(p + vec3(0, 0, -.5), size, steps));

  pR(p.xz, PI / 2.);
  p.y -= .5;
  s = min(s, stairPart(p + vec3(0, 0, -.5), size, steps));

  pR(p.xz, PI / 2.);
  p.y -= .5;
  s = min(s, stairPart(p + vec3(0, 0, -.5), size, steps));

  pR(p.xz, PI / 2.);
  p.y -= .5;
  s = min(s, stairPart(p + vec3(0, 0, -.5), size, steps));

  pR(p.xz, PI / 2.);
  p.y -= .5;
  s = min(s, stairPart(p + vec3(0, 0, -.5), size, steps));

  d = min(d, s);

  p = ppp;
  // d = max(d, -p.y);

  // d = min(d, grid);

  return d;
}


const int NORMAL_STEPS = 6;
vec3 calcNormal(vec3 pos){
  vec3 eps = vec3(.0001,0,0);
  vec3 nor = vec3(0);
  float invert = 1.;
  for (int i = 0; i < NORMAL_STEPS; i++){
    nor += map(pos + eps * invert) * eps * invert;
    eps = eps.zxy;
    invert *= -1.;
  }
  return normalize(nor);
}

const float ITER = 200.;
const float MAX_DIST = 5.;

vec3 getStereoDir() {
  vec2 p = gl_FragCoord.xy / iResolution.xy;
  float m = .5;
  p = (p * 2. * m - m) * 3.142;
  p.x *= iResolution.x / iResolution.y;
  vec3 dir = vec3(
    p.x * 2.,
    dot(p, p) - 1.,
    p.y * 2.
  );
  dir = dir.xzy;
  // pR(dir.xz, time * PI * 2.);
  pR(dir.xy, .5);
  return normalize(dir);
}



void main() {

  time = mod(iTime * .5, 1.);

  vec2 vertex = 2.0 * (gl_FragCoord.xy / iResolution.xy) - 1.0;

  float fov = 1. / projection[1].y;
  float aspect = projection[1].y / projection[0].x;
  vec3 eye = -(view[3].xyz) * mat3(view);
  vec3 dir = vec3(vertex.x * fov * aspect, vertex.y * fov,-1.0) * mat3(view);

  dir = getStereoDir();
  dir *= mat3(view);

  vec3 rayOrigin = vec3(0);
  // rayOrigin = eye;
  vec3 rayDirection = normalize(dir);
  vec3 rayPosition = rayOrigin;
  float rayLength = 0.;

  float distance = 0.;
  vec3 color = vec3(1);
  for (float i = 0.; i < ITER; i++) {
    rayLength += distance;
    rayPosition = rayOrigin + rayDirection * rayLength;
    // moveCam(rayPosition);
    distance = map(rayPosition);
    if (distance < .001) {
      vec3 normal = calcNormal(rayPosition);
      color = normal * .5 + .5;
      // color *= mix(1., dot(vec3(1,1,1), normal) * .5 + .5, .5);
      break;
    }
    if (distance >= MAX_DIST) {
      break;
    }
  }
  color = mix(color, vec3(1), smoothstep(0., MAX_DIST, rayLength));

  gl_FragColor = vec4(color, 1);
}
