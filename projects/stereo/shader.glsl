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

float fBox(vec3 p, vec3 s) {
  p = abs(p) - s;
  return max(p.x, max(p.y, p.z));
}

float map(vec3 p) {
  // 1 x 1 x 1 Box

  pR(p.xy, time * PI/ 2.);
  // p.x += 1.25;
  // pR(p.xz, time * PI * 2.);
  // p.x += iTime;

  float floor = dot(abs(p), vec3(0,-1,0)) + .5;
  float d = 1e12;

  p += .5;
  p = mod(p, 1.) - .5;

  float th = .3;
  d = min(d, fBox(p, vec3(.52,th,th)));
  d = min(d, fBox(p, vec3(th,.52,th)));
  d = min(d, fBox(p, vec3(th,th,.52)));

  d = min(d, fBox(p, vec3(.48)));
  
  d = -d;
  // d = min(d, floor);

  // d = 1e12;

  // d = min(d, length(p) - .04);
  return d;
}

vec3 calcNormal(vec3 p) {
  vec3 eps = vec3(.001,0,0);
  vec3 n = vec3(
    map(p + eps.xyy) - map(p - eps.xyy),
    map(p + eps.yxy) - map(p - eps.yxy),
    map(p + eps.yyx) - map(p - eps.yyx)
  );
  return normalize(n);
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
  // pR(dir.xy, 2.5);
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
  vec3 rayDirection = normalize(dir);
  vec3 rayPosition = rayOrigin;
  float rayLength = 0.;

  float distance = 0.;
  vec3 color = vec3(0);
  for (float i = 0.; i < ITER; i++) {
    rayLength += distance;
    rayPosition = rayOrigin + rayDirection * rayLength;
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
