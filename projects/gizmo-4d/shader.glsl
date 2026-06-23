#version 300 es
precision mediump float;

float gmTransform(inout vec3 p, vec3 t, vec4 r, vec3 s) {
  p -= t;
  p = mix(dot(r.xyz,p)*r.xyz, p, cos(-r.w))+sin(-r.w)*cross(r.xyz,p);
  p /= s;
  return min(s.x, min(s.y, s.z));
}


precision mediump float;

in vec3 eye;
in vec3 dir;
in vec3 cameraForward;

out vec4 fragColor;

uniform mat4 projection;
uniform int iFrame;


float vmax(vec3 v) {
	return max(max(v.x, v.y), v.z);
}

float fBox(vec3 p, vec3 b) {
	vec3 d = abs(p) - b;
	return length(max(d, vec3(0))) + vmax(min(d, vec3(0)));
}

float fBox4(vec4 p, vec4 b) {
   return length(p) - b.x;
  vec4 d = abs(p) - b;
  return min( max(max(d.x,d.y),max(d.z,d.w)),0.0) + length(max(d,0.0));
}

float rbox(vec4 p) {
  gmTransform(p.xyz, vec3(0.1618881,-0.9103126,0.3809413), vec4(0.8152734,-0.3090453,0.4897144,4.3307749), vec3(1,1,1));
  return fBox4(p, vec4(.5));
}

float map(vec3 pos) {
  // take a 3D slice
  vec4 p = vec4(pos, 0);

  float d = 1e12;
  float scl = 1.;

  float slice = p.z;
  const int count = 124;

  for (int i = 0; i < count; i++) {
    p.x = abs(p.x);
    scl *= gmTransform(p.xyw, vec3(1.2870606,-0.6327413,1.5376602), vec4(0.6199048,0.6546051,0.4326779,3.9049305), vec3(1,1,1));
    scl *= gmTransform(p.xyz, vec3(0,0,0), vec4(-0.5012654,0.8057376,0.3154676,0.8042507), vec3(0.8,0.8,0.8));
  }
    d = min(d, rbox(p) * scl);
  
// d = max(d, slice);

  return d;
}

float GIZMO_MAP(vec3 p) {
    return map(p);
}

const int NORMAL_STEPS = 6;
vec3 calcNormal(vec3 pos){
    vec3 eps = vec3(.0005,0,0);
    vec3 nor = vec3(0);
    float invert = 1.;
    vec3 npos;
    for (int i = 0; i < NORMAL_STEPS; i++){
        npos = pos + eps * invert;
        nor += map(npos) * eps * invert;
        eps = eps.zxy;
        invert *= -1.;
    }
    return normalize(nor);
}

const float ITER = 50.;

void main() {

  vec3 rayOrigin = eye;
  vec3 rayDirection = normalize(dir);
  vec3 rayPosition = rayOrigin;
  float rayLength = 0.;

  float dist = 0.;
  vec3 color = vec3(0);
  for (float i = 0.; i < ITER; i++) {
    rayLength += dist;
    rayPosition = rayOrigin + rayDirection * rayLength;
    dist = map(rayPosition);
    color += .05;
    if (dist < .001) {
      color *= calcNormal(rayPosition) * .5 + .5;
      break;
    }
  }
  
  color = pow(color, vec3(1./2.2));

  fragColor = vec4(color, 1);
}
