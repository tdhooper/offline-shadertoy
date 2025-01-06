#version 300 es

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

float map(vec3 p) {
    float d = 1e12;

    float scl = 1.;

    scl *= gmTransform(p, vec3(0,1,0), vec4(1,0,0,0), vec3(1,1,1));

    for (int i = 0; i < 3; i++) {
        d = min(d, fBox(p, vec3(.5)) * scl);
        scl *= gmTransform(p, vec3(1,-0.2,-0.5), vec4(0,1,0,1), vec3(0.8,0.8,0.8));
        //GIZMO(p);
    }

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
