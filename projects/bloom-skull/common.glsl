precision highp float;

uniform vec2 iResolution;
uniform float iTime;

varying vec3 eye;
varying vec3 dir;

void mainImage(out vec4 a, in vec2 b);

void main() {
    mainImage(gl_FragColor, gl_FragCoord.xy);
}

vec3 stepPosition = vec3(.1, .1, .2) * 3.;
// float stepScale = 1.;
float stepScale = .7;
vec3 stepNormal = normalize(vec3(1,1,.4));
