precision highp float;

uniform vec2 iResolution;
uniform float iTime;

varying vec3 eye;
varying vec3 dir;

void mainImage(out vec4 a, in vec2 b);

void main() {
    mainImage(gl_FragColor, gl_FragCoord.xy);
}

// vec3 stepPosition = vec3(.1, .1, .2) * 3.;
// float stepScale = .6;
// vec3 stepNormal = normalize(vec3(1,1,.1));

// vec3 stepPosition = vec3(.1, .1, .2) * 2.;
// float stepScale = .6;
// vec3 stepNormal = normalize(vec3(1,1,.4));

// vec3 stepPosition = vec3(0, -.35, 0);
// float stepScale = .1;
// vec3 stepNormal = normalize(vec3(.1,1,0));


vec3 stepPosition = normalize(vec3(.5, 1, .5)) * .35;
float stepScale = .1;
vec3 stepNormal = normalize(stepPosition);


