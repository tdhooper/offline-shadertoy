precision highp float;

uniform vec2 iResolution;
uniform float iTime;

varying vec3 eye;
varying vec3 dir;

void mainImage(out vec4 a, in vec2 b);

void main() {
    mainImage(gl_FragColor, gl_FragCoord.xy);
}

mat3 basisMatrix(vec3 forward, vec3 up) {
    vec3 ww = normalize(forward);
    vec3 uu = normalize(cross(up,ww));
    vec3 vv = normalize(cross(ww,uu));
    return mat3(uu, vv, ww);
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


vec3 stepPosition = vec3(.0, .4, .3);
float stepScale = .1;
mat3 stepRotate;
