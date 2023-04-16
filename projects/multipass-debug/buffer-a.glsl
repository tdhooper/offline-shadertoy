// framebuffer firstpassonly

precision highp float;

uniform vec2 iResolution;
uniform mat4 cameraMatrix;
uniform float drawIndex;
uniform int iFrame;
uniform float iTime;

varying vec3 eye;
varying vec3 dir;
varying float fov;
varying float aspect;
varying mat4 vView;


void mainImage(out vec4 a, in vec2 b);

void main() {
    mainImage(gl_FragColor, gl_FragCoord.xy);
}

void mainImage(out vec4 fragColor, in vec2 fragCoord) {
    vec2 uv = fragCoord.xy / iResolution.xy;
    uv = fract(uv * 3.);
    fragColor = vec4(uv, 0, 1);
}
