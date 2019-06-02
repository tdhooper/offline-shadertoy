precision highp float;

uniform vec2 iResolution;
uniform float iTime;

void main() {
    vec2 uv = gl_FragCoord.xy / iResolution.xy;
    uv = foo(uv);
    gl_FragColor = vec4(uv, sin(iTime) * .5 + .5, 1);
}
