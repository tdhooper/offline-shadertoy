precision highp float;

uniform vec2 iResolution;
uniform float iTime;
uniform sampler2D iChannel0; // buffer-a.glsl filter: linear wrap: clamp

void main() {
    vec2 uv = gl_FragCoord.xy / iResolution.xy;
    if (uv.y > .5) {
        gl_FragColor = texture2D(iChannel0, uv - .01);
    } else {
        uv = foo(uv);
        gl_FragColor = vec4(uv, sin(iTime) * .5 + .5, 1);
    }
}
