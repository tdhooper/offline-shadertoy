precision highp float;

uniform vec2 iResolution;

uniform sampler2D iChannel0; // buffer-a.glsl filter: linear wrap: clamp
// filter: nearest, linear, mipmap
// wrap: clamp, repeat

void main() {
    vec2 uv = gl_FragCoord.xy / iResolution.xy;
    if (uv.x > .5) {
        uv = foo(uv);
    }
    gl_FragColor = texture2D(iChannel0, uv);
    // gl_FragColor = vec4(uv, 0, 1);
}
