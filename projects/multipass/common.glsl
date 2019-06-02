precision highp float;

vec2 foo(vec2 uv) {
    return mix(uv, sin(uv * 10.), .5);
}
