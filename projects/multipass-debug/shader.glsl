precision highp float;

uniform vec2 iResolution;
uniform sampler2D iChannel0; // buffer-b.glsl filter: linear wrap: clamp

void mainImage(out vec4 a, in vec2 b);

void main() {
    mainImage(gl_FragColor, gl_FragCoord.xy);
}

#ifdef GL_ES
precision mediump float;
#endif

void mainImage(out vec4 fragColor, in vec2 fragCoord) {
    vec2 uv = fragCoord.xy / iResolution.xy;
    uv -= .666 * .25;
    uv /= .666;
    vec4 sample = texture2D(iChannel0, uv);
    vec3 col = sample.rgb;
    if (max(uv.x, uv.y) > 1. || min(uv.x, uv.y) < 0.) {
        col = vec3(0);
    }
    col = pow( col, vec3(0.4545) );
    fragColor = vec4(col, 1);    
}
