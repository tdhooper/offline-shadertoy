precision highp float;

uniform vec2 iResolution;
uniform sampler2D iChannel0; // buffer-a.glsl filter: linear wrap: clamp
uniform vec2 iChannel1Size;

void mainImage(out vec4 a, in vec2 b);

void main() {
    mainImage(gl_FragColor, gl_FragCoord.xy);
}

#ifdef GL_ES
precision mediump float;
#endif


void mainImage(out vec4 fragColor, in vec2 fragCoord) {
    vec2 uv = fragCoord.xy / iResolution.xy;
    vec4 sample = texture2D(iChannel0, uv);
    vec3 col = sample.rgb / sample.a;
    col = pow( col, vec3(0.4545) );
    fragColor = vec4(col, 1);    
}
