precision highp float;

uniform vec2 iResolution;
uniform sampler2D iChannel0; // buffer-a.glsl filter: linear wrap: clamp

void mainImage(out vec4 a, in vec2 b);

void main() {
    mainImage(gl_FragColor, gl_FragCoord.xy);
}

#ifdef GL_ES
precision mediump float;
#endif

// http://filmicworlds.com/blog/filmic-tonemapping-operators/
vec3 tonemap2(vec3 texColor) {
    texColor /= 2.;
   	texColor *= 16.;  // Hardcoded Exposure Adjustment
   	vec3 x = max(vec3(0),texColor-0.004);
   	return (x*(6.2*x+.5))/(x*(6.2*x+1.7)+0.06);
}

void mainImage(out vec4 fragColor, in vec2 fragCoord) {
    vec2 uv = fragCoord.xy / iResolution.xy;

    vec3 col = texture2D(iChannel0, uv).rgb;

    col = pow(col, vec3(1.25)) * 2.5;
    col = tonemap2(col);

    fragColor = vec4(col, 1);    
}
