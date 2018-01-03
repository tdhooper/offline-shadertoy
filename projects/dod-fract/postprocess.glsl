precision highp float;

uniform vec2 iResolution;
uniform vec2 iOffset;
uniform float iGlobalTime;
uniform vec4 iMouse;
uniform sampler2D iChannel0;


void mainImage(out vec4 a, in vec2 b);

void main() {
    mainImage(gl_FragColor, gl_FragCoord.xy + iOffset.xy);
}

#ifdef GL_ES
precision mediump float;
#endif

/* SHADERTOY FROM HERE */

void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
    vec2 uv = fragCoord.xy / iResolution.xy;
    // fragColor= vec4(uv,0,1);
    fragColor = texture2D(iChannel0, uv) * vec4(mod(iGlobalTime, 1.), .9, .5, 1.);
}
