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
    vec2 p = (-iResolution.xy + 2.0*fragCoord.xy)/iResolution.y;
    p.x += 1.;
    p *= 1.5;
    vec3 color;

    float d = fNova(p);

    color = vec3(smoothstep(0.01, .0, d));
    color += vec3(0,1,1) * mod(d * 5., 1.) * .5;

    fragColor = vec4(color,1.0);
}
