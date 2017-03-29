#define SHADER_NAME quad.frag

precision highp float;

uniform vec2 iResolution;
uniform float iGlobalTime;
uniform vec4 iMouse;


void mainImage(out vec4 a, in vec2 b);

void main() {
    mainImage(gl_FragColor, gl_FragCoord.xy);
}


/* SHADERTOY FROM HERE */

#pragma glslify: rand = require(glsl-random)

void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
	vec2 uv = fragCoord.xy / iResolution.xy;
	fragColor = vec4(uv,0.5+0.5*sin(iGlobalTime),1.0);
}
