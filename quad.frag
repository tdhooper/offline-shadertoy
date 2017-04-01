#define SHADER_NAME quad.frag

precision highp float;

uniform vec2 iResolution;
uniform float iGlobalTime;
uniform vec4 iMouse;
uniform sampler2D iChannel0;


void mainImage(out vec4 a, in vec2 b);

void main() {
    mainImage(gl_FragColor, gl_FragCoord.xy);
}


/* SHADERTOY FROM HERE */

#pragma glslify: rand = require(glsl-random)

void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
	vec2 uv = fragCoord.xy / iResolution.xy;
	vec3 rgb = texture2D(iChannel0, uv * 10.5).rgb;
	fragColor = vec4(rgb, 1.);
	
	//vec2 rg = (fragCoord.xy + iMouse.xy) / 2. / iResolution.xy;
	//float b = sin(iGlobalTime) * .5 + .5;
	//fragColor = vec4(rg, b, 1.);
}
