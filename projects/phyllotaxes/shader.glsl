#version 300 es

precision highp float;

uniform vec2 iResolution;
uniform vec2 iOffset;
uniform float iGlobalTime;
uniform vec4 iMouse;
uniform sampler2D iChannel0; // buffer-a.glsl filter: linear wrap: clamp

out vec4 fragColorOut;

void mainImage(out vec4 a, in vec2 b);

void main() {
    mainImage(fragColorOut, gl_FragCoord.xy + iOffset.xy);
}

#ifdef GL_ES
precision mediump float;
#endif


/*

    Phyllotaxes
    -----------

	4k executable graphics entry for Nova 2020

	Model from my earlier succulent shaders:

	    Bloom [skull] https://www.shadertoy.com/view/WdScDG
	    Echeveria II https://www.shadertoy.com/view/WtGXWm
	    Echeveria https://www.shadertoy.com/view/wlVGRz

	Lighting and tracing loop adapted from yx's Primitive Portrait:

	    https://www.shadertoy.com/view/ts2cWm

*/


vec3 aces(vec3 x) {
  const float a = 2.51;
  const float b = 0.03;
  const float c = 2.43;
  const float d = 0.59;
  const float e = 0.14;
  return clamp((x * (a * x + b)) / (x * (c * x + d) + e), 0.0, 1.0);
}

// colour grading from tropical trevor's scripts
// https://github.com/trevorvanhoof/ColorGrading
float Luma(vec3 color) { return dot(color, vec3(0.2126, 0.7152, 0.0722)); }

void mainImage(out vec4 fragColor, in vec2 fragCoord) {
	vec3 col = texelFetch(iChannel0, ivec2(fragCoord.xy), 0).rgb;
    vec3 uGain = vec3(1.8);
    vec3 uLift = vec3(.002,-.003,.007);
    vec3 uOffset = vec3(.00,.00,.00);
    vec3 uGamma = vec3(-.3);
    
	col = mix(col, vec3(Luma(col)), .25);
    col = pow(max(vec3(0.0), col * (1.0 + uGain - uLift) + uLift + uOffset), max(vec3(0.0), 1.0 - uGamma));
	col = max(col, vec3(0));
    col = pow( col, vec3(0.4545) );
    col = aces(col);
	fragColor = vec4(col,1);
}
