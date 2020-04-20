precision highp float;

uniform vec2 iResolution;
uniform sampler2D iChannel0; // buffer-a.glsl filter: linear wrap: clamp
uniform sampler2D iChannel1; // images/blue-noise.png filter: linear wrap: clamp
uniform vec2 iChannel1Size;

void mainImage(out vec4 a, in vec2 b);

void main() {
    mainImage(gl_FragColor, gl_FragCoord.xy);
}

#ifdef GL_ES
precision mediump float;
#endif

/* SHADERTOY FROM HERE */

/*

    Bloom
    -----

	Created for the Revision 2020 animated gif competition, achieving 1st place.
	As this gets rendered to a gif, image quality is given much more importance
	than rendering speed and code size; so yes, this is a lot of code, it takes
	a long time to compile, and it runs slow.

	I’ve disabled shadows and the depth of field pass so this has a chance of
	running, if you have a powerful GPU, you can enable them with the defines
	in common.

	Makes use of a few things I’ve worked on recently:

		* Uses a pseudo-3d texture to cache the skull distance function
		  https://www.shadertoy.com/view/WljSWz

		* Smooth fractal zoom to an arbitrary point and rotation
		  https://www.shadertoy.com/view/wslyzH

		* Parametric succulent model
		  https://www.shadertoy.com/view/WtGXWm

	The skull was modelled on this https://sketchfab.com/3d-models/visible-interactive-human-exploding-skull-252887e2e755427c90d9e3d0c6d3025f,
	using a method of overlaying my sdf model on the polygon model, and
	adjusting positions/sizes until the surfaces were close enough. It’s a slow
	and tedious process, I wouldn’t recommend it! I actually ran out of time to
	model the zygomatic arch.

	This photo by Scott Webb was my colour reference:
	https://www.pexels.com/photo/photo-of-succulent-plants-1903969/

	Special thanks to yx for giving me some serious competition with her entry:
    http://moonbase.lgbt/misc/yx-hexahedral-recurrence.gif

*/


// http://tuxedolabs.blogspot.com/2018/05/bokeh-depth-of-field-in-single-pass.html

vec2 uPixelSize; //The size of a pixel: vec2(1.0/width, 1.0/height)
float uFar = .32; // Far plane

const float GOLDEN_ANGLE = 2.39996323;
const float MAX_BLUR_SIZE = 30.;
const float RAD_SCALE = .2; // Smaller = nicer blur, larger = faster

float getBlurSize(float depth, float focusPoint, float focusScale) {
    float coc = clamp((1.0 / focusPoint - 1.0 / depth)*focusScale, -1.0, 1.0);
    return abs(coc) * MAX_BLUR_SIZE;
}

float dbg;

vec3 depthOfField(vec2 texCoord, float focusPoint, float focusScale) {
    vec4 centerTex = texture2D(iChannel0, texCoord);
    float centerDepth = centerTex.a * uFar;
    float centerSize = getBlurSize(centerDepth, focusPoint, focusScale);
    vec3 color = centerTex.rgb;
    
    #ifdef DISABLE_DOF
    	return color;
    #endif

    float tot = 1.0;

    float radius = RAD_SCALE;
    for (float ang = 0.; ang < 1000.; ang += GOLDEN_ANGLE) {
        if (radius >= MAX_BLUR_SIZE) break;

        dbg += 1.;
        vec2 tc = texCoord + vec2(cos(ang), sin(ang)) * uPixelSize * radius;
        vec4 sampleTex = texture2D(iChannel0, tc);
        vec3 sampleColor = sampleTex.rgb;
        float sampleDepth = sampleTex.a * uFar;
        float sampleSize = getBlurSize(sampleDepth, focusPoint, focusScale);
        //if (sampleSize < centerSize) break;
        if (sampleDepth > centerDepth) {
            sampleSize = clamp(sampleSize, 0.0, centerSize*2.0);
        }
        float m = smoothstep(radius-0.5, radius+0.5, sampleSize);
        color += mix(color/tot, sampleColor, m);
        tot += 1.0;
        radius += RAD_SCALE/radius;
        
        // modification: exit early when we're in focus
        if (centerDepth < uFar / 3. && m == 0.) break;
    }
    return color /= tot;
}

vec3 aces(vec3 x) {
  const float a = 2.51;
  const float b = 0.03;
  const float c = 2.43;
  const float d = 0.59;
  const float e = 0.14;
  return clamp((x * (a * x + b)) / (x * (c * x + d) + e), 0.0, 1.0);
}
 
void mainImage(out vec4 fragColor, in vec2 fragCoord) {
    vec2 uv = fragCoord.xy / iResolution.xy;
    uPixelSize = vec2(.002) / (iResolution.xy / iResolution.y);

    float focusA = .03 * 2.;
    float focusAStart = .25;
    float focusAEnd = .65;

    float focusB = .045 * 2.;
    float focusBStart = .65;
    float focusBEnd = 1.;

    float time = loopTime(iTime);
    float blend = smoothstep(focusAStart, focusAEnd, time) - smoothstep(focusBStart, focusBEnd, time);
    float focus = mix(focusB, focusA, blend);

    dbg = 0.;
    vec3 col = depthOfField(uv, focus, .05);

    // fix banding
    vec4 grain = texture2D(iChannel1, fragCoord.xy / iChannel1Size.x);
	col += (grain.x * 2. - 1.) * .002;
    
    col = pow( col, vec3(0.4545) ) * sign(col);
    col = aces(col);

    fragColor = vec4(col, 1);    
}
