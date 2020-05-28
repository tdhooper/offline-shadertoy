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

//#define DISABLE_DOF;

/* SHADERTOY FROM HERE */


// http://tuxedolabs.blogspot.com/2018/05/bokeh-depth-of-field-in-single-pass.html

vec2 uPixelSize; //The size of a pixel: vec2(1.0/width, 1.0/height)
float uFar = 1.; // Far plane

const float GOLDEN_ANGLE = 2.39996323;
const float MAX_BLUR_SIZE = 10.;
const float RAD_SCALE = 1.; // Smaller = nicer blur, larger = faster

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
    for (float ang = 0.; ang < 10000.; ang += GOLDEN_ANGLE) {
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
       // if (centerDepth < uFar / 3. && m == 0.) break;
    }
    return color /= tot;
}


// http://filmicworlds.com/blog/filmic-tonemapping-operators/
vec3 tonemap2(vec3 texColor) {
    texColor /= 2.;
   	texColor *= 16.;  // Hardcoded Exposure Adjustment
   	vec3 x = max(vec3(0),texColor-0.004);
   	return (x*(6.2*x+.5))/(x*(6.2*x+1.7)+0.06);
}

void mainImage(out vec4 fragColor, in vec2 fragCoord) {
    vec2 uv = fragCoord.xy / iResolution.xy;
    uPixelSize = vec2(.002) / (iResolution.xy / iResolution.y);

    dbg = 0.;
    vec3 col = depthOfField(uv, .4, .35);

    // fix banding
    vec4 grain = texture2D(iChannel1, fragCoord.xy / iChannel1Size.x);
	//col += (grain.x * 2. - 1.) * .002;


    col = pow(col, vec3(1.25)) * 2.5;
    
    ////col = pow(col, vec3(1.125)) * 1.5;    
    ////col = pow( col, vec3(0.4545) );

    col = tonemap2(col);

    fragColor = vec4(col, 1);    
}
