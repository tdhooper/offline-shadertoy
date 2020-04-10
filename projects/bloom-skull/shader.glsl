precision highp float;

uniform vec2 iResolution;
uniform sampler2D uTexture; // buffer-a.glsl filter: linear wrap: clamp

void mainImage(out vec4 a, in vec2 b);

void main() {
    mainImage(gl_FragColor, gl_FragCoord.xy);
}

#ifdef GL_ES
precision mediump float;
#endif


/* SHADERTOY FROM HERE */

// http://tuxedolabs.blogspot.com/2018/05/bokeh-depth-of-field-in-single-pass.html

vec2 uPixelSize; //The size of a pixel: vec2(1.0/width, 1.0/height)
float uFar = 2.; // Far plane

const float GOLDEN_ANGLE = 2.39996323;
const float MAX_BLUR_SIZE = 30.;
const float RAD_SCALE = .2; // Smaller = nicer blur, larger = faster

float getBlurSize(float depth, float focusPoint, float focusScale) {
    float coc = clamp((1.0 / focusPoint - 1.0 / depth)*focusScale, -1.0, 1.0);
    return abs(coc) * MAX_BLUR_SIZE;
}

vec3 depthOfField(vec2 texCoord, float focusPoint, float focusScale) {
    vec4 centerTex = texture2D(uTexture, texCoord);
    float centerDepth = centerTex.a * uFar;
    float centerSize = getBlurSize(centerDepth, focusPoint, focusScale);
    vec3 color = centerTex.rgb;
    return color;
    float tot = 1.0;

    float radius = RAD_SCALE;
    for (float ang = 0.; ang < 2000.; ang += GOLDEN_ANGLE) {
        if (radius >= MAX_BLUR_SIZE) {
            break;
        }
        vec2 tc = texCoord + vec2(cos(ang), sin(ang)) * uPixelSize * radius;
        vec4 sampleTex = texture2D(uTexture, tc);
        vec3 sampleColor = sampleTex.rgb;
        float sampleDepth = sampleTex.a * uFar;
        float sampleSize = getBlurSize(sampleDepth, focusPoint, focusScale);
        if (sampleDepth > centerDepth) {
            sampleSize = clamp(sampleSize, 0.0, centerSize*2.0);
        }
        float m = smoothstep(radius-0.5, radius+0.5, sampleSize);
        color += mix(color/tot, sampleColor, m);
        tot += 1.0;
        radius += RAD_SCALE/radius;
    }
    return color /= tot;
}

#pragma glslify: aces = require(glsl-tone-map/aces)
#pragma glslify: range = require(glsl-range)

float calcLum(vec3 color) {
 	float fmin = min(min(color.r, color.g), color.b); //Min. value of RGB
 	float fmax = max(max(color.r, color.g), color.b); //Max. value of RGB
 	return (fmax + fmin) / 2.0; // Luminance
}
 
void mainImage(out vec4 fragColor, in vec2 fragCoord) {
    vec2 uv = fragCoord.xy / iResolution.xy;
    // uv.x = 1.- uv.x;
    uPixelSize = vec2(.002) / (iResolution.xy / iResolution.y);

    float scaleA = .05;
    float focusA = .03;
    float focusAStart = .0;
    float focusAEnd = .5;

    float scaleB = .05;
    float focusB = .045;
    float focusBStart = .5;
    float focusBEnd = .7;

    float blend = smoothstep(focusAStart, focusAEnd, mTime) - smoothstep(focusBStart, focusBEnd, mTime) + smoothstep(focusAStart, focusAEnd, mTime - 1.);
    float focus = mix(focusB, focusA, blend);
    float scale = mix(scaleB, scaleA, blend);

    // focus = .045;
    // scale = .06;

    vec3 col = depthOfField(uv, focus * uFar, scale);

    // col = max(col, vec3(.1));
    // float l = calcLum(col);
    // col = mix(col, col * 1.6, l);
    // col *= mix(vec3(1), vec3(1., 1., 3.), pow(1.-l, 2.) * .3);
    // col *= mix(vec3(1), vec3(.6, .6, 1.2), pow(l, 2.) * .3);
    // col = aces(col);

    // col = max(col, vec3(.05));
    // float l = calcLum(col);
    // col = mix(col, col * 1.3, l);
    // col *= mix(vec3(1), vec3(1., 1., 3.), pow(1.-l, 2.) * .3);
    // col *= mix(vec3(1), vec3(.6, .6, 1.2), pow(l, 2.) * .3);
    

    //col *= 1.3;    
    col = pow( col, vec3(0.4545) );
    col = aces(col);

    // col += vec3(blend/2.,0,0);

    fragColor = vec4(col, 1);
    
}
