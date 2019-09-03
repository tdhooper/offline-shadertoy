precision highp float;

uniform vec2 iResolution;
uniform sampler2D uTexture; // buffer-a.glsl filter: linear wrap: repeat
uniform float iTime;

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
float uFar = 1.5; // Far plane

const float GOLDEN_ANGLE = 2.39996323;
const float MAX_BLUR_SIZE = 20.;
const float RAD_SCALE = 0.5; // Smaller = nicer blur, larger = faster

float getBlurSize(float depth, float focusPoint, float focusScale) {
    float coc = clamp((1.0 / focusPoint - 1.0 / depth)*focusScale, -1.0, 1.0);
    return abs(coc) * MAX_BLUR_SIZE;
}

vec3 depthOfField(vec2 texCoord, float focusPoint, float focusScale) {
    vec4 centerTex = texture2D(uTexture, texCoord);
    float centerDepth = centerTex.a * uFar;
    float centerSize = getBlurSize(centerDepth, focusPoint, focusScale);
    vec3 color = centerTex.rgb;
    float tot = 1.0;

    float radius = RAD_SCALE;
    for (float ang = 0.; ang < 1000.; ang += GOLDEN_ANGLE) {
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

void mainImage(out vec4 fragColor, in vec2 fragCoord) {
    vec2 uv = fragCoord.xy / iResolution.xy;
    uPixelSize = vec2(1) / iResolution.xy;
    uPixelSize = vec2(.003);
    vec3 col = depthOfField(uv, .09 * uFar, .1);
    col = pow(col, vec3(0.4545));
    fragColor = vec4(col, 1);
}
