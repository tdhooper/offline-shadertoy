#version 300 es

precision highp float;

uniform vec2 iResolution;
uniform vec2 iOffset;
uniform float iGlobalTime;
uniform vec4 iMouse;
uniform sampler2D iChannel0; // buffer-a.glsl filter: linear wrap: clamp

out vec4 fragColor;

void mainImage(out vec4 a, in vec2 b);

void main() {
    mainImage(fragColor, gl_FragCoord.xy + iOffset.xy);
}

#ifdef GL_ES
precision mediump float;
#endif

/*

    Fragments
    ---------

    Relax and unwind in the distance fields
    
    a 4k executable graphics entry for Névoke 2021

*/


// colour grading from tropical trevor's scripts
// https://github.com/trevorvanhoof/ColorGrading

#define sat(x) clamp(x,0.,1.)

// from http://www.tannerhelland.com/4435/convert-temperature-rgb-algorithm-code/
vec3 colorFromKelvin(float temperature) // photographic temperature values are between 15 to 150
{
    float r, g, b;
    if(temperature <= 66.0)
    {
        r = 1.0;
        g = sat((99.4708025861 * log(temperature) - 161.1195681661) / 255.0);
        if(temperature < 19.0)
            b = 0.0;
        else
            b = sat((138.5177312231 * log(temperature - 10.0) - 305.0447927307) / 255.0);
    }
    else
    {
        r = sat((329.698727446 / 255.0) * pow(temperature - 60.0, -0.1332047592));
        g = sat((288.1221695283  / 255.0) * pow(temperature - 60.0, -0.0755148492));
        b = 1.0;
    }
    return vec3(r, g, b);
}

vec3 aces(vec3 x) {
  const float a = 2.51;
  const float b = 0.03;
  const float c = 2.43;
  const float d = 0.59;
  const float e = 0.14;
  return clamp((x * (a * x + b)) / (x * (c * x + d) + e), 0.0, 1.0);
}

float Luma(vec3 color) { return dot(color, vec3(0.2126, 0.7152, 0.0722)); }

void mainImage(out vec4 fragColor, in vec2 fragCoord) {
	vec4 tex = texelFetch(iChannel0, ivec2(fragCoord.xy), 0);
	vec3 col = tex.rgb / tex.a;

    //fragColor = vec4(col, 1); return;

    // saturation
	float luma = Luma(col);
	col = mix(vec3(luma), col, 1.25);

    // temperature
    col *= 1. / colorFromKelvin(100.);

    vec3 uGain = vec3(1.333);
    vec3 uLift = vec3(.0015,.00,.005) * 1.25;
    vec3 uOffset = vec3(.00,.00,.00);
    vec3 uGamma = vec3(.0666);
    col = pow(max(vec3(0.0), col * (1.0 + uGain - uLift) + uLift + uOffset), max(vec3(0.0), 1.0 - uGamma));
    col = aces(col);
    col = pow( col, vec3(1./2.2) );
    
    fragColor = vec4(col, 1);
}

