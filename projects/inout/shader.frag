precision highp float;

uniform vec2 iResolution;
uniform sampler2D iChannel0; // buffer-a.frag filter: linear wrap: clamp

void mainImage(out vec4 a, in vec2 b);

void main() {
    mainImage(gl_FragColor, gl_FragCoord.xy);
}

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
    vec2 uv = fragCoord.xy / iResolution.xy;
    vec3 col = texture2D(iChannel0, uv).rgb;
    //fragColor = vec4(col, 1); return;
    vec3 uGain = vec3(1.8);
    vec3 uLift = vec3(.002,-.003,.007)/3.;
    vec3 uOffset = vec3(.00,.00,.00);
    vec3 uGamma = vec3(-.25);
    
	//col = mix(col, vec3(Luma(col)), .25);
    col = pow(max(vec3(0.0), col * (1.0 + uGain - uLift) + uLift + uOffset), max(vec3(0.0), 1.0 - uGamma));
	//col = max(col, vec3(0));
    col = pow( col, vec3(1./2.2) );
    col = aces(col);
	fragColor = vec4(col, 1);
}
