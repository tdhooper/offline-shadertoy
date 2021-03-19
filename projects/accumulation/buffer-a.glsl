// framebuffer drawcount: 9 tile: 3

precision highp float;

uniform vec2 iResolution;
uniform mat4 cameraMatrix;
uniform sampler2D previousSample; // buffer-a.glsl filter: linear
uniform float drawIndex;

varying vec3 eye;
varying vec3 dir;
varying float fov;
varying float aspect;
varying mat4 vView;


void mainImage(out vec4 a, in vec2 b);

void main() {
    mainImage(gl_FragColor, gl_FragCoord.xy);
}

struct Model {
    float d;
};

Model map(vec3 p) {
    float d = length(p) - .5;
    return Model(d);
}

vec3 calcNormal(vec3 p) {
  vec3 eps = vec3(.00001,0,0);
  vec3 n = vec3(
    map(p + eps.xyy).d - map(p - eps.xyy).d,
    map(p + eps.yxy).d - map(p - eps.yxy).d,
    map(p + eps.yyx).d - map(p - eps.yyx).d
  );
  return normalize(n);
}

void mainImage(out vec4 fragColor, in vec2 fragCoord) {

    float AA = 3.;
    float m = floor(drawIndex / AA);
    float n = floor(mod(drawIndex, AA));
    vec2 o = vec2(m, n) / AA - 0.5;

    vec2 p = (-iResolution.xy + 2. * (fragCoord + o)) / iResolution.y;

    vec3 camPos = eye;
    vec3 rayDirection = normalize(vec3(p.x * fov, p.y * fov, -1.) * mat3(vView));

    vec3 rayPosition = camPos;
    float rayLength = 0.;
    Model model;
    float dist = 0.;
    bool bg = false;

    for (int i = 0; i < 300; i++) {
        rayLength += dist;
        rayPosition = camPos + rayDirection * rayLength;
        model = map(rayPosition);
        dist = model.d;

        if (dist < .0001) {
            break;
        }
        
        if (rayLength > 100.) {
            bg = true;
            break;
        }
    }

    vec3 col = vec3(.1);

    if ( ! bg) {
        col = calcNormal(rayPosition) * .5 + .5;
    }

    if (drawIndex > 0.) {
        vec3 lastCol = texture2D(previousSample, fragCoord.xy / iResolution.xy).rgb;
        col = mix(lastCol, col, 1. / (drawIndex + 1.));
    }

    fragColor = vec4(col, 1);
}
