#extension GL_EXT_frag_depth : enable

precision mediump float;

uniform mat4 projection;
uniform vec2 iResolution;
uniform sampler2D uSource;
uniform sampler2D uDepth;

uniform bool guiModel;
uniform bool guiBlend;
uniform bool guiBlendError;
uniform bool guiSplit;

varying vec3 eye;
varying vec3 dir;
varying vec3 cameraForward;

vec3 pal( in float t, in vec3 a, in vec3 b, in vec3 c, in vec3 d ) {
    return a + b*cos( 6.28318*(c*t+d) );
}

vec3 spectrum(float n) {
    return pal( n, vec3(0.5,0.5,0.5),vec3(0.5,0.5,0.5),vec3(1.0,1.0,1.0),vec3(0.0,0.33,0.67) );
}

float map(vec3 p) {
    float d = length(p) - .66;
    return d;
}

vec3 calcNormal(vec3 p) {
    vec3 eps = vec3(.001,0,0);
    vec3 n = vec3(
    map(p + eps.xyy) - map(p - eps.xyy),
    map(p + eps.yxy) - map(p - eps.yxy),
    map(p + eps.yyx) - map(p - eps.yyx)
    );
    return normalize(n);
}

float getDepth(float depth) {
    depth = projection[3].z / (depth * -2. + 1. - projection[2].z);
    return depth;
}

const float ITER = 50.;

void main() {

    vec3 rayOrigin = eye;
    vec3 rayDirection = normalize(dir);
    vec3 rayPosition = rayOrigin;
    float rayLength = 0.;

    bool isBackground = true;
    float dist = 0.;
    vec3 color = vec3(0);
    for (float i = 0.; i < ITER; i++) {
        rayLength += dist;
        rayPosition = rayOrigin + rayDirection * rayLength;
        dist = map(rayPosition);
        color += .05;
        if (dist < .001) {
            color *= calcNormal(rayPosition) * .5 + .5;
            isBackground = false;
            break;
        }
    }

    float eyeHitZ = -rayLength * dot(rayDirection, cameraForward);

    vec3 eyeSpace = vec3(0, 0, eyeHitZ);
    float zc = ( projection * vec4(eyeSpace, 1)).z;
    float wc = ( projection * vec4(eyeSpace, 1)).w;
    float depth = (zc/wc + 1.) / 2.;


    float polyD = getDepth(texture2D(uDepth, gl_FragCoord.xy / iResolution.xy).r);
    float rayD = getDepth(depth);

    if (guiBlendError && ! isBackground && ! guiSplit) {
        color = spectrum((1. - smoothstep(.0, .04, distance(polyD, rayD))) * .6);
        if (polyD > rayD) {
            color *= .5;
        }
    }

    float alpha = smoothstep(.06, -.06, polyD - rayD);

    // alpha = .5;

    if (polyD > rayD) {
        alpha = max(0., alpha - .1);
    }

    // alpha = .5;

    if ( ! guiBlendError && ! guiBlend) {
        alpha = 1.;
    }

    // if (guiBlend) {
    //     alpha = polyD > rayD ? 0. : 1.;
    // }

    if (guiSplit) {
        alpha = rayPosition.x < 0. ? 0. : 1.;
        // alpha = 0.;
    }

    if (guiModel) {
        alpha = 0.;
    }

    vec3 polyColor = texture2D(uSource, gl_FragCoord.xy / iResolution.xy).rgb;
    polyColor = pow(polyColor, vec3(2.2));
    color = mix(polyColor, color, alpha);

    if (abs(polyD - rayD) < .001) {
        // color = vec3(1);
    }

    gl_FragDepthEXT = depth;

    color = pow(color, vec3(1./2.2));

    gl_FragColor = vec4(color, 1);
    gl_FragDepthEXT = depth;
}