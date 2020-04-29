#extension GL_EXT_frag_depth : enable

precision mediump float;

uniform vec2 iResolution;
uniform vec2 iOffset;
uniform float iTime;

uniform sampler2D uSource;
uniform sampler2D uDepth;


uniform mat4 projection;
varying vec3 eye;
varying vec3 dir;
varying vec3 cameraForward;
varying mat4 vView;
varying float fov;
varying float aspect;
varying vec2 vVertex;

uniform vec3 debugPlanePosition;
uniform mat4 debugPlaneMatrix;

uniform bool guiModel;
uniform bool guiBlend;
uniform bool guiBlendError;
uniform bool guiSplit;


#define PI 3.141592653589793

vec3 pal( in float t, in vec3 a, in vec3 b, in vec3 c, in vec3 d ) {
    return a + b*cos( 6.28318*(c*t+d) );
}

vec3 spectrum(float n) {
    return pal( n, vec3(0.5,0.5,0.5),vec3(0.5,0.5,0.5),vec3(1.0,1.0,1.0),vec3(0.0,0.33,0.67) );
}

struct Model {
    float dist;
    vec3 material;
};

vec3 modelAlbedo = vec3(.5);

#pragma glslify: sdSkull = require(../skull/skull.glsl)

float map(vec3 p) {
    return sdSkull(p);
}

float hitDebugPlane = 0.;

float mapDebug(vec3 p) {
    float d = map(p);
    // d = 1e12;
    // return d;

    p = (debugPlaneMatrix * vec4(p, 1)).xyz;

    float plane = abs(p.y);
    float r = .001;
    float marker = mix(length(p.xy) - r, length(p) - r, step(0., p.z));

    hitDebugPlane = plane < abs(d) ? 1. : 0.;
    d = min(d, plane);

    // hitDebugPlane = marker < abs(d) ? 2. : hitDebugPlane;
    // d = min(d, marker);

    return d;
}


// --------------------------------------------------------
// Rendering
// --------------------------------------------------------

struct Hit {
    Model model;
    vec3 pos;
    bool isBackground;
    vec3 normal;
    vec3 rayOrigin;
    float rayLength;
    vec3 rayDirection;
    float steps;
};


// --------------------------------------------------------
// Ray Marching
// Adapted from: https://www.shadertoy.com/view/Xl2XWt
// --------------------------------------------------------

const float MAX_TRACE_DISTANCE = 3.5;
const float INTERSECTION_PRECISION = .0001;
const int NUM_OF_TRACE_STEPS = 550;

const int NORMAL_STEPS = 6;
vec3 calcNormal(vec3 pos){
    vec3 eps = vec3(.0001,0,0);
    vec3 nor = vec3(0);
    float invert = 1.;
    for (int i = 0; i < NORMAL_STEPS; i++){
        nor += map(pos + eps * invert) * eps * invert;
        eps = eps.zxy;
        invert *= -1.;
    }
    return normalize(nor);
}

// vec3 calcNormal( in vec3 pos )
// {
//     vec2 e = vec2(1.0,-1.0)*0.5773*0.0001;
//     return normalize( e.xyy*map( pos + e.xyy) + 
//                       e.yyx*map( pos + e.yyx) + 
//                       e.yxy*map( pos + e.yxy) + 
//                       e.xxx*map( pos + e.xxx) );
// }


Hit raymarch(vec3 rayOrigin, vec3 rayDirection){

    float currentDist = INTERSECTION_PRECISION * 2.0;
    float rayLength = 0.;
    bool isBackground = true;
    float steps = 0.;

    for(int i = 0; i < NUM_OF_TRACE_STEPS; i++){
        // if (currentDist < INTERSECTION_PRECISION * (1. + 50. * (rayLength / MAX_TRACE_DISTANCE)) ) {
        if (currentDist < INTERSECTION_PRECISION) {
            isBackground = false;
            break;
        }
        if (rayLength > MAX_TRACE_DISTANCE) {
            break;
        }
        //mapDebug
        currentDist = mapDebug(rayOrigin + rayDirection * rayLength);
        rayLength += currentDist;
        steps += 1.;
    }

    vec3 pos = vec3(0);
    vec3 normal = vec3(0);

    pos = rayOrigin + rayDirection * rayLength;

    if ( ! isBackground) {
        normal = calcNormal(pos);
    }

    Model model = Model(currentDist, modelAlbedo);

    return Hit(
        model,
        pos,
        isBackground,
        normal,
        rayOrigin,
        rayLength,
        rayDirection,
        steps
    );
}

float getDepth(float depth) {
    depth = projection[3].z / (depth * -2. + 1. - projection[2].z);
    return depth;
}

#pragma glslify: distanceMeter = require(../clifford-torus/distance-meter.glsl)



void main() {

    vec3 dir2 = dir;
    vec3 cameraForward2 = cameraForward;

    vec3 rayOrigin = eye;
    vec3 rayDirection = normalize(dir2);

    vec2 p = (-iResolution.xy + 2. * gl_FragCoord.xy) / iResolution.y;

    vec3 rayPosition = rayOrigin;

    vec3 bg = vec3(.7,.8,.9);

    Hit hit = raymarch(rayOrigin, rayDirection);

    vec3 color = bg;

    if ( ! hit.isBackground) {
        color = hit.normal * .5 + .5;
    }

    if (hitDebugPlane == 1.) {
        float d = map(hit.pos);
        color = distanceMeter(d * 2., hit.rayLength, hit.rayDirection, hit.rayOrigin);
    }

    if (hitDebugPlane == 2.) {
        color = vec3(1);
    }

    // color = vec3(0,0,1);
    // color = pow(color, vec3(1. / 2.2)); // Gamma

    float eyeHitZ = -hit.rayLength * dot(rayDirection, cameraForward2);

    vec3 eyeSpace = vec3(0, 0, eyeHitZ);
    float zc = ( projection * vec4(eyeSpace, 1)).z;
    float wc = ( projection * vec4(eyeSpace, 1)).w;
    float depth = (zc/wc + 1.) / 2.;


    float polyD = getDepth(texture2D(uDepth, gl_FragCoord.xy / iResolution.xy).r);
    float rayD = getDepth(depth);

    if (guiBlendError && ! hit.isBackground && ! guiSplit) {
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
        alpha = hit.pos.x < 0. ? 0. : 1.;
        // alpha = 0.;
    }

    if (guiModel) {
        alpha = 0.;
    }

    vec3 polyColor = texture2D(uSource, gl_FragCoord.xy / iResolution.xy).rgb;
    color = mix(polyColor, color, alpha);

    if (abs(polyD - rayD) < .001) {
        // color = vec3(1);
    }

    gl_FragDepthEXT = depth;
    

    // gl_FragColor = vec4(spectrum(hit.steps / float(NUM_OF_TRACE_STEPS)), 1); return;

    // float fog = 1. - exp( -(hit.rayLength - length(rayOrigin)) * 1.8 );
    // color = mix(color, bg, fog);

    // color *= 1.2;

    // float tintl = color.r;
    // tintl = pow(tintl, 2.);
    // vec3 tint = spectrum(mix(.5, -.2, tintl));
    // // tint *= pow(color, vec3(2.));
    // color *= mix(vec3(1.), tint, .3);
    // // color = pow(color, vec3(1. / 2.2)); // Gamma

    // color *= 1.2;

    // if (p.x > 0.) {
        // color = spectrum(hit.steps / 100.);
    // }

    // if (SHADE_DEBUG) color *= 2.;

    color = pow(color, vec3(1./2.2));

    gl_FragColor = vec4(color, 1);
    
}
