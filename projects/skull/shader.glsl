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

uniform bool guiBlend;
uniform bool guiBlendError;
uniform bool guiSplit;



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



float smin(float a, float b, float k){
    float f = clamp(0.5 + 0.5 * ((a - b) / k), 0., 1.);
    return (1. - f) * a + f  * b - f * (1. - f) * k;
}

float smax(float a, float b, float k) {
    return -smin(-a, -b, k);
}

float smin2(float a, float b, float r) {
    vec2 u = max(vec2(r - a,r - b), vec2(0));
    return max(r, min (a, b)) - length(u);
}

float smax2(float a, float b, float r) {
    vec2 u = max(vec2(r + a,r + b), vec2(0));
    return min(-r, max (a, b)) + length(u);
}

float smin3(float a, float b, float k){
    return min(
        smin(a, b, k),
        smin2(a, b, k)
    );
}

float smax3(float a, float b, float k){
    return max(
        smax(a, b, k),
        smax2(a, b, k)
    );
}





void pR(inout vec2 p, float a) {
    p = cos(a)*p + sin(a)*vec2(p.y, -p.x);
}

vec3 pRx(vec3 p, float a) {
    pR(p.yz, a); return p;
}

vec3 pRy(vec3 p, float a) {
    pR(p.xz, a); return p;
}

vec3 pRz(vec3 p, float a) {
    pR(p.xy, a); return p;
}



// iq https://www.shadertoy.com/view/MldfWn
float sdEllipse( vec2 p, in vec2 ab )
{
    p = abs( p ); if( p.x > p.y ){ p=p.yx; ab=ab.yx; }

    float l = ab.y*ab.y - ab.x*ab.x;
    
    float m = ab.x*p.x/l; 
    float n = ab.y*p.y/l; 
    float m2 = m*m;
    float n2 = n*n;
    
    float c = (m2 + n2 - 1.0)/3.0; 
    float c3 = c*c*c;

    float q = c3 + m2*n2*2.0;
    float d = c3 + m2*n2;
    float g = m + m*n2;

    float co;

    if( d<0.0 )
    {
        float h = acos(q/c3)/3.0;
        float s = cos(h);
        float t = sin(h)*sqrt(3.0);
        float rx = sqrt( -c*(s + t + 2.0) + m2 );
        float ry = sqrt( -c*(s - t + 2.0) + m2 );
        co = ( ry + sign(l)*rx + abs(g)/(rx*ry) - m)/2.0;
    }
    else
    {
        float h = 2.0*m*n*sqrt( d );
        float s = sign(q+h)*pow( abs(q+h), 1.0/3.0 );
        float u = sign(q-h)*pow( abs(q-h), 1.0/3.0 );
        float rx = -s - u - c*4.0 + 2.0*m2;
        float ry = (s - u)*sqrt(3.0);
        float rm = sqrt( rx*rx + ry*ry );
        co = (ry/sqrt(rm-rx) + 2.0*g/rm - m)/2.0;
    }

    float si = sqrt( 1.0 - co*co );
 
    vec2 r = ab * vec2(co,si);
    
    return length(r-p) * sign(p.y-r.y);
}

// generic ellipsoid - approximated distance: https://www.shadertoy.com/view/tdS3DG
float sdEllipsoid( in vec3 p, in vec3 r ) 
{
    float k0 = length(p/r);
    float k1 = length(p/(r*r));
    return k0*(k0-1.0)/k1;
}

// symmetric ellipsoid - EXACT distance
float sdEllipsoidXXZ( in vec3 p, in vec2 r ) 
{
    return sdEllipse( vec2( length(p.xy), p.z ), r );
}



float map(vec3 p) {
    p.x = abs(p.x);
    float d = 1e12;
    float back = sdEllipsoidXXZ(p - vec3(0,-.12,.16), vec2(.38, .29));
    d = min(d, back);
    float forehead = sdEllipsoidXXZ(pRx(p - vec3(0,-.15,-.13), .5), vec2(.35, .44) * .97);
    d = smin(d, forehead, .22);
    float backbump = sdEllipsoidXXZ(pRx(pRy(p - vec3(.22,-.27,.05), -.25), .25), vec2(.1, .2) * .5);
    d = smin(d, backbump, .3);
    float topbump = sdEllipsoidXXZ(pRx(p - vec3(0,-.33,-.05), -.0), vec2(.1, .15) * .5);
    d = smin(d, topbump, .3);
    // d = topbump;
    // d = 1e12;
    // d = min(d, abs(p.x) - .001);
    // d = min(d, abs(p.y) - .001);
    // d = min(d, abs(p.z) - .001);
    // d = max(d, length(p) - .7);
    return d;
}

float hitDebugPlane = 0.;

float mapDebug(vec3 p) {
    float d = map(p);
    // return d;

    p = (debugPlaneMatrix * vec4(p, 1)).xyz;

    float plane = abs(p.y);
    float r = .001;
    float marker = mix(length(p.xy) - r, length(p) - r, step(0., p.z));

    hitDebugPlane = plane < abs(d) ? 1. : 0.;
    d = min(d, plane);

    hitDebugPlane = marker < abs(d) ? 2. : hitDebugPlane;
    d = min(d, marker);

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
const int NUM_OF_TRACE_STEPS = 150;

const int NORMAL_STEPS = 6;
vec3 calcNormal(vec3 pos){
    vec3 eps = vec3(.00001,0,0);
    vec3 nor = vec3(0);
    float invert = 1.;
    for (int i = 0; i < NORMAL_STEPS; i++){
        nor += map(pos + eps * invert) * eps * invert;
        eps = eps.zxy;
        invert *= -1.;
    }
    return normalize(nor);
}

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
        currentDist = map(rayOrigin + rayDirection * rayLength);
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
        color = spectrum(smoothstep(.01, -.01, polyD - rayD));
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
