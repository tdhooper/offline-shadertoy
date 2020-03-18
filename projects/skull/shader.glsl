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

// Shortcut for 45-degrees rotation
void pR45(inout vec2 p) {
    p = (p + vec2(p.y, -p.x))*sqrt(0.5);
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



// sdUberprim with precomputed constants
float sdUnterprim(vec3 p, vec4 s, vec3 r, vec2 ba, float sz2) {
    vec3 d = abs(p) - s.xyz;
    float q = length(max(d.xy, 0.0)) + min(0.0,max(d.x,d.y)) - r.x;
    // hole support: without this line, all results are convex
#ifndef CONVEX    
    q = abs(q) - s.w;
#endif
    
    vec2 pa = vec2(q, p.z - s.z);
    vec2 diag = pa - vec2(r.z,sz2) * clamp(dot(pa,ba), 0.0, 1.0);
    vec2 h0 = vec2(max(q - r.z,0.0),p.z + s.z);
    vec2 h1 = vec2(max(q,0.0),p.z - s.z);
    
    return sqrt(min(dot(diag,diag),min(dot(h0,h0),dot(h1,h1))))
        * sign(max(dot(pa,vec2(-ba.y, ba.x)), d.z)) - r.y;
}

// s: width, height, depth, thickness
// r: xy corner radius, z corner radius, bottom radius offset
float sdUberprim(vec3 p, vec4 s, vec3 r) {
    // these operations can be precomputed
    s.xy -= r.x;
#ifdef CONVEX  
    r.x -= r.y;
#else
    r.x -= s.w;
    s.w -= r.y;
#endif
    s.z -= r.y;
    vec2 ba = vec2(r.z, -2.0*s.z);
    return sdUnterprim(p, s, r, ba/dot(ba,ba), ba.y);
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

// Torus in the XZ-plane
float fTorus(vec3 p, float smallRadius, float largeRadius) {
    return length(vec2(length(p.xz) - largeRadius, p.y)) - smallRadius;
}

float fEllipseTorus(vec3 p, vec2 smallRadius, float largeRadius) {
    vec2 c = vec2(length(p.xz), p.y);
    c.x -= largeRadius;
    return sdEllipse(c, smallRadius);
}

float fTorusEdge(vec3 p, float smallRadius, float largeRadius) {
    p.x += largeRadius;
    return fTorus(p, smallRadius, largeRadius);
}

// curve the x axis around the y axis
void pCurve(inout vec3 p, float r) {
    p.z -= r;
    r = abs(r);
    p = vec3(atan(p.x, -p.z) * r, p.y, length(p.xz) - r);
}

float fZygomatic(vec3 p) {
    vec3 pp = p;
    p = pRy(pRx(pRz(p - vec3(.35,.215,-.26), .3), .12), -.2);
    float largeRadius = .35;
    p.x += largeRadius;
    vec3 c = vec3(atan(p.z, p.x), length(p.xz), p.y);
    float top = -.02;
    float bottom = .02;
    top = mix(top, -.04, smoothstep(.4, -.27, c.x));
    top = mix(top, -.1, smoothstep(-.12, -.27, c.x));
    top = mix(top, -.17, smoothstep(-.1, -.45, c.x));
    bottom = mix(bottom, .12, smoothstep(.3, -.33, c.x));
    bottom = mix(bottom, .04, smoothstep(-.3, -.7, c.x));
    float width = .01;
    width = mix(width, .03, smoothstep(.2, -.6, c.x));
    vec2 smallRadius = vec2(width, distance(top, bottom) / 2.);
    c.y -= largeRadius;
    c.z -= top + smallRadius.y;
    float rot = smoothstep(.3, -.27, c.x);
    pR(c.yz, rot * -.3);
    float d = sdEllipse(c.yz, smallRadius);
    p = pp;
    p = pRy(p - vec3(.2,.125,-.45), 1.);
    float cut = length(p.xy) - .11;
    p = pp;
    p = pRy(pRz(p - vec3(.35,0,0), .4), -.1);
    cut = min(cut, p.x);
    d = smax(d, -cut, .04);
    return d;
}

float fMaxilla(vec3 p) {
    vec3 pp = p;
    p = pRx(p - vec3(0,.42,-.29), .4);
    float gum = sdEllipsoidXXZ(p, vec2(.2, .27));
    float gumback = pRy(p, .55).z - .01;
    gumback = smin(gumback, pRy(p, -.55).z - .08, .04);
    gum = smax(gum, gumback, .08);
    p = pRx(p, .02);
    pCurve(p.zxy, -.8);
    gum = smax(gum, p.y, .02);
    p = pp;
    p = pRx(p - vec3(0,.33,-.32), .3);
    float maxilla = length(p.xz) - .18;
    maxilla = smax(maxilla, -(length((p - vec3(0,0,.1)).xz) - .1), .1);
    maxilla = smax(maxilla, p.y - .02, .1);
    maxilla = smax(maxilla, -p.y - .2, .1);
    gum = smin(gum, maxilla, .07);
    p = pp;
    float roof = sdEllipsoidXXZ(p - vec3(0,.47,-.28), vec2(.13, .22));
    gum = smax(gum, -roof, .03);
    float d = gum;

    p -= vec3(.2,.28,-.39);
    float t = dot(p, normalize(vec3(7,3,6))) - .06;
    t = smin(t, dot(p, normalize(vec3(-1,1,15))) - .03, .02);
    t = smax2(t, -dot(p, normalize(vec3(-3,-6,10))) - .055, .06);
    t = smax2(t, -dot(p, normalize(vec3(-2.5,1.5,.7))) - .06, .03);
    // t = max(t, length(p) - .15);
    d = smin(d, t, .04);
    // d = smin(d, t, .0);
    
    // d = max(d, length(p) - .15);

    return d;
}

float map(vec3 p) {
    p.x = abs(p.x);
    vec3 pp = p;
    float d = 1e12;
    float back = sdEllipsoidXXZ(p - vec3(0,-.11,.16), vec2(.4, .32));
    d = min(d, back);
    float base = length(p - vec3(0,-.08,.25)) - .15;
    d = smin(d, base, .3);
    float baseside = length(p - vec3(.2,.1,.25)) - .05;
    d = smin(d, baseside, .25);
    float forehead = sdEllipsoidXXZ(pRx(p - vec3(0,-.15,-.14), .5), vec2(.35, .44) * .97);
    d = smin(d, forehead, .22);
    float foreheadside = length(p - vec3(.17,-.13,-.3)) - .05;
    d = smin(d, foreheadside, .25);
    float backbump = sdEllipsoidXXZ(pRx(pRy(p - vec3(.27,-.29,.0), -.25), .0), vec2(.1, .5) * .25);
    d = smin(d, backbump, .34);
    float topbump = sdEllipsoidXXZ(p - vec3(0,-.33,-.05), vec2(.1, .15) * .5);
    d = smin(d, topbump, .3);
    float side = sdEllipsoidXXZ(pRz(p - vec3(.25,.05,-.0), .3).yzx, vec2(.1, .05) * .8);
    d = smin(d, side, .25);
    float sphenoid = sdEllipsoidXXZ(pRy(pRz(p - vec3(.1,.2,-.2), .4), -.5).yzx, vec2(.05, .025));
    d = smin(d, sphenoid, .3);
    float sphenoidcut = sdEllipsoidXXZ(pRx(pRz(p - vec3(.4,.1,-.35), .4), .3).xzy, vec2(.005, .25) * .5);
    d = smax(d, -sphenoidcut, .3);
    p = pp;
    float maxilla = fMaxilla(p);
    d = smin(d, maxilla, .1);

    d = maxilla;

    // float zygomatic = fZygomatic(p);
    // d = smin(d, zygomatic, .1);
    // p = pRy(pRz(p - vec3(.18,.105,-.47), .15), .4);
    // pCurve(p, .25);
    // float socket = fTorus(p.yzx, .02, .1);
    // float thic = .02;
    // socket = sdUberprim(p, vec4(.155,.12,thic,0), vec3(.12,thic,thic));
    // d = smin(d, socket, .05);

    // p -= vec3(.15,.1,-.5);
    // pCurve(p, .1);
    // d = fTorus((p).yzx, .02, .1);
    // d = length(p.yz) - .1;
    // d = max(d, -p.x);

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
