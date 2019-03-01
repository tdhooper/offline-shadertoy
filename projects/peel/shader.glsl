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

uniform bool guiBlend;
uniform bool guiSplit;
uniform bool guiNeck;
uniform bool guiDebug;
uniform bool guiStep0;
uniform bool guiStep1;

/* SHADERTOY FROM HERE */


const float EDGE_THICKNESS = .2;
const float WIDTH = 1.;
const float RADIUS = 3.;
const float CHANNEL_DEPTH_RATIO = 1.;
const float BALL_COUNT = 19.;
const float BALL_SIZE_RATIO = 1.;
const float BALL_SPEED = -5.;
const float TWISTS = .5;
const float TWIST_SPEED = 1.;


#define PI 3.14159265359

void pR(inout vec2 p, float a) {
    p = cos(a)*p + sin(a)*vec2(p.y, -p.x);
}

float hash(const in vec3 p) {
    return fract(sin(dot(p,vec3(127.1,311.7,758.5453123)))*43758.5453123);
}


// --------------------------------------------------------
// IQ
// https://www.shadertoy.com/view/ll2GD3
// --------------------------------------------------------

vec3 pal( in float t, in vec3 a, in vec3 b, in vec3 c, in vec3 d ) {
    return a + b*cos( 6.28318*(c*t+d) );
}

vec3 spectrum(float n) {
    return pal( n, vec3(0.5,0.5,0.5),vec3(0.5,0.5,0.5),vec3(1.0,1.0,1.0),vec3(0.0,0.33,0.67) );
}


/*

    Geodesic tiling, with absolute positions
    ----------------------------------------

    Similar to https://www.shadertoy.com/view/llGXWc
    except with the full absolute position of each
    point.

*/


// --------------------------------------------------------
// Icosahedron faces and vertices
// --------------------------------------------------------

#define PHI (1.618033988749895)

// Return a or b, depending if p is in front of,
// or behind the plane normal
vec3 splitPlane(vec3 a, vec3 b, vec3 p, vec3 plane) {
    float split = max(sign(dot(p, plane)), 0.);
    return mix(a, b, split);
}

// An icosahedron vertex for the nearest face,
// a bit like finding the nearest icosahedron vertex,
// except we only need one per face
vec3 icosahedronVertex(vec3 p) {
    vec3 sp, v1, v2, result, plane;
    sp = sign(p);
    v1 = vec3(PHI, 1, 0) * sp;
    v2 = vec3(1, 0, PHI) * sp;
    plane = vec3(1, PHI, -PHI - 1.) * sp;
    result = splitPlane(v2, v1, p, plane);
    return normalize(result);
}

// Nearest dodecahedron vertex (nearest icosahrdron face)
vec3 dodecahedronVertex(vec3 p) {
    vec3 sp, v1, v2, v3, v4, result, plane;
    sp = sign(p);
    v1 = sp;
    v2 = vec3(0, 1, PHI + 1.) * sp;
    v3 = vec3(1, PHI + 1., 0) * sp;
    v4 = vec3(PHI + 1., 0, 1) * sp;
    plane = vec3(-1. - PHI, -1, PHI);
    result = splitPlane(v1, v2, p, plane * sp);
    result = splitPlane(result, v3, p, plane.yzx * sp);
    result = splitPlane(result, v4, p, plane.zxy * sp);
    return normalize(result);
}


// --------------------------------------------------------
// Triangle tiling
// Adapted from mattz https://www.shadertoy.com/view/4d2GzV
//
// Finds the closest triangle center on a 2D plane 
// --------------------------------------------------------

const float sqrt3 = 1.7320508075688772;
const float i3 = 0.5773502691896258;

const mat2 cart2hex = mat2(1, 0, i3, 2. * i3);
const mat2 hex2cart = mat2(1, 0, -.5, .5 * sqrt3);

struct TriPoints {
    vec2 a;
    vec2 b;
    vec2 c;
    vec2 center;
    vec2 hexCenter;
    vec2 ab;
    vec2 bc;
    vec2 ca;
};

TriPoints closestTriPoints(vec2 p) {

    float rot = PI / 2.;
    pR(p, rot);

    vec2 pTri = cart2hex * p;
    vec2 pi = floor(pTri);
    vec2 pf = fract(pTri);
    
    float split1 = step(pf.y, pf.x);
    float split2 = step(pf.x, pf.y);
    
    vec2 a = vec2(split1, 1);
    vec2 b = vec2(1, split2);
    vec2 c = vec2(0, 0);

    a += pi;
    b += pi;
    c += pi;

    a = hex2cart * a;
    b = hex2cart * b;
    c = hex2cart * c;
    
    vec2 center = (a + b + c) / 3.;
    
    vec2 ab0 = (a + b) / 2.;
    vec2 bc0 = (b + c) / 2.;
    vec2 ca0 = (c + a) / 2.;

    vec2 ab, bc, ca;

    vec2 hexCenter = a;
    ab = ab0;
    bc = ca0;
    ca = bc0;
    if (distance(p, b) < distance(p, hexCenter)) {
        hexCenter = b;
        ab = bc0;
        bc = ab0;
        ca = ca0;
    }
    if (distance(p, c) < distance(p, hexCenter)) {
        hexCenter = c;
        ab = ca0;
        bc = bc0;
        ca = ab0;
    }

    pR(a, -rot);
    pR(b, -rot);
    pR(c, -rot);

    pR(center, -rot);
    pR(hexCenter, -rot);

    pR(ab, -rot);
    pR(bc, -rot);
    pR(ca, -rot);

    return TriPoints(a, b, c, center, hexCenter, ab, bc, ca);
}


// --------------------------------------------------------
// Geodesic tiling
//
// Finds the closest triangle center on the surface of a
// sphere:
// 
// 1. Intersect position with the face plane
// 2. Convert that into 2D uv coordinates
// 3. Find the closest triangle center (tile the plane)
// 4. Convert back into 3D coordinates
// 5. Project onto a unit sphere (normalize)
//
// You can use any tiling method, such as one that returns
// hex centers or adjacent cells, so you can create more
// interesting geometry later.
// --------------------------------------------------------

struct TriPoints3D {
    vec3 a;
    vec3 b;
    vec3 c;
    vec3 center;
    vec3 hexCenter;
    vec3 ab;
    vec3 bc;
    vec3 ca;
    float id;
};


vec3 facePlane = vec3(0);
vec3 uPlane = vec3(0);
vec3 vPlane = vec3(0);

// Intersection point of vector and plane
vec3 intersection(vec3 n, vec3 planeNormal, float planeOffset) {
    float denominator = dot(planeNormal, n);
    float t = (dot(vec3(0), planeNormal) + planeOffset) / -denominator;
    return n * t;
}

// 3D position -> 2D (uv) coordinates on the icosahedron face
vec2 icosahedronFaceCoordinates(vec3 p) {
    vec3 i = intersection(normalize(p), facePlane, -1.);
    return vec2(dot(i, uPlane), dot(i, vPlane));
}

// 2D (uv) coordinates -> 3D point on a unit sphere
vec3 faceToSphere(vec2 facePoint) {
    return normalize(facePlane + (uPlane * facePoint.x) + (vPlane * facePoint.y));
}

// Edge length of an icosahedron with an inscribed sphere of radius of 1
// const float edgeLength = 1. / ((sqrt(3.) / 12.) * (3. + sqrt(5.)));
// Inner radius of the icosahedron's face
// const float faceRadius = (1./6.) * sqrt(3.) * edgeLength;
// float faceRadius = 1./3.;
float faceRadius = .2205;

// Closest geodesic point (triangle center) on unit sphere's surface
TriPoints3D geodesicTriPoints(vec3 p, float subdivisions) {
    
    vec3 dv = dodecahedronVertex(p);
    vec3 iv = icosahedronVertex(p);
    
    facePlane = dv;
    vPlane = normalize(cross(iv, dv));
    uPlane = normalize(cross(vPlane, dv));

    vec2 uv = icosahedronFaceCoordinates(p);
    
    // faceRadius is used as a scale multiplier so that our triangles
    // always stop at the edge of the face
    float uvScale = subdivisions / faceRadius / 2.;

    // Get points on the nearest triangle tile
    TriPoints points = closestTriPoints(uv * uvScale);

    // Project 2D triangle coordinates onto a sphere 
    vec3 a = faceToSphere(points.a / uvScale);
    vec3 b = faceToSphere(points.b / uvScale);
    vec3 c = faceToSphere(points.c / uvScale);
    vec3 center = faceToSphere(points.center / uvScale);
    vec3 hexCenter = faceToSphere(points.hexCenter / uvScale);
    vec3 ab = faceToSphere(points.ab / uvScale);
    vec3 bc = faceToSphere(points.bc / uvScale);
    vec3 ca = faceToSphere(points.ca / uvScale);

    float id = hash(vec3(int(hexCenter * 1000.)) / 1000.);

    return TriPoints3D(a, b, c, center, hexCenter, ab, bc, ca, id);
}



// --------------------------------------------------------
// Modelling utilities
// hg_sdf https://www.shadertoy.com/view/Xs3GRB
// --------------------------------------------------------

#define saturate(x) clamp(x, 0., 1.)


float vmax(vec2 v) {
    return max(v.x, v.y);
}

float vmax(vec3 v) {
    return max(max(v.x, v.y), v.z);
}

float vmin(vec3 v) {
    return min(min(v.x, v.y), v.z);
}

float fBox2(vec2 p, vec2 b) {
    vec2 d = abs(p) - b;
    return length(max(d, vec2(0))) + vmax(min(d, vec2(0)));
}

float fBox(vec3 p, vec3 b) {
    vec3 d = abs(p) - b;
    return length(max(d, vec3(0))) + vmax(min(d, vec3(0)));
}

float fDisc(vec3 p, float r) {
    float l = length(p.xz) - r;
    return l < 0. ? abs(p.y) : length(vec2(p.y, l));
}

// Capsule: A Cylinder with round caps on both sides
float fCapsule(vec3 p, float r, float c) {
    return mix(length(p.xz) - r, length(vec3(p.x, abs(p.y) - c, p.z)) - r, step(c, abs(p.y)));
}

float fHalfCapsule(vec3 p, float r) {
    return mix(length(p.xz) - r, length(p) - r, step(0., p.y));
}

float fHalfCapsule(vec2 p, float r) {
    return mix(length(p.x) - r, length(p) - r, step(0., p.y));
}

// Distance to line segment between <a> and <b>, used for fCapsule() version 2below
float fLineSegment(vec3 p, vec3 a, vec3 b) {
    vec3 ab = b - a;
    float t = saturate(dot(p - a, ab) / dot(ab, ab));
    return length((ab*t + a) - p);
}

// Capsule version 2: between two end points <a> and <b> with radius r 
float fCapsule(vec3 p, vec3 a, vec3 b, float r) {
    return fLineSegment(p, a, b) - r;
}

// IQ https://www.shadertoy.com/view/Xds3zN
float sdRoundCone( in vec3 p, in float r1, float r2, float h )
{
    vec2 q = vec2( length(p.xz), p.y );
    
    float b = (r1-r2)/h;
    float a = sqrt(1.0-b*b);
    float k = dot(q,vec2(-b,a));
    
    if( k < 0.0 ) return length(q) - r1;
    if( k > a*h ) return length(q-vec2(0.0,h)) - r2;
        
    return dot(q, vec2(a,b) ) - r1;
}

// float smin(float a, float b, float r) {
//     vec2 u = max(vec2(r - a,r - b), vec2(0));
//     return max(r, min (a, b)) - length(u);
// }

// float smax(float a, float b, float r) {
//     vec2 u = max(vec2(r + a,r + b), vec2(0));
//     return min(-r, max (a, b)) + length(u);
// }

float smin(float a, float b, float k){
    float f = clamp(0.5 + 0.5 * ((a - b) / k), 0., 1.);
    return (1. - f) * a + f  * b - f * (1. - f) * k;
}

float smax(float a, float b, float k) {
    return -smin(-a, -b, k);
}

float smin(float a, float b) {
    return smin(a, b, .0);
}

float smax(float a, float b) {
    return smax(a, b, 0.);
}


vec3 cartToPolar(vec3 p) {
    float x = p.x; // distance from the plane it lies on
    float r = length(p.zy); // distance from center
    float a = atan(p.y, p.z); // angle around center
    return vec3(x, r, a);
}

vec3 polarToCart(vec3 p) {
    return vec3(
        p.x,
        sin(p.z) * p.y,
        cos(p.z) * p.y
    );
}

mat3 calcLookAtMatrix(vec3 ro, vec3 ta, vec3 up) {
    vec3 ww = normalize(ta - ro);
    vec3 uu = normalize(cross(ww,up));
    vec3 vv = normalize(cross(uu,ww));
    return mat3(uu, vv, ww);
}


// --------------------------------------------------------
// Model
// --------------------------------------------------------

struct Model {
    float dist;
    vec3 material;
};

Model mapA(vec3 p) {
    float d;
    float c = length(p.xy) - .5;

    float lead = 20.;
    float strands = 2.;

    d = abs(
        sin((atan(p.y,p.x)-p.z * lead) / strands)
        * min(1., length(p.xy))
    ) / (lead / strands) - .02;
    d = max(d, c);

    return Model(d, vec3(.8));
}

float helix(vec3 p, float lead, float thick) {
    p.z += iTime * .1;
    float d = (mod(atan(p.y, p.x) - p.z * lead, PI * 2.) - PI) / lead;
    d = abs(d) - thick;
    return d;
}

float ellip(vec3 p, vec3 s) {
    float r = vmin(s);
    p *= r / s;
    return length(p) - r;
}

vec3 modelAlbedo;


float mHead(vec3 p, bool bounded) {

    modelAlbedo = vec3(.9);

    // return fBox(p, vec3(.4));

    // return length(p) - .5;

    pR(p.yz, -.1);

    float bound = length(p - vec3(0,.03,0)) - .53;
    bound = smin(bound, length(p - vec3(0,-.45,.28)) - .25, .3);
    bound = smin(bound, length(p - vec3(0,-.25,.5)) - .1, .1);
    bound = smax(bound, abs(p.x) - .4, .2);

    return bound + .05;

    if (bounded && bound > .01) {
        return bound;
    }

    vec3 pa = p;
    p.x = abs(p.x);
    vec3 pp = p;


    float d = 1e12;

    // skull back
    p += vec3(0,-.135,.09);
    d = ellip(p, vec3(.395, .385, .395));

    // skull base
    p = pp;
    p += vec3(0,-.135,.09) + vec3(0,.1,.07);
    d = smin(d, ellip(p, vec3(.38, .36, .35)), .05);

    // forehead
    p = pp;
    p += vec3(0,-.145,-.175);
    d = smin(d, ellip(p, vec3(.315, .3, .33)), .18);

    p = pp;
    pR(p.yz, -.5);
    float bb = fBox(p, vec3(.5,.67,.7));
    d = smax(d, bb, .2);

    // face base
    p = pp;
    p += vec3(0,.25,-.13);
    d = smin(d, length(p) - .28, .1);

    // behind ear
    p = pp;
    p += vec3(-.15,.13,.06);
    d = smin(d, ellip(p, vec3(.15,.15,.15)), .15);

    // cheek base
    p = pp;
    p += vec3(-.2,.14,-.14);
    d = smin(d, ellip(p, vec3(.15,.22,.2) * .8), .15);

    // jaw base
    p = pp;
    p += vec3(0,.475,-.16);
    pR(p.yz, .8);
    d = smin(d, ellip(p, vec3(.19,.1,.2)), .1);

    // return d;

    // brow
    p = pp;
    p += vec3(0,-.0,-.18);
    vec3 bp = p;
    float brow = fHalfCapsule(p * vec3(.65,1,.9), .27);
    brow = length(p) - .36;
    p.x -= .37;
    brow = smax(brow, dot(p, normalize(vec3(1,.2,-.2))), .2);
    p = bp;
    brow = smax(brow, dot(p, normalize(vec3(0,.6,1))) - .43, .25);
    p = bp;
    pR(p.yz, -.5);
    float peak = -p.y - .165;
    peak += smoothstep(.0, .2, p.x) * .01;
    peak -= smoothstep(.12, .29, p.x) * .025;
    // peak += smoothstep(.2, .4, p.x) * .025;
    brow = smax(brow, peak, .07);
    p = bp;
    pR(p.yz, .5);
    brow = smax(brow, -p.y - .06, .15);
    d = smin(d, brow, .06);

    // return d;

    // return brow;

    // if (guiNeck) {
    //     p = pa;
    //     p += vec3(.18,.57,-.1);
    //     float nb = length(p);
    //     nb = mix(.11, .17, smoothstep(.1, .3, nb));
    //     d = smin(d, neck, nb);
    // }

    // return d;

    // nose
    p = pp;
    p += vec3(0,.03,-.45);
    pR(p.yz, 3.);
    d = smin(d, sdRoundCone(p, .008, .05, .18), .1);

    p = pp;
    p += vec3(0,.06,-.47);
    pR(p.yz, 2.77);
    d = smin(d, sdRoundCone(p, .005, .04, .225), .05);

    // jaw

    p = pp;
    vec3 jo = vec3(-.25,.4,-.07);
    p = pp + jo;
    float jaw = dot(p, normalize(vec3(1,-.2,-.05))) - .069;
    jaw = smax(jaw, dot(p, normalize(vec3(.5,-.25,.35))) - .13, .12);
    jaw = smax(jaw, dot(p, normalize(vec3(-.0,-1.,-.8))) - .12, .15);
    jaw = smax(jaw, dot(p, normalize(vec3(.98,-1.,.15))) - .13, .08);
    jaw = smax(jaw, dot(p, normalize(vec3(.6,-.2,-.45))) - .19, .15);
    jaw = smax(jaw, dot(p, normalize(vec3(.5,.1,-.5))) - .26, .15);
    jaw = smax(jaw, dot(p, normalize(vec3(1,.2,-.3))) - .22, .15);

    p = pp;
    p += vec3(0,.63,-.2);
    pR(p.yz, .15);
    float cr = .5;
    jaw = smax(jaw, length(p.xy - vec2(0,cr)) - cr, .05);

    p = pp + jo;
    jaw = smax(jaw, dot(p, normalize(vec3(0,-.4,1))) - .35, .1);
    jaw = smax(jaw, dot(p, normalize(vec3(0,1.5,2))) - .3, .2);
    jaw = max(jaw, length(pp + vec3(0,.6,-.3)) - .7);

    p = pa;
    p += vec3(.2,.5,-.1);
    float jb = length(p);
    jb = smoothstep(.0, .4, jb);
    float js = mix(0., -.005, jb);
    jb = mix(.01, .04, jb);

    d = smin(d, jaw - js, jb);

    // return d;

    // chin
    p = pp;
    p += vec3(0,.585,-.395);
    p.x *= .7;
    d = smin(d, ellip(p, vec3(.028,.028,.028)*1.2), .15);

    // cheek

    p = pp;
    p += vec3(-.2,.2,-.28);
    pR(p.xz, .5);
    pR(p.yz, .4);
    float ch = ellip(p, vec3(.1,.1,.12)*1.05);
    d = smin(d, ch, .1);

    p = pp;
    p += vec3(-.26,.02,-.1);
    pR(p.xz, .13);
    pR(p.yz, .5);
    float temple = ellip(p, vec3(.1,.1,.15));
    temple = smax(temple, p.x - .07, .1);
    d = smin(d, temple, .1);

    p = pp;
    p += vec3(.0,.2,-.32);
    ch = ellip(p, vec3(.1,.08,.1));
    d = smin(d, ch, .1);

    p = pp;
    p += vec3(-.17,.31,-.17);
    ch = ellip(p, vec3(.1));
    d = smin(d, ch, .1);
    // return ch;
    
    // return temple;
    // return d;

    // cheek

    p = pp;
    p += vec3(-.13,.2,-.26);
    // d = smin(d, ellip(p, vec3(.13,.1,.1)), .15);

    // return d;



    // mouth base
    p = pp;
    p += vec3(-.0,.29,-.29);
    pR(p.yz, -.3);
    d = smin(d, ellip(p, vec3(.13,.15,.1)), .18);

    p = pp;
    p += vec3(0,.37,-.4);
    d = smin(d, ellip(p, vec3(.03,.03,.02) * .5), .1);

    p = pp;
    p += vec3(-.09,.37,-.31);
    d = smin(d, ellip(p, vec3(.04)), .18);

    // bottom lip
    p = pp;
    p += vec3(0,.455,-.455);
    p.z += smoothstep(.0, .2, p.x) * .05;
    float lb = mix(.035, .03, smoothstep(.05, .15, length(p)));
    float bottomlip = ellip(p, vec3(.055,.028,.022) * 1.25);
    d = smin(d, bottomlip, lb);

    // top lip
    p = pp;
    p += vec3(0,.38,-.45);
    pR(p.xz, -.3);
    float toplip = ellip(p, vec3(.065,.03,.05));
    p = pp;
    p += vec3(0,.33,-.45);
    pR(p.yz, .7);
    float cut = p.y - smoothstep(0., .03, p.x) * .015;
    cut += smoothstep(.03, .18, p.x) * .2;
    toplip = smax(toplip, cut, .03);
    d = smin(d, toplip, .07);

    // seam
    p = pp;
    p += vec3(0,.425,-.44);
    lb = length(p);
    float lr = mix(.04, .02, smoothstep(.05, .12, lb));
    pR(p.yz, .1);
    p.y -= smoothstep(0., .03, p.x) * .002;
    p.y += smoothstep(.03, .1, p.x) * .007;
    // TODO: use a circle instead to help with corner blending
    p.z -= .133;
    float seam = fDisc(p, .2);
    d = mix(d, smax(d, -seam, lr), .65);
    // d = min(d, seam);
    // return d;


    // nostrils base
    p = pp;
    p += vec3(0,.3,-.43);
    d = smin(d, length(p) - .05, .07);

    // nostrils
    p = pp;
    p += vec3(0,.27,-.52);
    pR(p.yz, .2);
    float nostrils = ellip(p, vec3(.055,.05,.06));

    p = pp;
    p += vec3(-.043,.28,-.48);
    pR(p.xy, .15);
    p.z *= .8;
    nostrils = smin(nostrils, sdRoundCone(p, .042, .0, .12), .02);

    d = smin(d, nostrils, .02);

    p = pp;
    p += vec3(-.033,.3,-.515);
    pR(p.xz, .5);
    d = smax(d, -ellip(p, vec3(.011,.03,.025)), .015);

    // return d;

    // eyelids
    p = pp;
    p += vec3(-.16,.07,-.34);
    // pR(p.xy, -.9);
    float eyelids = ellip(p, vec3(.08,.1,.1));

    p = pp;
    p += vec3(-.16,.09,-.35);
    // pR(p.xy, -.9);
    float eyelids2 = ellip(p, vec3(.09,.1,.07));

    // return eyelids;

    // edge top
    p = pp;
    p += vec3(-.173,.148,-.43);
    p.x *= .97;
    float et = length(p.xy) - .09;

    // edge bottom
    p = pp;
    p += vec3(-.168,.105,-.43);
    p.x *= .9;
    float eb = dot(p, normalize(vec3(-.1,-1,-.2))) + .001;
    eb = smin(eb, dot(p, normalize(vec3(-.3,-1,0))) - .006, .01);
    eb = smax(eb, dot(p, normalize(vec3(.5,-1,-.5))) - .018, .05);

    // p = pp;
    // p += vec3(-.08,-.004,-.43);
    // pR(p.xz, -.2);
    // p.x *= .9;
    // eb = smin(eb, length(p.xy) - .1, .01);

    float edge = max(max(eb, et), -d);

    d = smin(d, eyelids, .01);
    d = smin(d, eyelids2, .03);
    d = smax(d, -edge, .005);

    // eyeball
    p = pp;
    p += vec3(-.165,.0715,-.346);
    d = min(d, length(p) - .088);

    // tear duct
    p = pp;
    p += vec3(-.075,.1,-.37);
    d = min(d, length(p) - .05);

    p = pp;

    // d = min(d, bound);

    return d;

    // p = pa;
    // d += length(sin(p * 60. + vec3(0,iTime*3.,0).zxy)) * .005 - .005;

    // d += sin(cc * 100. - iTime) * smoothstep(.5, .0, cc) * .01;

    if (d < -.01) {
        modelAlbedo = vec3(.2,.25,.3);
        modelAlbedo = mix(modelAlbedo, vec3(.7,.8,.9), .5);
        modelAlbedo = vec3(0,1,0);
    }

    p = pa;
    float h = helix(p.xzy, 35., .06);

    d = abs(d + .01) - .01;
    
    d = max(d, h);

    return d;
}

vec3 projectSurface(vec3 dir, vec3 origin) {
    vec3 ray = dir;
    float dist = 0.;
    const int STEPS = 5;
    for(int i = 0; i < STEPS; i++ ) {
        dist = mHead(ray - origin, true);
        if (dist < .001) {
            break;
        }
        ray += dist * -dir;
    }
    return ray - origin;
}

vec3 projectSurface(vec3 dir) {
    return projectSurface(dir, vec3(0));
}

vec3 _projectSurface(vec3 dir, vec3 origin) {
    vec3 ray = dir;
    float dist = 0.;
    dist = mHead(ray - origin, true); ray += dist * -dir;
    dist = mHead(ray - origin, true); ray += dist * -dir;
    dist = mHead(ray - origin, true); ray += dist * -dir;
    dist = mHead(ray - origin, true); ray += dist * -dir;
    dist = mHead(ray - origin, true); ray += dist * -dir;
    return ray - origin;
}

float _map(vec3 p) {

    vec3 origin = vec3(cos(iTime * 2.) * .25,sin(iTime * 2.) * .25 + .1,0);
    origin = vec3(0,.1,0);

    TriPoints3D points = geodesicTriPoints(p + origin, 3.);

    vec3 psA = projectSurface(points.a, origin);
    vec3 psB = projectSurface(points.b, origin);
    vec3 psC = projectSurface(points.c, origin);

    float sz = .02;
    float dotsA = length(p - psA) - sz;
    float dotsB = length(p - psB) - sz;
    float dotsC = length(p - psC) - sz;
    float dots = min(dotsA, min(dotsB, dotsC));

    float head = mHead(p, true);

    return smin(head, dots, .015);

    // head = abs(head + .01) - .01;

    // float headd = smin(head, dots, .1 / s);


    // return mix(head, mix(headd, dots, smoothstep(.33, 1., iTime)), smoothstep(0., .33, iTime));

    // return smin(head, dots, .1 / s);

    // return mHead(p) - iTime;
}

float range(float vmin, float vmax, float value) {
  return clamp((value - vmin) / (vmax - vmin), 0., 1.);
}

float sinstep(float a) {
    return sin(a * PI - PI * .5) * .5 + .5;
}

const float shell = .08;
const float stepScale = .15;
const float plodeDuration = 1.;
const float plodeOverlap = .35;
const float blendDuration = .5;
const float plodeDistance = .75;

float mEdge(vec3 p, TriPoints3D points) {
    vec3 edgeAB = normalize(cross(points.center, points.ab));
    vec3 edgeBC = normalize(cross(points.center, points.bc));
    float edge = min(dot(p, edgeAB), -dot(p, edgeBC));
    return edge;
}

float mHeadShell(vec3 p) {
    float d = mHead(p, true);
    d = abs(d + shell) - shell;
    return d;
}

float time;

float animPlode(float id, float startOffset) {
    id = 0.;
    float delay = id * .2;
    float start = delay;
    float end = plodeDuration;
    float plode = range(start, end, time - startOffset);
    plode = sinstep(sinstep(plode));
    plode *= (1. - delay);
    return plode;
}

bool animPlodeStarted(float startOffset) {
    return time > startOffset;
}

float animBlend(float startOffset) {
    float start = 0.;
    float end = start + blendDuration;
    float blend = range(start, end, time - startOffset);
    return blend;
}

vec3 wayTrans0;
vec3 wayTrans1;
float wayScale0;
float wayScale1;
mat3 wayRot0;
mat3 wayRot1;

void calcWaypoints() {

    float animTime = range(.0, (plodeDuration - plodeOverlap) - .0, time);
    float ar = (pow(stepScale, animTime) - 1.) / (stepScale - 1.);
    // animTime = sinstep(sinstep(animTime));
    // animTime = pow(animTime, 2.);
    // float focusScale = 1. + (animTime / stepScale) * (1. - stepScale);
    float focusScale = 1. / pow(stepScale, animTime);
    // float focusScale = mix(1., 1./stepScale, at);

    wayScale0 = 1.;
    wayScale1 = 1. / stepScale;

    TriPoints3D points, focusPoints;
    vec3 focusHexCenter;
    vec3 focusP, focusP2, focusP3;
    float sectionEdge0, sectionEdge1;
    float plodeEdge0;
    float d, d2;

    focusHexCenter = normalize(vec3(0, 1, PHI + 1.));
    focusPoints = geodesicTriPoints(focusHexCenter, 1.);
    focusP = projectSurface(focusPoints.hexCenter) - focusPoints.hexCenter * shell;
    focusP += focusPoints.hexCenter * plodeDistance;// * animPlode(focusPoints.id, plodeOverlap - plodeDuration);

    wayTrans0 = focusP;

    focusHexCenter += focusP; // or minus?
    focusPoints = geodesicTriPoints(focusHexCenter, 1.);
    // focusPoints.hexCenter = calcLookAtMatrix(vec3(0), focusPoints.hexCenter, vec3(0,1,0)) * focusPoints.hexCenter;
    focusP2 = projectSurface(focusPoints.hexCenter) - focusPoints.hexCenter * shell;
    focusP2 += focusPoints.hexCenter * plodeDistance;// * animPlode(focusPoints.id, 0.);
    focusP2 *= stepScale;

    wayTrans1 = focusP + focusP2;

}

float map(vec3 p) {

    float animTime = range(.0, (plodeDuration - plodeOverlap) - .0, time);
    float ar = (pow(stepScale, animTime) - 1.) / (stepScale - 1.);
    float aj = (pow(1./stepScale, animTime) - 1.) / (1./stepScale - 1.);
    float focusScale = mix(wayScale0, wayScale1, aj);
    p /= focusScale;

    TriPoints3D points;
    float sectionEdge0, sectionEdge1;
    float plodeEdge0;
    float d, d2;

    p += mix(wayTrans0, wayTrans1, ar);

    // p = calcLookAtMatrix(vec3(0), mix(vec3(0,0,1), focusPoints.hexCenter, vec3(ar)), vec3(0,1,0)) * p;

    float focusDebug = length(p - wayTrans0) - .07 / wayScale0;
    focusDebug = min(focusDebug, length(p - wayTrans1) - .07 / wayScale1);

    modelAlbedo = vec3(.9);

    points = geodesicTriPoints(p, 1.);
    sectionEdge0 = mEdge(p, points);

    p -= points.hexCenter * animPlode(points.id, plodeOverlap - plodeDuration) * plodeDistance;

    if (guiStep0) {
        p -= projectSurface(points.hexCenter) - points.hexCenter * shell;
    }

    if ( ! guiStep0) {
        plodeEdge0 = mEdge(p, points);
        d = mHeadShell(p);
        d = max(d, -plodeEdge0);
        d = min(d, sectionEdge1+.02);
        return min(d, focusDebug);
        return d;
    }

    float idA = points.id;

    p /= stepScale;

    // p *= calcLookAtMatrix(vec3(0), points.hexCenter, vec3(0,1,0));

    points = geodesicTriPoints(p, 1.);
    sectionEdge1 = mEdge(p, points) * stepScale;

    if (animPlodeStarted(0.)) {
        p -= points.hexCenter * animPlode(points.id, 0.) * plodeDistance;
    }

    d = mHeadShell(p) * stepScale;

    if (animPlodeStarted(0.)) {
        plodeEdge0 = mEdge(p, points) * stepScale;
        d = max(d, -plodeEdge0);
        sectionEdge1 = max(sectionEdge1, d - .2 * stepScale);
        d = min(d, sectionEdge1 + .02 * stepScale);

        p -= projectSurface(points.hexCenter) - points.hexCenter * shell;

        if (guiStep1) {
            p /= stepScale;
            // p *= calcLookAtMatrix(vec3(0), points.hexCenter, vec3(0,1,0));
            d2 = mHead(p, false) * stepScale * stepScale;
            d2 = min(d2, sectionEdge1 + .02 * stepScale);
            d = mix(d, d2, animBlend(plodeDuration - plodeOverlap - blendDuration));
        }
    }

    d = min(d, sectionEdge0 + .02);
    // return focusDebug;
    return min(d * focusScale, focusDebug);

    return d * focusScale;
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
};

float calcAO( in vec3 pos, in vec3 nor )
{
    float occ = 0.0;
    float sca = 1.0;
    for( int i=0; i<5; i++ )
    {
        float hr = 0.01 + 0.12*float(i)/4.0;
        vec3 aopos =  nor * hr + pos;
        float dd = map( aopos );
        occ += -(dd-hr)*sca;
        sca *= 0.95;
    }
    return clamp( 1.0 - 3.0*occ, 0.0, 1.0 );
}

vec3 render(Hit hit, vec3 col) {
    if ( ! hit.isBackground) {
        // The simple ambient occlusion method results in hot spots
        // at the base and sides of the balls. This is a result of
        // the limited samples we do across the normal. In reality
        // there would be a more evenly distributed darkness along
        // the base of the channell; so here it's faked with the uv
        // coordinates and blended in.
        float ao = calcAO(hit.pos, hit.normal);
        float light = dot(normalize(vec3(1,1,0)), hit.normal) * .5 + .5;
        float diff = light * ao;
        vec3 diffuse = mix(vec3(.5,.5,.6) * .7, vec3(1), diff);
        col = hit.model.material * diffuse;
        // col = hit.normal * .5 + .5;

        vec3 lig = vec3(0,1.5,.5);
        // lig = vec3(0,.5,1.5);
        lig = vec3(0,1,0);
        col = modelAlbedo * pow(clamp(dot(lig, hit.normal) * .5 + .5, 0., 1.), 1./2.2);
        // col = vec3(1,0,0);

    }
    return col;
}


// --------------------------------------------------------
// Ray Marching
// Adapted from: https://www.shadertoy.com/view/Xl2XWt
// --------------------------------------------------------

const float MAX_TRACE_DISTANCE = 4.;
const float INTERSECTION_PRECISION = .0001;
const int NUM_OF_TRACE_STEPS = 250;

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

Hit raymarch(vec3 rayOrigin, vec3 rayDirection){

    float currentDist = INTERSECTION_PRECISION * 2.0;
    float rayLength = 0.;
    bool isBackground = true;

    for(int i = 0; i < NUM_OF_TRACE_STEPS; i++){
        if (currentDist < INTERSECTION_PRECISION) {
            isBackground = false;
            break;
        }
        if (rayLength > MAX_TRACE_DISTANCE) {
            break;
        }
        currentDist = map(rayOrigin + rayDirection * rayLength);
        rayLength += currentDist;
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
        rayDirection
    );
}

float getDepth(float depth) {
    depth = projection[3].z / (depth * -2. + 1. - projection[2].z);
    return depth;
}

void main() {

    calcWaypoints();

    time = iTime / 3.;
    // time *= .333;
    time = mod(time, plodeDuration - plodeOverlap);


    vec3 rayOrigin = eye;
    vec3 rayDirection = normalize(dir);
    vec3 rayPosition = rayOrigin;

    vec3 bg = vec3(.7,.8,.9) * 1.1;
    bg *= .8;

    Hit hit = raymarch(rayOrigin, rayDirection);
    vec3 color = render(hit, bg);

    // color = vec3(0,0,1);
    // color = pow(color, vec3(1. / 2.2)); // Gamma

    float eyeHitZ = -hit.rayLength * dot(rayDirection, cameraForward);

    vec3 eyeSpace = vec3(0, 0, eyeHitZ);
    float zc = ( projection * vec4(eyeSpace, 1)).z;
    float wc = ( projection * vec4(eyeSpace, 1)).w;
    float depth = (zc/wc + 1.) / 2.;


    float polyD = getDepth(texture2D(uDepth, gl_FragCoord.xy / iResolution.xy).r);
    float rayD = getDepth(depth);

    if (guiBlend && ! hit.isBackground && ! guiSplit) {
        color = spectrum(smoothstep(.01, -.01, polyD - rayD));
    }

    float alpha = smoothstep(.06, -.06, polyD - rayD);

    // alpha = .5;

    if (polyD > rayD) {
        alpha = max(0., alpha - .1);
    }

    // alpha = .5;

    if ( ! guiBlend) {
        alpha = 1.;
    }

    if (guiSplit) {
        alpha = hit.pos.x < 0. ? 0. : 1.;
        alpha = 0.;
    }

    vec3 polyColor = texture2D(uSource, gl_FragCoord.xy / iResolution.xy).rgb;
    color = mix(polyColor, color, alpha);

    if (abs(polyD - rayD) < .001) {
        // color = vec3(1);
    }

    color = mix(color, bg, smoothstep(MAX_TRACE_DISTANCE / 2., MAX_TRACE_DISTANCE, hit.rayLength));



    gl_FragColor = vec4(color, 1);
    gl_FragDepthEXT = depth;
}
