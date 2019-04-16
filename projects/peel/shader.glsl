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

uniform bool guiBlend;
uniform bool guiSplit;
uniform bool guiNeck;
uniform bool guiDebug;
uniform bool guiAnotherLevel;
uniform bool guiEdit;
uniform bool guiStep0;
uniform bool guiStep1;
uniform bool guiStep2;
uniform bool guiStep3;
uniform bool guiFixedCamera;
uniform bool guiLoop;
uniform float guiRand;
uniform bool guiMultiscreen;

uniform float guiPlodeDistance;
uniform float guiCamDistance;
uniform float guiCamLookDown;

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

#pragma glslify: import('./quat.glsl')

void pR(inout vec2 p, float a) {
    p = cos(a)*p + sin(a)*vec2(p.y, -p.x);
}

vec2 pRi(vec2 p, float a) {
    pR(p, a);
    return p;
}

float hash(const in vec3 p) {
    return fract(sin(dot(p,vec3(127.1,311.7,758.5453123)))*43758.5453123);
}

vec3 hash3( vec2 p )
{
    vec3 q = vec3( dot(p,vec2(127.1,311.7)), 
                   dot(p,vec2(269.5,183.3)), 
                   dot(p,vec2(419.2,371.9)) );
    return fract(sin(q)*43758.5453);
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

vec3 icosahedronVertexComplete(vec3 p) {
    vec3 sp, v1, v2, v3, result, plane;
    float split;
    sp = sign(p);
    v1 = vec3(PHI, 1, 0) * sp;
    v2 = vec3(1, 0, PHI) * sp;
    v3 = vec3(0, PHI, 1) * sp;
    plane = cross(cross(v1, v2), v1 + v2);
    split = max(sign(dot(p, plane)), 0.);
    result = mix(v1, v2, split);
    plane = cross(cross(result, v3), v3 + result);
    split = max(sign(dot(p, plane)), 0.);
    result = mix(result, v3, split);
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


float range(float vmin, float vmax, float value) {
  return clamp((value - vmin) / (vmax - vmin), 0., 1.);
}

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

    // float hashId = hash(vec3(int(hexCenter * 1000.)) / 1000.);
    float ee = 100.;
    vec3 h = vec3(
        dot(hexCenter, vec3(1,0,0)),
        dot(hexCenter, vec3(0,1,0)),
        dot(hexCenter, vec3(0,0,1))
    ) * .5 + .5;
    h = floor(h * ee + .5) / ee;
    float seed = guiRand;
    // seed = .6; // .6;
    float hashId = hash(h + seed);

    // id = range(.8, -.8, hexCenter.y);
    float id = range(1., -1., dot(hexCenter, normalize(vec3(.5,1,.5))));
    // float id = range(1., -1., dot(hexCenter, normalize(vec3(.5,1,.25))));
    // float id = range(1., -1., dot(hexCenter, normalize(vec3(0,1,0))));
    id = mix(id, hashId, .4);
    // id = min(id, .2);
    // id = 0.;
    // id = hashId;


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

float vmin(vec2 v) {
    return min(v.x, v.y);
}

float fBox2(vec2 p, vec2 b) {
    vec2 d = abs(p) - b;
    return length(max(d, vec2(0))) + vmax(min(d, vec2(0)));
}

float fBox(vec3 p, vec3 b) {
    vec3 d = abs(p) - b;
    return length(max(d, vec3(0))) + vmax(min(d, vec3(0)));
}

float fCorner2(vec2 p) {
    return length(max(p, vec2(0))) + vmax(min(p, vec2(0)));
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

float smin2(float a, float b, float r) {
    vec2 u = max(vec2(r - a,r - b), vec2(0));
    return max(r, min (a, b)) - length(u);
}

float smax2(float a, float b, float r) {
    vec2 u = max(vec2(r + a,r + b), vec2(0));
    return min(-r, max (a, b)) + length(u);
}

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

bool isMapPass = false;
bool isAoPass = false;

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

float ellip(vec2 p, vec2 s) {
    float r = vmin(s);
    p *= r / s;
    return length(p) - r;
}

vec3 MAIN_COL = vec3(.583,.643,.68) * 1.0;
vec3 modelAlbedo = MAIN_COL;
float isSkin = 0.;

float mHeadInside(vec3 p) {
    pR(p.yz, -.1);
    p.x = abs(p.x);
    float bound = ellip(p - vec3(0,.05,0), vec3(.52,.47,.52));
    bound = smax(bound, abs(p.x) - .39, .2);
    bound = smin3(bound, length(p - vec3(.04,-.5,.28)) - .1, .5);
    bound = smin3(bound, length(p - vec3(.1,-.3,.28)) - .05, .25);
    bound = smin3(bound, length(p - vec3(.02,-.58,.36)) - .05, .13);
    bound = smax(bound, -length(p - vec3(.12,-.1,.6)) + .1, .15);
    // bound = smin(bound, length(p - vec3(0,-.25,.52)) - .1, .1);
    // bound = smin(bound, length(vec3(abs(p.x), p.yz) - vec3(.26,-.11,-.12)) - .23, .1);
    return bound;
}

float mHeadApprox(vec3 p) {

    if (guiEdit) {
        p.z -= .01;
        p.y -= .08;
    } else {
        pR(p.yz, -.1);
    }

    p.x = abs(p.x);
    vec3 pp = p;
    p -= vec3(0,.135,.01);
    pR(p.yz, .1);
    float bound = ellip(p, vec3(.52,.38,.5));
    p = pp;
    p -= vec3(0,-.05,-.00);
    pR(p.yz, .2);
    bound = smin3(bound, ellip(p, vec3(.52,.38,.5)), .07);
    p = pp;
    pR(p.xz, .1);
    bound = smax(bound, abs(p.x) - .51, .45);
    bound = smax(bound, abs(p.x) - .41, .1);
    p = pp;
    p -= vec3(.0,-.5,.2);
    pR(p.yz, .8);
    bound = smin3(bound, ellip(p, vec3(.125,.08,.125)), .33); // jaw
    bound = smin3(bound, ellip(p - vec3(.0,.09,.16), vec3(.022,.02,.02)), .23); // chin
    p = pp;
    bound = smin3(bound, length(p - vec3(.21,-.2,.26)) -.05, .16); // cheek
    bound = smin3(bound, length(p - vec3(.12,-.32,.3)) - .11, .1); // cheek
    bound = smax(bound, -length(p - vec3(.1,-.09,.53)) + .1, .1); // eye
    p -= vec3(0,.02,.08);
    pR(p.yz, -.05);
    float brow = fDisc(p, .33) - .05;
    brow = smax(brow, abs(p.x) - .25, .1);
    bound = smin3(bound, brow, .1); // brow
    p = pp;
    bound = smin3(bound, ellip(p - vec3(0,-.4,.4), vec3(.12,.1,.1)), .05); // lips
    p = pp;
    p -= vec3(0,-.25,.46);
    vec3 nn = normalize(vec3(1.1,.27,.5));
    float nose = smax(dot(p, nn), dot(p, nn * vec3(-1,1,1)), .09) - .072;
    nose = smax(nose, dot(p, normalize(vec3(0,-1,.2))) - .085, .05);
    nose = smax(nose, dot(p, normalize(vec3(1,.0,-.2))) - .1, .05);
    nose = max(nose, length(p) - .3);
    bound = smin3(bound, nose, .05);
    // ear
    p = pp;
    p -= vec3(.35,-.12,-.1);
    float ear = ellip(p.zy, vec2(.09,.11));
    p -= vec3(0,-.15,.05);
    ear = smin(ear, length(p.zy) - .04, .1);
    pR(p.xz, .42);
    pR(p.xy, -.1);
    ear = max(ear, p.x - .025);
    bound = min(bound, ear);
    p = pp;
    // bring head above ear forward to avoid glitch
    p -= vec3(.4,.015,-.16);
    bound = min(bound, length(p) - .035);
    // // // bound = smin(bound, length(p - vec3(0,-.25,.52)) - .1, .1);
    // // // bound = smin(bound, length(vec3(abs(p.x), p.yz) - vec3(.26,-.11,-.12)) - .23, .1);
    return bound;
}

float mHead(vec3 p, bool bounded) {

    if (isMapPass) {
        modelAlbedo = MAIN_COL;
        isSkin = 1.;
    }

    // return fBox(p, vec3(.4));

    // return length(p) - .5;
    
    bounded = bounded && ! isAoPass;

    if (guiEdit) {
        p.z -= .01;
        p.y -= .08;
    } else {
        pR(p.yz, -.1);
    }

    float bound = length(p - vec3(0,.03,0)) - .55;
    bound = smin(bound, length(p - vec3(0,-.45,.28)) - .25, .3);
    bound = smin(bound, length(p - vec3(0,-.25,.52)) - .1, .1);
    bound = smax(bound, abs(p.x) - .4, .2);
    bound = smin(bound, length(vec3(abs(p.x), p.yz) - vec3(.26,-.11,-.12)) - .23, .1);

    return bound += .03;

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

    p = pp;
    p += vec3(-.07,.18,.1);
    d = smin(d, length(p) - .2, .18);

    // cheek base
    p = pp;
    p += vec3(-.2,.12,-.14);
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
    vec3 ls = vec3(.055,.028,.022) * 1.25;
    float w = .192;
    vec2 pl2 = vec2(p.x, length(p.yz * vec2(.79,1)));
    float bottomlip = length(pl2 + vec2(0,w-ls.z)) - w;
    bottomlip = smax(bottomlip, length(pl2 - vec2(0,w-ls.z)) - w, .055);
    d = smin(d, bottomlip, lb);
    

    // top lip
    p = pp;
    p += vec3(0,.38,-.45);
    pR(p.xz, -.3);
    ls = vec3(.065,.03,.05);
    w = ls.x * (-log(ls.y/ls.x) + 1.);
    vec3 pl = p * vec3(.78,1,1);
    float toplip = length(pl + vec3(0,w-ls.y,0)) - w;
    toplip = smax(toplip, length(pl - vec3(0,w-ls.y,0)) - w, .065);
    p = pp;
    p += vec3(0,.33,-.45);
    pR(p.yz, .7);
    float cut;
    cut = dot(p, normalize(vec3(.5,.25,0))) - .056;
    float dip = smin(
        dot(p, normalize(vec3(-.5,.5,0))) + .005,
        dot(p, normalize(vec3(.5,.5,0))) + .005,
        .025
    );
    cut = smax(cut, dip, .04);
    cut = smax(cut, p.x - .1, .05);
    toplip = smax(toplip, cut, .02);
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
    seam = smax(seam, -d - .015, .01); // fix inside shape
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
    float eyeball = length(p) - .088;
    if (isMapPass && eyeball < d) {
        modelAlbedo = vec3(1.2);
        isSkin = 0.;
    }
    d = min(d, eyeball);

    // tear duct
    p = pp;
    p += vec3(-.075,.1,-.37);
    d = min(d, length(p) - .05);


    // Ear base
    // p = pp;
    // p += vec3(-.33,.18,.04);
    // d = smin(d, ellip(p, vec3(.01,.015,.02)), .075);

    // Ear

    // bottom bump
    // p = pp;
    // p += vec3(-.35,.19,.06);
    // d = smin(d, ellip(p, vec3(.015,.015,.02)), .09);


    // position

    p = pp;
    p += vec3(-.405,.12,.10);
    pR(p.xy, -.12);
    pR(p.xz, .35);
    pR(p.yz, -.3);
    vec3 pe = p;

    float earBound = fBox(p - vec3(-.01,-.045,0), vec3(.02,.16,.1) - .05) - .1;
    float earEps = .01;

    if (bounded && earBound > earEps) {

        d = min(d, earBound);

    } else {

        // base
        float ear = p.s + smoothstep(-.05, .1, p.y) * .015 - .005;
        float earback = -ear - mix(.001, .025, smoothstep(.3, -.2, p.y));

        // inner
        pR(p.xz, -.5);
        float iear = ellip(p.zy - vec2(.01,-.03), vec2(.045,.05));
        iear = smin(iear, length(p.zy - vec2(.04,-.09)) - .02, .09);
        float ridge = iear;
        iear = smin(iear, length(p.zy - vec2(.1,-.03)) - .06, .07);
        ear = smax2(ear, -iear, .04);
        earback = smin(earback, iear - .04, .02);

        // ridge
        p = pe;
        pR(p.xz, .2);
        ridge = ellip(p.zy - vec2(.01,-.03), vec2(.045,.055));
        ridge = smin3(ridge, -pRi(p.zy, .2).x - .01, .015);
        // ridge = smin(ridge, ellip(pRi(p.zy - vec2(.025,-.1), .2), vec2(.02,.03)), .025);
        // ridge = smin(ridge, length(p.zy - vec2(.06,-.0)) - .03, .025);
        // ridge = smax2(ridge, -(fBox2(pRi(p.zy - vec2(-.085,.135), .2), vec2(.1)) - .03), .015);
        ridge = smax3(ridge, -ellip(p.zy - vec2(-.01,.1), vec2(.12,.08)), .02);

        float ridger = .01;
        // ridger = ridger * smoothstep(-.15, -.05, p.y);

        ridge = max(-ridge, ridge - ridger);
        // ridge = smax2(ridge, -p.y - .15, .0);


        // modelAlbedo = mix(modelAlbedo, vec3(1,0,0), mod(ridge*50.,1.));
        // modelAlbedo = mix(modelAlbedo, modelAlbedo.brg, step(ridge,0.));
        // return p.x;

        ridge = smax2(ridge, abs(p.x) - ridger/2., ridger/2.);

        // d = min(d, ridge);
        ear = smin(ear, ridge, .045);

        // ear = mix(ear, p.x, smoothstep(.0, .06, p.x));
        // ear = max(ear, p.x- .03);

        // return ridge;

        p = pe;
        // return earback;

        // outline
        float outline = ellip(pRi(p.yz, .2), vec2(.12,.09));
        outline = smin(outline, ellip(p.yz + vec2(.155,-.02), vec2(.035, .03)), .14);
        // outline = smin(outline, ellip(p.yz + vec2(.11,-.07), vec2(.06)), .04);

        // edge
        float eedge = p.x + smoothstep(.2, -.4, p.y) * .06 - .03;

        float edgeo = ellip(pRi(p.yz, .1), vec2(.095,.065));
        edgeo = smin(edgeo, length(p.zy - vec2(0,-.1)) - .03, .1);
        float edgeoin = smax(abs(pRi(p.zy, .15).y + .035) - .01, -p.z-.01, .01);
        edgeo = smax(edgeo, -edgeoin, .05);

        float eedent = smoothstep(-.05, .05, -p.z) * smoothstep(.06, 0., fCorner2(vec2(-p.z, p.y)));
        eedent += smoothstep(.1, -.1, -p.z) * .2;
        eedent += smoothstep(.1, -.1, p.y) * smoothstep(-.03, .0, p.z) * .3;
        eedent = min(eedent, 1.);

        // modelAlbedo = mix(modelAlbedo, vec3(1,0,0), eedent);
        // return p.x;


        eedge += eedent * .06;

        eedge = smax(eedge, -edgeo, .01);
        ear = smin(ear, eedge, .01);
        ear = max(ear, earback);
        // ear = smin2(iear, earback, .01);

        // return ear;
        // return p.x;

        // return eedge;

        ear = smax2(ear, outline, .015);

        // return ear;

        // float earc = smax(-p.x + smoothstep(-.0, -.3, p.y) * .05, outline + .016, .01);
        // ear = smax(ear, -earc, .01);

        // return ear;

        d = smin(d, ear, .015);

        // hole
        p = pp;
        p += vec3(-.36,.19,.06);
        pR(p.xz, -.5);
        pR(p.xy, -.2);
        p.x += .02;
        // d = smax(d, -fHalfCapsule(p.zxy, .02), .04);

        // targus
        p = pp;
        p += vec3(-.34,.2,.02);
        d = smin2(d, ellip(p, vec3(.015,.025,.015)), .035);
        p = pp;
        p += vec3(-.37,.18,.03);
        pR(p.xz, .5);
        pR(p.yz, -.4);
        d = smin(d, ellip(p, vec3(.01,.03,.015)), .015);
    }

    // p = pa;
    // bound = abs(bound + .001) - .001;
    // float ys = .05;
    // p.y = mod(p.y, ys) - ys / 2.;
    // bound = max(bound, abs(p.y) - ys / 4.);
    // d = min(d, bound);

    return d;
}

vec3 projectSurface(vec3 dir, vec3 origin) {
    vec3 ray = dir;
    float dist = 0.;
    const int STEPS = 5;
    for(int i = 0; i < STEPS; i++ ) {
        dist = mHeadApprox(ray - origin);
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

float sinstep(float a) {
    return sin(a * PI - PI * .5) * .5 + .5;
}

const float shell = .07;
const float surfaceOffset = .12;

float loopDuration;
const float stepScale = .15;
const float plodeDuration = .6;
const float blendDuration = .3;
const float blendDelay = .0;
const float plodeMaxDelay = .5;
#define plodeDistance guiPlodeDistance

float mEdge(vec3 p, TriPoints3D points) {
    vec3 edgeAB = normalize(cross(points.center, points.ab));
    vec3 edgeBC = normalize(cross(points.center, points.bc));
    float edge = min(dot(p, edgeAB), -dot(p, edgeBC));
    return edge;
}

float mHeadShell(vec3 p) {
    float d = mHead(p, true);
    // d = abs(d + shell) - shell;
    float di = mHeadInside(p);
    d = max(d, -di - shell * 2.);
    return d;
}

float time;

float invXEase(float x, float k)
{
    k = clamp(k, 0.0001, 10000.0) - 1.0;
    x = clamp(x, 0.0, 1.0);
    float kx = k * x;
    return (x + kx) / (kx + 1.0);
}

float animPlode(float delay) {
    // delay = 0.;
    float start = delay;
    float end = plodeDuration + delay;
    float plode = range(start, end, time);
    // plode = sinstep(sinstep(plode));
    // if (plode > 0.) {
    //     modelAlbedo = spectrum(plode*10.);
    // }
    plode = sinstep(plode);
    // plode = sinstep(plode);
    plode = invXEase(plode, 2.5);
    // plode *= (1. - delay);
    return plode;
}

bool animPlodeStarted(float delay) {
    // return time > startOffset;
    return animPlode(delay) > 0.;
}

float animBlend(float startOffset) {
    float start = 0.;
    float end = start + blendDuration;
    float blend = range(start, end, time - startOffset);
    // modelAlbedo = spectrum(blend*10.);
    // blend = sinstep(sinstep(blend));
    // blend = invXEase(blend, 2.);
    blend = sinstep(blend);
    blend = sinstep(blend);
    return blend;
}

float fWaypoint(vec3 p, vec3 trans, vec4 rot, float scale) {
    p -= trans;
    p = rotate_vector(p, q_conj(rot));
    float d = fBox(p, scale * vec3(.02, .05, .03) * 1.);
    p -= scale * vec3(.01,.025,.04);
    d = min(d, fBox(p, scale * vec3(.01,.025,.01) * 1.));
    return d;
}

struct Waypoint {
    vec3 trans;
    vec4 rot;
    float scale;
};

Waypoint way0;
Waypoint way1;
Waypoint way2;
Waypoint way3;
Waypoint way4;

float focusDelay;

float calcDelay(TriPoints3D points) {
    // return 0.;
    // return range(1., -1., normalize(points.hexCenter).y) * .3;
    return points.id * plodeMaxDelay;
}

void calcWaypoints() {

    TriPoints3D focusPoints;
    vec3 focusHexCenter;
    vec3 focusP;

    focusHexCenter = normalize(vec3(2, 1, 1));
    // focusHexCenter = normalize(vec3(0, 1, PHI + 1.));
    focusPoints = geodesicTriPoints(focusHexCenter, 1.);
    vec3 hexCenter = focusPoints.hexCenter;
    focusDelay = calcDelay(focusPoints);
    vec3 projected = projectSurface(hexCenter) - hexCenter * surfaceOffset;
    projected += hexCenter * plodeDistance;

    way0.scale = 1. / stepScale;
    way0.trans = vec3(0);
    way0.rot = QUATERNION_IDENTITY;
    // way0.rot = q_look_at(vec3(0,0,1), vec3(0,1,0));

    way1.scale = way0.scale * stepScale;
    way1.trans = projected * way1.scale;
    way1.rot = q_look_at(hexCenter, vec3(0,-1,0));
    // way1.rot = QUATERNION_IDENTITY;

    // idunno
    // hexCenter.x *= -1.;
    // projected.x *= -1.;

    way2.scale = way1.scale * stepScale;
    way2.trans = way1.trans + rotate_vector(projected * way2.scale, way1.rot);
    way2.rot = q_look_at(rotate_vector(hexCenter, way1.rot), rotate_vector(vec3(0,-1,0), way1.rot));
    // way2.rot = QUATERNION_IDENTITY;

    way3.scale = way2.scale * stepScale;
    way3.trans = way2.trans + rotate_vector(projected * way3.scale, way2.rot);
    way3.rot = q_look_at(rotate_vector(hexCenter, way2.rot), rotate_vector(vec3(0,-1,0), way2.rot));
    // way3.rot = QUATERNION_IDENTITY;

    way4.scale = way3.scale * stepScale;
    way4.trans = way3.trans + rotate_vector(projected * way4.scale, way3.rot);
    way4.rot = q_look_at(rotate_vector(hexCenter, way3.rot), rotate_vector(vec3(0,-1,0), way3.rot));
    // way4.rot = QUATERNION_IDENTITY;

}

vec3 Catmull(vec3 p0, vec3 p1, vec3 p2, vec3 p3, float t){
    return (((-p0 + p1*3. - p2*3. + p3)*t*t*t + (p0*2. - p1*5. + p2*4. - p3)*t*t + (-p0 + p2)*t + p1*2.)*.5);
}

vec3 bezier(vec3 p0, vec3 p1, vec3 p2, vec3 p3, float t) {
    vec3 a0 = mix(p0, p1, t);
    vec3 a1 = mix(p1, p2, t);
    vec3 a2 = mix(p2, p3, t);
    vec3 b0 = mix(a0, a1, t);
    vec3 b1 = mix(a1, a2, t);
    return mix(a0, b1, t);
}

vec3 midpointNormal(vec3 a, vec3 b, vec3 c) {
    vec3 an = normalize(a - b);
    vec3 cn = normalize(c - b);
    vec3 mid = mix(an, cn, .5);
    return normalize(-mid);
}

vec3 planeNormal(vec3 a, vec3 b, vec3 c) {
    return normalize(cross(b - a, c - a));
}

vec3 controlDir(vec3 a, vec3 b, vec3 c) {
    return cross(
        midpointNormal(a, b, c),
        planeNormal(a, b, c)
    );
}

vec3 tweenCameraPos(float t, Waypoint w0, Waypoint w1, Waypoint w2, Waypoint w3) {
    vec3 c1 = w1.trans - controlDir(w0.trans, w1.trans, w2.trans) * distance(w1.trans, w2.trans) * .6;
    vec3 c2 = w2.trans + controlDir(w1.trans, w2.trans, w3.trans) * distance(w1.trans, w2.trans) * .2;
    t = pow(t, 1.08); // correct for non-constant speed
    vec3 p = bezier(w1.trans, c1, c2, w2.trans, t);
    return p;
}

float tweenCamera(inout vec3 p, float animTime, Waypoint w0, Waypoint w1, Waypoint w2, Waypoint w3) {
    float ar = (pow(stepScale, animTime) - 1.) / (stepScale - 1.);
    float focusScale = mix(w1.scale, w2.scale, ar);
    p *= focusScale;
    vec4 rot = q_slerp(w1.rot, w2.rot, animTime);
    p = rotate_vector(p, rot);
    p += tweenCameraPos(ar, w0, w1, w2, w3);
    return focusScale;
}

float blendHeadPrepare(float head, float t, float scale) {
    float blendExpand = range(1., .5, t) * scale;
    head = smin(head, head, blendExpand) + blendExpand * .2;
    return head;
}

    float sectionEps = .001;

float drawPlode(inout vec3 p, inout float bound, float level, TriPoints3D points, float start) {
    float scale = pow(stepScale, level);
    float sectionEdge = mEdge(p, points) * scale;
    float inner = mHeadInside(p) * scale;
    p -= points.hexCenter * animPlode(start) * plodeDistance;
    float d = mHeadShell(p) * scale;
    sectionEdge = max(sectionEdge, d  - .15 * scale);
    sectionEdge = max(sectionEdge, -inner  - .15 * scale);
    bound = min(bound, sectionEdge + sectionEps * scale);
    float plodeEdge = mEdge(p, points) * scale;
    if (isMapPass && -plodeEdge > d) {
        modelAlbedo = MAIN_COL;
        isSkin = 0.;
    }
    d = max(d, -plodeEdge);
    return d;
}

void moveIntoHex(inout vec3 p, inout float level, TriPoints3D points) {
    p -= projectSurface(points.hexCenter) - points.hexCenter * surfaceOffset;
    p /= stepScale;
    p *= calcLookAtMatrix(vec3(0), points.hexCenter, vec3(0,1,0));
    p.x *= -1.; // somehow look at flips this
    level += 1.;
}

float drawBlend(float d, vec3 p, float level, float start, float bound) {
    vec3 _modelAlbedo = modelAlbedo;
    float _isSkin = isSkin;
    float scale = pow(stepScale, level);
    float d2 = mHead(p, false) * scale;
    float blend = animBlend(start);
    // blend = .5;
    d2 = blendHeadPrepare(d2, blend, scale);
    d = mix(d, d2, blend);
    modelAlbedo = mix(_modelAlbedo, modelAlbedo, blend);
    isSkin = mix(_isSkin, isSkin, blend);
    if ( ! isAoPass) {
        d = min(d, bound);
    }
    return d;
}

float mapWaypoints(vec3 p) {
    // float d = 1e12;
    // const float WITER = 20.;
    // for (float i = 0.; i < WITER; i++){
    //     d = min(d, length(p - tweenCameraPos(i / WITER)) - .0003);
    // }
    // return d;
    // return min(min(
    //     fWaypoint(p, way0.trans, way0.rot, way0.scale),
    //     fWaypoint(p, way1.trans, way1.rot, way1.scale)
    // ),fWaypoint(p, way2.trans, way2.rot, way2.scale));
    return min(
        min(
            min(
                fWaypoint(p, way0.trans, way0.rot, way0.scale),
                fWaypoint(p, way1.trans, way1.rot, way1.scale)
            ),
            min(
                fWaypoint(p, way2.trans, way2.rot, way2.scale),
                fWaypoint(p, way3.trans, way3.rot, way3.scale)
            )
        ),
        fWaypoint(p, way4.trans, way4.rot, way4.scale)
    );
}

// #define ANOTHER_LEVEL

float mapAnimMain(vec3 p) {

    TriPoints3D points;
    float plodeEdge;
    float d, d2;
    float blend;
    float bound = 1e12;
    float start;
    float delay = 0.;
    float level = 0.;



    points = geodesicTriPoints(p, 1.);
    delay += calcDelay(points);
    // start at focus blend finishing
    #ifndef ANOTHER_LEVEL
        start = delay - loopDuration;
    #else
        start = delay - loopDuration * 2.;
    #endif
    d = drawPlode(p, bound, level, points, start);

    if ( ! guiStep0) {
        return min(d, bound);
    }

    moveIntoHex(p, level, points);

    start += blendDelay;
    if ( ! animPlodeStarted(start + blendDuration) || ! guiStep1) {
        d = drawBlend(d, p, level, start, bound);
        // return min(d, focusDebug);
        return d;
    }




    points = geodesicTriPoints(p, 1.);
    delay = calcDelay(points);
    start += blendDuration + delay;
    d = drawPlode(p, bound, level, points, start);

    if ( ! guiStep2) {
        return min(d, bound);
    }

    moveIntoHex(p, level, points);

    start += blendDelay;

    #ifndef ANOTHER_LEVEL
        return drawBlend(d, p, level, start, bound);
    #endif

    if ( ! animPlodeStarted(start + blendDuration) || ! guiStep3) {
        return drawBlend(d, p, level, start, bound);
    }

    points = geodesicTriPoints(p, 1.);
    delay = calcDelay(points);
    start += blendDuration + delay;
    d = drawPlode(p, bound, level, points, start);

    if ( ! guiStep3) {
        return min(d, bound);
    }

    moveIntoHex(p, level, points);

    start += blendDelay;
    return drawBlend(d, p, level, start, bound);
}

float mapAnim(vec3 p) {
    float animTime = range(.0, loopDuration, time);
    float focusScale;
    #ifndef ANOTHER_LEVEL
        focusScale = tweenCamera(p, animTime, way0, way1, way2, way3);
    #else
        focusScale = tweenCamera(p, animTime, way1, way2, way3, way4);
    #endif
    // return mapWaypoints(p) / focusScale;
    return mapAnimMain(p) / focusScale;
    return min(
        mapAnimMain(p),
        mapWaypoints(p)
    )  / focusScale;
}


// Hexagonal prism, circumcircle variant
float fHexagonCircumcircle(vec3 p, vec2 h) {
    vec3 q = abs(p);
    return max(q.y - h.y, max(q.x*sqrt(3.)*0.5 + q.z*0.5, q.z) - h.x);
    //this is mathematically equivalent to this line, but less efficient:
    //return max(q.y - h.y, max(dot(vec2(cos(PI/3), sin(PI/3)), q.zx), q.z) - h.x);
}

vec3 LIGHT_POS = vec3(-.1,.12,.2) * 5.;

float map(vec3 p) {

    // if ( ! guiEdit) {
        float ad = mapAnim(p);
        // ad = min(ad, length(p - LIGHT_POS) - .01);
        // ad = length(p - LIGHT_POS) - .1;
        return ad;
    // }

    // float t = clamp(mod(iTime, 1.5), 0., 1.);
    // float d = fHexagonCircumcircle(p.yzx + vec3(.1,-.1,0), vec2(.5,.5));
    // float head = mHead(p, false);
    // head = blendHeadPrepare(head, t);
    // d = mix(d, head, t);

    // float w = .02;
    // d = abs(d + w) - w;
    // d = max(d, p.y - .1);


    // float split = p.x;
    // float d = mHead(p, false);
    // float di = mHeadApprox(p);
    // TriPoints3D points = geodesicTriPoints(p, 1.);
    // p *= calcLookAtMatrix(vec3(p), points.hexCenter, vec3(0,1,0));
    // float mask = length(p.xy) - .05;
    // if (split > 0.) {
    //     float ddd = d; d = di; di = ddd;
    // }
    // float diff = d - di;
    // d = max(d, -mask);
    // di = max(di, mask);
    // // float split = p.x;
    // // modelAlbedo = spectrum(diff*10.) * mix(.8, 1.2, step(diff * sign(split), 0.));
    // // d = max(d, split);
    // // di = max(di, -split);
    // if (di < d) {
    //     if (isMapPass) modelAlbedo = spectrum(diff*10.);
    //     return di;
    // }
    // if (isMapPass) modelAlbedo = vec3(1);
    // return d;

    // return max(mHeadShell(p), p.y - iTime + .5);

    // float d = mHeadInside(p);
    // TriPoints3D points;
    // points = geodesicTriPoints(p, 3.);
    // vec3 o = projectSurface(points.hexCenter, vec3(0));
    // d = min(d, length(p - o) - .03);
    // return d;

    p -= vec3(-.1,-.02,-.24);
    pR(p.xz, .7);
    pR(p.yz, 0.2);
    pR(p.xy, -.15);
    float scale = .45;
    p /= scale;
    // return mHead(p, false) * scale;
    TriPoints3D points;
    float d;
    float delay;
    float bound = 1e12;
    float start;
    float level = 0.;
    vec3 pp = p;
    points = geodesicTriPoints(p, 1.);
    // if (isMapPass) modelAlbedo = spectrum(points.id);
    start += calcDelay(points);
    d = drawPlode(p, bound, level, points, start);
    moveIntoHex(p, level, points);
    start += blendDelay;
    d = drawBlend(d, p, level, start, bound);
    // d = max(d, -pp.z);
    d *= scale;
    return d;
}

float mapPlayground(vec3 p) {
    float d = mHead(p, false);

    // return d;

    // d = abs(d + .03) - .03;
    // d = max(d, p.y - .1);

    // p.y -= sin(iTime) * .1;
    pR(p.xy, iTime/2.);
    pR(p.xz, iTime/3.);
    pR(p.yz, -iTime);
    vec3 iv = icosahedronVertexComplete(p);
    vec3 dv = dodecahedronVertex(p);
    if (length(p - iv) > length(p - dv)) {
        // iv = dv;
    }

    float shape = dot(p, iv) + .3;
    shape = length(p - iv * mix(.4, .6, cos(iTime / 2.) * .5 + .5)) - .05;
    
    d = smin2(d, shape, 0.2);

    return d;

    // p = pa;
    // d += length(sin(p * 60. + vec3(0,iTime*3.,0).zxy)) * .005 - .005;

    // d += sin(cc * 100. - iTime) * smoothstep(.5, .0, cc) * .01;

    if (d < -.01) {
        modelAlbedo = vec3(.2,.25,.3);
        modelAlbedo = mix(modelAlbedo, vec3(.7,.8,.9), .5);
        modelAlbedo = vec3(0,1,0);
    }

    float h = helix(p.xzy, 35., .06);

    d = abs(d + .01) - .01;
    
    d = max(d, h);

    return d;
}

bool hitDebugPlane = false;

float mapDebug(vec3 p) {
    float d = map(p);
    return d;
    // return d;
    // if ( ! guiDebug) {
    //     return d;
    // }
    float plane = abs(p.y + .1);
    //plane= abs(p.z);
    hitDebugPlane = plane < abs(d);
    // hitDebugPlane = true;
    return hitDebugPlane ? plane : d;
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

#define HASHSCALE1 .1031
float hash(float p)
{
    vec3 p3  = fract(vec3(p) * HASHSCALE1);
    p3 += dot(p3, p3.yzx + 19.19);
    return fract((p3.x + p3.y) * p3.z);
}

vec3 randomSphereDir(vec2 rnd)
{
    float s = rnd.x*PI*2.;
    float t = rnd.y*2.-1.;
    return vec3(sin(s), cos(s), t) / sqrt(1.0 + t * t);
}
vec3 randomHemisphereDir(vec3 dir, float i)
{
    vec3 v = randomSphereDir( vec2(hash(i+1.), hash(i+2.)) );
    return v * sign(dot(v, dir));
}

float ambientOcclusion( in vec3 p, in vec3 n, in float maxDist, in float falloff )
{
    isAoPass = true;
    const int nbIte = 32;
    const float nbIteInv = 1./float(nbIte);
    const float rad = 1.-1.*nbIteInv; //Hemispherical factor (self occlusion correction)
    
    float ao = 0.0;
    
    for( int i=0; i<nbIte; i++ )
    {
        float l = hash(float(i))*maxDist;
        vec3 rd = normalize(n+randomHemisphereDir(n, l )*rad)*l; // mix direction with the normal
                                                                // for self occlusion problems!
        
        ao += (l - max(map( p + rd ),0.)) / maxDist * falloff;
    }
    isAoPass = false;
    return clamp( 1.-ao*nbIteInv, 0., 1.);
}



float calcAO( in vec3 pos, in vec3 nor )
{
    return ambientOcclusion(pos, nor, .2, .9);
    isAoPass = true;
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
    isAoPass = false;
    return clamp( 1.0 - 3.0*occ, 0.0, 1.0 );
}

#pragma glslify: distanceMeter = require(../clifford-torus/distance-meter.glsl)

// float 

float calcSoftshadow(vec3 ro, vec3 rd, float initialStep, float tmax) {
    isAoPass = true;
    float lit = 1.0;
    float d;
    float rl = initialStep;
    for(int i = 0; i < 256; i++) {
        d = map(ro + rd * rl);
        lit = min(lit, 2. * d / min(rl, tmax / 2.));
        rl += d / 10.;
        if (lit < 0.0001 || rl > tmax) break;
    }
    isAoPass = false;
    return saturate(lit);
}

float G1V(float dnv, float k){
    return 1.0/(dnv*(1.0-k)+k);
}

float ggx(vec3 n, vec3 v, vec3 l, float rough, float f0){
    float alpha = rough*rough;
    vec3 h = normalize(v+l);
    float dnl = clamp(dot(n,l), 0.0, 1.0);
    float dnv = clamp(dot(n,v), 0.0, 1.0);
    float dnh = clamp(dot(n,h), 0.0, 1.0);
    float dlh = clamp(dot(l,h), 0.0, 1.0);
    float f, d, vis;
    float asqr = alpha*alpha;
    const float pi = 3.14159;
    float den = dnh*dnh*(asqr-1.0)+1.0;
    d = asqr/(pi * den * den);
    dlh = pow(1.0-dlh, 5.0);
    f = f0 + (1.0-f0)*dlh;
    float k = alpha/1.0;
    vis = G1V(dnl, k)*G1V(dnv, k);
    float spec = dnl * d * f * vis;
    return spec;
}


bool SHADE_DEBUG = false;

vec3 shadeLight(vec3 p, vec3 rd, vec3 n, float fresnel, vec3 lp, vec3 lc, vec3 albedo) {
    vec3 ld = normalize(lp-p);

    float shadow = calcSoftshadow(p, ld, .01, .75);
    shadow = mix(shadow, 1., .1);

    float diff = max(0.0, dot(n, ld));
    float spec = ggx(n, rd, ld, 3., fresnel);

    diff *= shadow;
    spec *= shadow;

    vec3 specC = lc * 4.;
    specC = spectrum(dot(n, ld) - .2) * 1.;
    // specC = spectrum(mix(-.5, .8, dot(n, ld))) * 1.;

    return (albedo * diff + spec * specC);
}


vec3 screenhash;

vec3 shade(vec3 p, vec3 rd, vec3 n) {
    float fresnel = pow( max(0.0, 1.0+dot(n, rd)), 5.0 );
    vec3 albedo = modelAlbedo;

    vec3 ambient = vec3(.6) * albedo;
    float ao = calcAO(p, n);
    ambient *= ao;

    n = mix(n, normalize(n + screenhash * .1), isSkin);

    vec3 l1 = shadeLight(p, rd, n, fresnel, LIGHT_POS, vec3(1), albedo);
    //// vec3 l1 = shadeLight(p, rd, n, fresnel, vec3(-.2,.1,.1) * 5., vec3(1,0,0), albedo);
    vec3 l2 = shadeLight(p, rd, n, fresnel, vec3(.08,-.2,-.025) * 5., vec3(1), albedo);
    //// vec3 l2 = shadeLight(p, rd, n, fresnel, -LIGHT_POS, vec3(1), albedo);
    // vec3 l3 = shadeLight(p, rd, n, fresnel, vec3(.2,-.1,.0) * 5., vec3(1), albedo);

    // if ( false) {
    //     return ambient + l1 + l3 + l2;
    // }

    return ambient + l1 + l2;
}

// linear white point
const float W = 1.2;
const float T2 = 7.5;

float filmic_reinhard_curve (float x) {
    float q = (T2*T2 + 1.0)*x*x;    
    return q / (q + x + T2*T2);
}

vec3 filmic_reinhard(vec3 x) {
    float w = filmic_reinhard_curve(W);
    return vec3(
        filmic_reinhard_curve(x.r),
        filmic_reinhard_curve(x.g),
        filmic_reinhard_curve(x.b)) / w;
}

vec3 render(Hit hit, vec3 col) {
    if ( ! hit.isBackground) {
        // col = hit.normal * .5 + .5;
        // col = vec3(dot(hit.normal, vec3(.2,.5,1.3))) * modelAlbedo;
        col = shade(hit.pos, hit.rayDirection, hit.normal);
    }
    if (hitDebugPlane) {
        float d = map(hit.pos);
        col = distanceMeter(d * 2., hit.rayLength, hit.rayDirection, hit.rayOrigin);
    }
    return col;
}


// --------------------------------------------------------
// Ray Marching
// Adapted from: https://www.shadertoy.com/view/Xl2XWt
// --------------------------------------------------------

const float MAX_TRACE_DISTANCE = 4.5;
const float INTERSECTION_PRECISION = .0001;
const int NUM_OF_TRACE_STEPS = 550;

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
    isMapPass = true;
    float steps = 0.;

    for(int i = 0; i < NUM_OF_TRACE_STEPS; i++){
        if (currentDist < INTERSECTION_PRECISION) {
            isBackground = false;
            break;
        }
        if (rayLength > MAX_TRACE_DISTANCE) {
            break;
        }
        currentDist = mapDebug(rayOrigin + rayDirection * rayLength);
        rayLength += currentDist;
        steps += 1.;
    }

    isMapPass = false;
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


void main() {

    calcWaypoints();

    loopDuration = blendDelay + blendDuration + focusDelay;

    vec2 vertex = vVertex;
    vertex = mod(vertex, 1.) * 2. - 1.;

    time = iTime;
    time /= 3.;
    // time -= .1;
    // time *= .333;

    if (guiMultiscreen) {
        float screen = floor(vVertex.x) + floor(1.-vVertex.y) * 2.;
        time += screen / 4.;
    }

    if (guiLoop) {
        time = mod(time, 1.);
    }
    time *= loopDuration;

    SHADE_DEBUG = (gl_FragCoord.x / iResolution.x) > .5;
    SHADE_DEBUG = SHADE_DEBUG == (gl_FragCoord.y / iResolution.y) > .5;

    vec3 dir2 = dir;
    vec3 cameraForward2 = cameraForward;

    if (guiMultiscreen) {
        dir2 = vec3(vertex.x * fov * aspect, vertex.y * fov,-1.0) * mat3(vView);
        cameraForward2 = vec3(0,0,-1) * mat3(vView);
    }

    vec3 rayOrigin = eye;
    vec3 rayDirection = normalize(dir2);

    vec2 p = (-iResolution.xy + 2. * gl_FragCoord.xy) / iResolution.y;

    if (guiFixedCamera) {
        vec3 camPos = vec3(0,-.1,.5) * guiCamDistance;
        vec3 camTar = vec3(0,-.05,0) * guiCamLookDown;
        vec3 camUp = vec3(0,1,0);
        mat3 camMat = calcLookAtMatrix(camPos, camTar, camUp);
        float focalLength = 2.4;
        rayDirection = normalize(camMat * vec3(p, focalLength));
        rayOrigin = camPos;
    }

    vec3 rayPosition = rayOrigin;

    screenhash = hash3(p + time);

    vec3 bg = vec3(.7,.8,.9) * 1.1;
    bg *= .8;
    // bg = mix(vec3(.5), vec3(1,0,1), .25);
    bg = MAIN_COL;

    Hit hit = raymarch(rayOrigin, rayDirection);
    vec3 color = render(hit, bg);

    if ( ! guiFixedCamera) {
        // color = vec3(0,0,1);
        // color = pow(color, vec3(1. / 2.2)); // Gamma

        float eyeHitZ = -hit.rayLength * dot(rayDirection, cameraForward2);

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
            // alpha = 0.;
        }

        vec3 polyColor = texture2D(uSource, gl_FragCoord.xy / iResolution.xy).rgb;
        color = mix(polyColor, color, alpha);

        if (abs(polyD - rayD) < .001) {
            // color = vec3(1);
        }

        gl_FragDepthEXT = depth;
    }

    float fog = 1. - exp( -(hit.rayLength - length(rayOrigin)) * 1.8 );
    color = mix(color, bg, fog);

    // color *= 1.2;

    // float tintl = color.r;
    // tintl = pow(tintl, 2.);
    // vec3 tint = spectrum(mix(.5, -.2, tintl));
    // // tint *= pow(color, vec3(2.));
    // color *= mix(vec3(1.), tint, .3);
    // // color = pow(color, vec3(1. / 2.2)); // Gamma

    // color *= 1.2;

    // color = spectrum(hit.steps / 400.);

    // if (SHADE_DEBUG) color *= 2.;

    color = filmic_reinhard(color);

    gl_FragColor = vec4(color, 1);
    
}
