#define SHADER_NAME quad.frag

precision highp float;

uniform vec2 iResolution;
uniform float iGlobalTime;
uniform vec4 iMouse;
uniform sampler2D iChannel0;


void mainImage(out vec4 a, in vec2 b);

void main() {
    mainImage(gl_FragColor, gl_FragCoord.xy);
}


/* SHADERTOY FROM HERE */
vec2 mousee;

#pragma glslify: renderSuperstructure = require(./shaders/intergalactic.glsl, iChannel0=iChannel0, iGlobalTime=iGlobalTime, mousee=mousee)

// Author:
// Title:

#ifdef GL_ES
precision mediump float;
#endif

uniform vec2 u_resolution;
uniform vec2 u_mouse;
uniform float u_time;

float time;

//#define DEBUG
//#define SHOW_STEPS
//#define SHOW_BOUNDS
//#define FAST_COMPILE
#define SHOW_ZOOM
//#define DEBUG_MODEL
//#define CAMERA_CONTROL

#define SHOW_FOG
#define SHADOWS
#define SHOW_SPACE
#define SHOW_MODELS



// --------------------------------------------------------
// Spectrum colour palette
// IQ https://www.shadertoy.com/view/ll2GD3
// --------------------------------------------------------

vec3 pal( in float t, in vec3 a, in vec3 b, in vec3 c, in vec3 d ) {
    return a + b*cos( 6.28318*(c*t+d) );
}

vec3 spectrum(float n) {
    return pal( n, vec3(0.5,0.5,0.5),vec3(0.5,0.5,0.5),vec3(1.0,1.0,1.0),vec3(0.0,0.33,0.67) );
}


// --------------------------------------------------------
// HG_SDF
// https://www.shadertoy.com/view/Xs3GRB
// --------------------------------------------------------

#define PI 3.14159265359
#define PHI (1.618033988749895)

// Clamp to [0,1] - this operation is free under certain circumstances.
// For further information see
// http://www.humus.name/Articles/Persson_LowLevelThinking.pdf and
// http://www.humus.name/Articles/Persson_LowlevelShaderOptimization.pdf
#define saturate(x) clamp(x, 0., 1.)

float smax(float a, float b, float r) {
    #ifdef FAST_COMPILE
    	return max(a, b);
    #else
        float m = max(a, b);
        if ((-a < r) && (-b < r)) {
            return max(m, -(r - sqrt((r+a)*(r+a) + (r+b)*(r+b))));
        } else {
            return m;
        }
    #endif
}

// The "Round" variant uses a quarter-circle to join the two objects smoothly:
float smin(float a, float b, float r) {
	#ifdef FAST_COMPILE
    	return min(a, b);
    #else
    	float m = min(a, b);
        if ((a < r) && (b < r) ) {
            return min(m, r - sqrt((r-a)*(r-a) + (r-b)*(r-b)));
        } else {
         	return m;
        }
    #endif
}

// The "Chamfer" flavour makes a 45-degree chamfered edge (the diagonal of a square of size <r>):
float cmin(float a, float b, float r) {
    float m = min(a, b);
    //if ((a < r) && (b < r)) {
        return min(m, (a - r + b)*sqrt(0.5));
    //} else {
        return m;
    //}
}

// Intersection has to deal with what is normally the inside of the resulting object
// when using union, which we normally don't care about too much. Thus, intersection
// implementations sometimes differ from union implementations.
float cmax(float a, float b, float r) {
    float m = max(a, b);
    if (r <= 0.) return m;
    if (((-a < r) && (-b < r)) || (m < 0.)) {
        return max(m, (a + r + b)*sqrt(0.5));
    } else {
        return m;
    }
}


// Rotate around a coordinate axis (i.e. in a plane perpendicular to that axis) by angle <a>.
// Read like this: R(p.xz, a) rotates "x towards z".
// This is fast if <a> is a compile-time constant and slower (but still practical) if not.
void pR(inout vec2 p, float a) {
    p = cos(a)*p + sin(a)*vec2(p.y, -p.x);
}

// Repeat around the origin by a fixed angle.
// For easier use, num of repetitions is use to specify the angle.
float pModPolar(inout vec2 p, float repetitions) {
	float angle = 2.*PI/repetitions;
	float a = atan(p.y, p.x) + angle/2.;
	float r = length(p);
	float c = floor(a/angle);
	a = mod(a,angle) - angle/2.;
	p = vec2(cos(a), sin(a))*r;
	// For an odd number of repetitions, fix cell index of the cell in -x direction
	// (cell index would be e.g. -5 and 5 in the two halves of the cell):
	if (abs(c) >= (repetitions/2.)) c = abs(c);
	return c;
}

// Reflect space at a plane
float pReflect(inout vec3 p, vec3 planeNormal, float offset) {
    float t = dot(p, planeNormal)+offset;
    if (t < 0.) {
        p = p - (2.*t)*planeNormal;
    }
    return sign(t);
}

// Cone with correct distances to tip and base circle. Y is up, 0 is in the middle of the base.
float fCone(vec3 p, float radius, float height) {

    #ifdef FAST_COMPILE
        float q = length(p.xz);
        vec2 c = normalize(vec2(height, radius));
        return dot(c, vec2(q, p.y - height));
	#else
        vec2 q = vec2(length(p.xz), p.y);
        vec2 tip = q - vec2(0, height);
        vec2 mantleDir = normalize(vec2(height, radius));
        float mantle = dot(tip, mantleDir);
        float d = max(mantle, -q.y);
        float projected = dot(tip, vec2(mantleDir.y, -mantleDir.x));

        // distance to tip
        if ((q.y > height) && (projected < 0.)) {
            d = max(d, length(tip));
        }

        // distance to base ring
        if ((q.x > radius) && (projected > length(vec2(height, radius)))) {
            d = max(d, length(q - vec2(radius, 0)));
        }
        return d;
	#endif
}

float fCone(vec3 p, float radius, float height, vec3 direction) {
    p = reflect(p, normalize(mix(vec3(0,1,0), -direction, .5)));
    //p -= vec3(0,height,0);
    return fCone(p, radius, height);
}

float fCone(vec3 p, float radius, vec3 start, vec3 end) {
    float height = length(start - end);
    vec3 direction = normalize(end - start);
    return fCone(p - start, radius, height, direction);
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

// Plane with normal n (n is normalized) at some distance from the origin
float fPlane(vec3 p, vec3 n, float distanceFromOrigin) {
    return dot(p, n) + distanceFromOrigin;
}


float fPaper(vec3 p, vec3 n, float l) {
    return max(
        fPlane(p, n, l),
        fPlane(p, -n, -l - .0005)
    );
}


// --------------------------------------------------------
// Rotation
// --------------------------------------------------------

mat3 sphericalMatrix(vec2 thetaphi) {
    float theta = -thetaphi.y;
    float phi = thetaphi.x;
    float cx = cos(theta);
    float cy = cos(phi);
    float sx = sin(theta);
    float sy = sin(phi);
    return mat3(
        cy, -sy * -sx, -sy * cx,
        0, cx, sx,
        sy, cy * -sx, cy * cx
    );
}


// --------------------------------------------------------
// knighty
// https://www.shadertoy.com/view/MsKGzw
// --------------------------------------------------------

struct Tri {
    vec3 a;
    vec3 b;
    vec3 c;
};
    
struct TriPlanes {
    vec3 ab;
    vec3 bc;
    vec3 ca;
};
    
    
vec3 facePlane;
vec3 uPlane;
vec3 vPlane;

vec3 nc,pab,pbc,pca;
Tri triV;
TriPlanes triP;

void initPolyhedron(int Type) {//setup folding planes and vertex
    float cospin=cos(PI/float(Type)), scospin=sqrt(0.75-cospin*cospin);
    nc=vec3(-0.5,-cospin,scospin);//3rd folding plane. The two others are xz and yz planes
    pab=vec3(0.,0.,1.);
    pbc=vec3(scospin,0.,0.5);//No normalization in order to have 'barycentric' coordinates work evenly
    pca=vec3(0.,scospin,cospin);
    pbc=normalize(pbc); pca=normalize(pca);//for slightly better DE. In reality it's not necesary to apply normalization :) 

    // Triangle vertices
    triV = Tri(pbc, pab, pca);
    // Triangle edge plane normals 
    triP = TriPlanes( 
        normalize(cross(triV.a, triV.b)),
        normalize(cross(triV.b, triV.c)),
        normalize(cross(triV.c, triV.a))
    );
    
    facePlane = pca;
    uPlane = cross(vec3(1,0,0), facePlane);
    vPlane = vec3(1,0,0);    
}

// Repeat space to form subdivisions of an icosahedron
void pIcosahedron(inout vec3 p) {
    p = abs(p);
    pReflect(p, nc, 0.);
    p.xy = abs(p.xy);
    pReflect(p, nc, 0.);
    p.xy = abs(p.xy);
    pReflect(p, nc, 0.);
}


// Barycentric to Cartesian
vec3 bToC(float a, float b, float c) {
    return a * triV.a + b * triV.b + c * triV.c;
}
vec3 bToC(int a, int b, int c) {
    return bToC(float(a), float(b), float(c));
}

// Barycentric to Cartesian normalized
vec3 bToCn(float a, float b, float c) {
    return normalize(bToC(a, b, c));
}
vec3 bToCn(int a, int b, int c) {
    return bToCn(float(a), float(b), float(c));
}


// --------------------------------------------------------
// GEODESIC TILING
// https://www.shadertoy.com/view/llGXWc
// --------------------------------------------------------

const float sqrt3 = 1.7320508075688772;
const float i3 = 0.5773502691896258;

const mat2 cart2tri = mat2(1, 0, i3, 2. * i3);
const mat2 tri2cart = mat2(1, 0, -.5, .5 * sqrt3);

vec2 closestTri(vec2 p) {
    p = cart2tri * p;
    vec2 pf = fract(p);
    vec2 v = vec2(1./3., 2./3.);
    vec2 tri = mix(v, v.yx, step(pf.y, pf.x));
    tri += floor(p);
    tri = tri2cart * tri;
    return tri;
}

vec2 closestHex(vec2 p) {
    p = cart2tri * p;
	vec2 pi = floor(p);
	vec2 pf = fract(p);
    float split0 = step(pf.x * .5, pf.y);
    float split1 = step(pf.x * .5 + .5, pf.y);
    float split2 = step(pf.x, 1. - pf.y);
    float split3 = step(pf.y * .5 + (.5 * split2), pf.x);
    vec2 left = vec2(0, split1);
    vec2 right = vec2(1, split0);
    vec2 hex = mix(left, right, split3);
    hex += vec2(pi);    
    hex = tri2cart * hex;
    return hex;
}

vec3 intersection(vec3 n, vec3 planeNormal, float planeOffset) {
    float denominator = dot(planeNormal, n);
    float t = (dot(vec3(0), planeNormal) + planeOffset) / -denominator;
    return n * t;
}

vec2 icosahedronFaceCoordinates(vec3 p) {
    vec3 i = intersection(normalize(p), facePlane, -1.);
    return vec2(dot(i, uPlane), dot(i, vPlane));
}

vec3 faceToSphere(vec2 facePoint) {
	return normalize(facePlane + (uPlane * facePoint.x) + (vPlane * facePoint.y));
}

const float faceRadius = 0.3819660112501051;

vec3 geodesicPoint(vec3 p, float subdivisions) {
	float uvScale = subdivisions / faceRadius / 2.;
    vec2 uv = icosahedronFaceCoordinates(p);
    vec2 tri = closestHex(uv * uvScale);
    return faceToSphere(tri / uvScale);
}


// --------------------------------------------------------
// Modelling
// --------------------------------------------------------

bool isMasked;
bool useBounds;

struct Model {
    float dist;
    float id;
};
    
// checks to see which intersection is closer
Model opU( Model m1, Model m2 ){
    if (m1.dist < m2.dist) {
        return m1;
    } else {
        return m2;
    }
}

float addSeam(float d, float part, float round) {
    part = max(part, -part - .0005);
    return smax(d, -part, round);
}

Model modelProto0(vec3 p) {
        
    float id = 1.;
    float d;
    vec3 n, n1;
    d = 1e12;
     
    if (useBounds) {
        float bounds;
        bounds = length(p) - 3.;
        bounds = max(bounds, -length(p - triV.a * 4.55) + 3.);
        bounds = min(bounds, fCapsule(p, vec3(0), triV.a * 3.1, .25));

        #ifdef SHOW_BOUNDS
        if (true) {
        #else
        if (bounds > .02) {
        #endif
            return Model(bounds, id);
        }
    }
    
    // Core

    float outer, inner, hole, spike;

    outer = length(p) - 1.;
    inner = -(length(p) - .9);

    float len = 3.3;
    float width = .35;
    float round = .4;
    float thickness = .01;
    
    n = bToCn(1,0,0);
    spike = fCone(p, width, n * 0., n * len) - thickness;
    outer = smin(outer, spike, round);
    
    n1 = reflect(n, triP.bc);
    spike = fCone(p, width, n1 * 0., n1 * len) - thickness;
    outer = smin(outer, spike, round);
    
    n1 = reflect(n1, triP.ca);
    spike = fCone(p, width, n1 * 0., n1 * len) - thickness;
    outer = smin(outer, spike, round);

    len = 1.5;
    width = .8;
    round = .2;
    
    n = bToCn(0,1,0);
    spike = fCone(p, width, n * 0., n * len) - thickness;
    outer = smin(outer, spike, round);
    
    n1 = reflect(n, triP.ca);
    spike = fCone(p, width, n1 * 0., n1 * len) - thickness;
    outer = smin(outer, spike, round);
    
    n1 = reflect(n1, triP.bc);
    spike = fCone(p, width, n1 * 0., n1 * len) - thickness;
    outer = smin(outer, spike, round);


    // Core seams

    float seam, spikeSeam, ringSeam, lineSeam, baseSeam, tipSeam;
    float outerSeamed;    
    float seamRound = .02;

    spikeSeam = length(p);

    baseSeam = spikeSeam - 1.15;
    tipSeam = spikeSeam - 1.25;
    seam = max(-baseSeam, tipSeam);
    outer = addSeam(outer, seam, seamRound);

    id += step(-baseSeam, 0.);
    id += step(-tipSeam, 0.);

    float split = fPlane(p, triP.bc, .3);
    float idSeam = mix(tipSeam, -1e12, step(-split, 0.));
    id += step(-idSeam, 0.);

    lineSeam = fPlane(p, triP.ca, 0.);
    outerSeamed = addSeam(outer, lineSeam, seamRound);
    outer = mix(outer, outerSeamed, step(seam, 0.));

    baseSeam = spikeSeam - 2.3;
    tipSeam = spikeSeam - 2.8;
    seam = max(-baseSeam, tipSeam);
    outer = addSeam(outer, seam, seamRound);

    id += step(-baseSeam, 0.);
    id += step(-tipSeam, 0.);

    Model core = Model(outer, id);
    

    // Shell
    
    outer = length(p) - 1.9;
    inner = -outer - .2;

    n = bToCn(0,0,1);
    spike = fCone(p, 1.3, n * 0., n * 2.35) - thickness;
    outer = smin(outer, spike, .2);    
    
    n = bToCn(1,0,0);
    hole = fCone(p, 1.55, n * 3., n * 0.);
    outer = smax(outer, -hole, .13);

    outer = smax(outer, inner, .1);


    // Shell seams

    spikeSeam = fPlane(p, triV.c, -2.);
    ringSeam = fPlane(p, triP.ab, .28);
    seam = max(ringSeam, spikeSeam);
    outer = addSeam(outer, seam, seamRound);

    id = 7.;
    id += step(ringSeam, 0.);
    id += step(-spikeSeam, 0.);
    
    Model shell = Model(outer, id);

    return opU(core, shell);
}

Model modelProto1(vec3 p) {
    
    if (isMasked) {
        // return Model(1., 1.);
    }

    float d;
    vec3 n, n1;
    d = 1e12;
    
    if (useBounds) {
        float bounds;
        bounds = dot(p, triV.c) - 1.3;
        bounds = max(bounds, dot(p, triV.a) - 1.33);
        bounds = max(bounds, -(length(p) - .9));

        #ifdef SHOW_BOUNDS
        if (true) {
        #else
        if (bounds > .02) {
        #endif
            return Model(bounds, 1.);
        }
    }
    
    float outer, inner, hole, spike;

    float len = 1.4;
    float width = .2;
    float round = .1;
    float thickness = .01;
    
    outer = length(p) - 1.;
    inner = -outer - .1;
    
    float blend = 1./3.;
    n = bToCn(blend, 1. - blend, .0);
    spike = fCone(p, width, n * 0., n * len) - thickness;
    outer = smin(outer, spike, round);
    
    n1 = reflect(n, triP.ca);
    spike = fCone(p, width, n1 * 0., n1 * len) - thickness;
    outer = smin(outer, spike, round);
    
    n1 = reflect(n, triP.bc);
    spike = fCone(p, width, n1 * 0., n1 * len) - thickness;
    outer = smin(outer, spike, round);
    
    len = 1.3;
    round = .05;
    
    n = bToCn(0,0,1);
    hole = fCone(p, .41, n * len, n * 0.);
    outer = smax(outer, -hole, round);
    
    n = bToCn(1,0,0);
    hole = fCone(p, .31, n * len, n * 0.);
    outer = smax(outer, -hole, round);
    
    d = smax(outer, inner, .05);
    
    return Model(d, 1.);
}    
    
Model modelProto2(vec3 p) {

    float d;
    vec3 n, n1;
    d = 1e12;

    if (useBounds) {
        float bounds;
        bounds = dot(p, triV.a) - 1.05;
        bounds = min(bounds, fCapsule(p, vec3(0), triV.c * 1.8, .06));

        #ifdef SHOW_BOUNDS
        if (true) {
        #else
        if (bounds > .02) {
        #endif
            return Model(bounds, 1.);
        }
   	}

    float outer, inner, hole, spike;

    float thickness = .01;
    
    outer = length(p) - 1.;
    inner = -outer - .08;
    
    n = bToCn(0,0,1);
    spike = fCone(p, .2, n * 0., n * 1.8) - thickness;
    outer = smin(outer, spike, .2);
 
    n = bToCn(0,1,0);
    spike = fCone(p, .5, n * 0., n * 1.2) - thickness;
    outer = smin(outer, spike, .1);

    n = bToCn(1,0,0);
    spike = fCone(p, .4, n * 0., n * 3.) - thickness;
    outer = smin(outer, spike, .2);
    
    
    float blend = 1.;
    n = bToCn(blend,1.-blend,0.);
    hole = fCone(p, 1.5, n * 5., n * 0.);
    outer = smax(outer, -hole, .05);

    d = smax(outer, inner, .05);
            
    return Model(d, 1.);
}



Model model7(vec3 p) {
    float bounds = dot(p, normalize(vec3(0,0,-1))) - 1.;
    
    pR(p.xy, -.2);
    pR(p.xz, -.45);
    pR(p.yz, .32);
    pIcosahedron(p);
    
    Model proto = modelProto0(p);

    if ( ! isMasked || ! useBounds || bounds > 0.2) {
      return proto;
    }

    vec3 point = geodesicPoint(p, 2.);
    float size = .02;
    if (isMasked) {
        size = .015;
    }
    float d = length(p - point * 2.75) - size;
    Model decal = Model(d, 20.);

    return opU(proto, decal);
}    

Model model8(vec3 p) {
    float bounds = dot(p, vec3(0,0,-1)) + 0.;
    pIcosahedron(p);    
    Model proto = modelProto1(p);

    if ( ! isMasked || ! useBounds || bounds > 0.2) {
     return proto;
    }

    vec3 point = geodesicPoint(p, 6.);
    float size = .02;
    if (isMasked) {
        size = .01;
    }
    float d = length(p - point * 1.8) - size;
    Model decal = Model(d, 20.);

    return opU(proto, decal);

}    
    
Model model9(vec3 p) {
	float bounds = dot(p, vec3(0,0,-1)) + 1.;
    pIcosahedron(p);    
	Model proto = modelProto2(p);

    if ( ! isMasked || ! useBounds || bounds > 0.2) {
      return proto;
    }

    vec3 point = geodesicPoint(p, 2.);
    float size = .02;
    if (isMasked) {
        size = .01;
    }
    float d = fCapsule(p, point * 3., point * 3.5, size);
    Model decal = Model(d, 20.);

    return opU(proto, decal);  
}


Model debugPlane(vec3 p) {

    //pTetrahedron(p);
    
    //return Model(length(p) - 5., 99.);

    float d = 10000.;
    //return Model(d, 99.);
    
    return Model(fPaper(p, vec3(0,.1,1), 0.), 101.);

    float a = fPaper(p, cross(pbc, pca), 0.);
    float b = fPaper(p, cross(pab, pbc), 0.);
    float c = fPaper(p, cross(pca, pab), 0.);
    
    d = min(d, a);
    d = min(d, b);
    d = min(d, c);
    
    d = max(d, length(p) - 1.4);
    d = max(d, -(length(p) - .0));
    return Model(d, 101.);
}

Model scene( vec3 p ){
    Model model, part;
    
    vec3 pp = p;
    
    vec3 p0 = vec3(-1., .7, 1.5);
    vec3 p1 = vec3(1.3, -2.5, -1.);
    vec3 p2 = vec3(2.7, .5, -2.5);
    
    float scale;

    #ifdef DEBUG_MODEL
        scale = 1.6;
        p /= scale;
        model= model7(p);
        model.dist *= scale;
        return model;
	#endif

    //pR(p.xz, time*5.);
        
    p = pp;
    scale = .9;
    p -= p0;
    p /= scale;
   	part = model7(p);
    part.dist *= scale;
	model = part;
    
    p = pp;
    scale = .95;
    p -= p1;
    p /= scale;
    part = model8(p);
    part.dist *= scale;
	model = opU(model, part);
    
    p = pp;
    scale = .6;
    p-= p2;
    p /= scale;
   	part = model9(p);
    part.dist *= scale;    
    model = opU(model, part);
    
    return model;
}

Model map(vec3 p) {
    //pR(p.xz, time + PI * .4);
    //pR(p.yz, PI * .5);

    //p = mod(p, 2.);
    //float d = length(p - vec3(1.)) - .4;
    //return Model(d, 1.);

    #ifdef SHOW_MODELS
        Model model = scene(p); 
    #else
        Model model = Model(1e12, 0.);
    #endif

    #ifdef DEBUG
        model = opU(model, debugPlane(p));
    #endif
    
    return model;
}


// --------------------------------------------------------
// Gamma
// https://www.shadertoy.com/view/Xds3zN
// --------------------------------------------------------

const float GAMMA = 2.2;

vec3 gamma(vec3 color, float g) {
    return pow(color, vec3(g));
}

vec3 linearToScreen(vec3 linearRGB) {
    return gamma(linearRGB, 1.0 / GAMMA);
}

vec3 screenToLinear(vec3 screenRGB) {
    return gamma(screenRGB, GAMMA);
}


// --------------------------------------------------------
// Ray Marching
// Adapted from: https://www.shadertoy.com/view/Xl2XWt
// --------------------------------------------------------

const float MAX_TRACE_DISTANCE = 30.; // max trace distance
const float INTERSECTION_PRECISION = .001; // precision of the intersection
const int NUM_OF_TRACE_STEPS = 100;
const float FUDGE_FACTOR = .8; // Default is 1, reduce to fix overshoots

struct CastRay {
    vec3 origin;
    vec3 direction;
};

struct Ray {
    vec3 origin;
    vec3 direction;
    float len;
};

struct Hit {
    Ray ray;
    Model model;
    vec3 pos;
    bool isBackground;
    vec3 normal;
    vec3 color;
    float steps;
};

vec3 calcNormal( in vec3 pos ){
    vec3 eps = vec3( 0.001, 0.0, 0.0 );
    vec3 nor = vec3(
        map(pos+eps.xyy).dist - map(pos-eps.xyy).dist,
        map(pos+eps.yxy).dist - map(pos-eps.yxy).dist,
        map(pos+eps.yyx).dist - map(pos-eps.yyx).dist );
    return normalize(nor);
}

Hit raymarch(CastRay castRay){
	float steps = 0.;
    float currentDist = INTERSECTION_PRECISION * 2.0;
    Model model;

    Ray ray = Ray(castRay.origin, castRay.direction, 0.);

    for( int i=0; i< NUM_OF_TRACE_STEPS ; i++ ){
        if (currentDist < INTERSECTION_PRECISION || ray.len > MAX_TRACE_DISTANCE) {
            break;
        }
        model = map(ray.origin + ray.direction * ray.len);
        currentDist = model.dist;
        ray.len += currentDist * FUDGE_FACTOR;
        steps += 1.;
    }

    bool isBackground = false;
    vec3 pos = vec3(0);
    vec3 normal = vec3(0);
    vec3 color = vec3(0);

    if (ray.len > MAX_TRACE_DISTANCE) {
        isBackground = true;
    } else {
        pos = ray.origin + ray.direction * ray.len;
        normal = calcNormal(pos);
    }

    return Hit(ray, model, pos, isBackground, normal, color, steps);
}


// --------------------------------------------------------
// Rendering
// --------------------------------------------------------

// LIGHTING

float softshadow( in vec3 ro, in vec3 rd, in float mint, in float tmax )
{
    #ifndef SHADOWS
    	return 1.;
   	#endif
    
    float res = 1.0;
    float t = mint;
    for( int i=0; i<16; i++ )
    {
        float h = map( ro + rd*t ).dist;
        res = min( res, 8.0*h/t );
        t += clamp( h, 0.02, 0.10 );
        if( h<0.001 || t>tmax ) break;
    }
    return clamp( res, 0.0, 1.0 );
}


float calcAO( in vec3 pos, in vec3 nor )
{
    #ifndef SHADOWS
    	return 1.;
   	#endif

    float occ = 0.0;
    float sca = 1.0;
    for( int i=0; i<5; i++ )
    {
        float hr = 0.01 + 0.12*float(i)/4.0;
        vec3 aopos =  nor * hr + pos;
        float dd = map( aopos ).dist;
        occ += -(dd-hr)*sca;
        sca *= 0.95;
    }
    return clamp( 1.0 - 3.0*occ, 0.0, 1.0 );    
}

    
bool backMask(vec2 uv) {
    pR(uv, PI/2.);
    pModPolar(uv, 6.);
        
    float hex = dot(uv, vec2(1,0)) - .58;

    return hex > 0.;
}

    

void shadeModel(inout Hit hit) {
    vec3 pos = hit.pos;
    vec3 nor = hit.normal;
    vec3 rd = hit.ray.direction;
    vec3 albedo;
    
    vec3 col1 = vec3(.65, .65, .75);
    vec3 col2 = vec3(.9, .5, .8);
    vec3 col3 = vec3(.8);

    //col2 = mix(col2, vec3(.74, .5, .99), .5);
    col2 = vec3(.74, .5, .99);
    //col2.xz = iMouse.xy / iResolution.xy;

    float id = hit.model.id;

    albedo = vec3(0);

    if (id == 1.) { albedo = col3; }
    if (id == 2.) { albedo = col1; }
    if (id == 3.) { albedo = col3; }

    if (id == 4.) { albedo = col1; }
    if (id == 5.) { albedo = col2; }
    if (id == 6.) { albedo = col3; }

    if (id == 7.) { albedo = col1; }
    if (id == 8.) { albedo = col2; }
    if (id == 9.) { albedo = col3; } 

    //albedo = spectrum((hit.model.id - 1.) / 9. - iGlobalTime / 2.);
    
    useBounds = false;
    
    vec3 lightPos = vec3(0,0,-1);
    vec3 backLightPos = normalize(vec3(0,-.3,1));
    vec3 ambientPos = vec3(0,1,0);

    mat3 m;
    //m = sphericalMatrix((iMouse.xy / iResolution.xy - .5) * 8.);
    vec2 mouseSetting = vec2(0.45607896335673687, 0.8963768106439839);
    m = sphericalMatrix((mouseSetting - .5) * 8.);
    lightPos *= m;
    backLightPos *= m;
    
    // lighitng        
    float occ = calcAO( pos, nor );
    //occ = 1.;
    vec3  lig = lightPos;
    float amb = clamp((dot(nor, ambientPos) + 1.) / 2., 0., 1.);
    float dif = clamp( dot( nor, lig ), 0.0, 1.0 );
    float bac = pow(clamp(dot(nor, backLightPos), 0., 1.), 1.5);
    float fre = pow( clamp(1.0+dot(nor,rd),0.0,1.0), 2.0 );
    
    dif *= mix(softshadow(pos, lig, 0.02, 2.5), 1., 0.3);

    vec3 lin = vec3(0.0);
    lin += 1.2 * dif * vec3(.95, .8, .8);
    lin += 1.1 * amb * vec3(.4, .6, .9) * occ;
    lin += 0.3 * bac * vec3(.25, .25, .25) * occ;
    lin += 0.2 * fre * vec3(1.,1., 1.) * occ;
    vec3 col = albedo*lin;

    float fog = smoothstep(hit.ray.len, 6., 9.);
    //fog *= .5;
    col = mix(col, vec3(.7,.4,1.), fog * .75);
    col *= 1. + fog * .6;
    //col = vec3(fog);

    //col = vec3(occ);

    hit.color = col;
}
    
void shadeSurface(inout Hit hit){

    
    #ifdef SHOW_STEPS
    	hit.color = vec3(hit.steps / 40.);
    	return;
    #endif
    
    vec3 background = vec3(1.);
    
    if (hit.isBackground) {
        hit.color = background;
        return;
    }
    
    if (hit.model.id > 100.) {
        float dist = scene(hit.pos).dist;
        hit.color = vec3(mod(dist * 10., 1.));
        return;
    }
    
    #ifdef DEBUG
        hit.color = hit.normal * .5 + .5;
        return;
    #endif
    
    if (hit.model.id == 20.) {
        hit.color = vec3(.05,.02,.3);
        return;
    }
    
    shadeModel(hit);
}


vec4 render(Hit hit) {

    shadeSurface(hit);

    return vec4(hit.color, hit.ray.len);
}


// --------------------------------------------------------
// Camera
// https://www.shadertoy.com/view/Xl2XWt
// --------------------------------------------------------

mat3 calcLookAtMatrix( in vec3 ro, in vec3 ta, in float roll )
{
    vec3 ww = normalize( ta - ro );
    vec3 uu = normalize( cross(ww,vec3(sin(roll),cos(roll),0.0) ) );
    vec3 vv = normalize( cross(uu,ww));
    return mat3( uu, vv, ww );
}

void doCamera(out vec3 camPos, out vec3 camTar, out float camRoll, in vec2 mouse) {
    float dist = 10.5;
    camRoll = 0.;
    camTar = vec3(0,0,0);
    camPos = vec3(0,0,dist);
    camPos += camTar;
    #ifdef CAMERA_CONTROL
        camPos *= sphericalMatrix(mouse * 5.);
    #endif
}

#pragma glslify: starField = require(./shaders/starfield.glsl)
#pragma glslify: nebulaField = require(./shaders/nebula.glsl)


void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
    initPolyhedron(5);
    
    time = iGlobalTime;

    vec2 p = (-iResolution.xy + 2.0*fragCoord.xy)/iResolution.y;
    #ifdef SHOW_ZOOM
    	p *= .7;
    #endif
    vec2 m = iMouse.xy / iResolution.xy - .5;
    mousee = m;

    vec3 camPos, camTar;
    float camRoll;
    doCamera(camPos, camTar, camRoll, m);
    mat3 camMat = calcLookAtMatrix( camPos, camTar, camRoll );  // 0.0 is the camera roll
    vec3 rd = normalize( camMat * vec3(p.xy,2.0) ); // 2.0 is the lens length
    CastRay ray = CastRay(camPos, rd);

    isMasked = backMask(p);
    //isMasked = false;
    useBounds = true;
    Hit hit = raymarch(ray);

    vec4 color = render(hit);

    #ifdef SHOW_SPACE
    	if ( ! isMasked && hit.isBackground) {

            vec2 sp = p * 10. + vec2(-.2,1.);
            vec3 soffset = vec3(7.9,3.001,0.15);
            vec3 stars = starField(sp, soffset) * .01;
            vec3 field = nebulaField(sp);
            vec3 bg = (field + stars) * .9 + .2;
            bg = clamp(bg, 0., 1.); 

            color = vec4(screenToLinear(bg), hit.ray.len);

        	//color = vec4(pow(spaceCol * 1.2, vec3(1.5)), hit.ray.len);

        	#ifdef SHOW_FOG
    			vec4 sliderVal = vec4(0.5,0.4,0.16,0.7);
    			sliderVal = vec4(0.5,0.7,0.2,0.9);

                //0.4848822844959103
            	//0.553018368604615
                //camPos *= sphericalMatrix(m * 8.);
                //camMat = calcLookAtMatrix( camPos, camTar, camRoll );  // 0.0 is the camera roll
                //rd = normalize( camMat * vec3(p.xy,2.0) ); // 2.0 is the lens length
                //ray = CastRay(camPos, rd);

    			color = renderSuperstructure(ray.origin, ray.direction, sliderVal, color);
        	#endif

            color.rgb += pow(stars * .4, vec3(2.));
        }
    #endif

    #ifndef DEBUG
      color.rgb = linearToScreen(color.rgb);
    #endif


    fragColor = color;
}



