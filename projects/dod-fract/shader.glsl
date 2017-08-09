precision highp float;

uniform vec2 iResolution;
uniform vec2 iOffset;
uniform float iGlobalTime;
uniform vec4 iMouse;
uniform sampler2D iChannel0;


void mainImage(out vec4 a, in vec2 b);

void main() {
    mainImage(gl_FragColor, gl_FragCoord.xy + iOffset.xy);
}

#ifdef GL_ES
precision mediump float;
#endif


/* SHADERTOY FROM HERE */


vec2 mousee;

#define MODEL_ROTATION vec2(.5, .5)
#define CAMERA_ROTATION vec2(.5, .5)

// 0: Defaults
// 1: Model
// 2: Camera
#define MOUSE_CONTROL 2

//#define DEBUG

float time;

#define PI 3.14159265359
#define HALF_PI 1.5707963267948966
#define TAU 6.28318530718
#define PHI 1.618033988749895


// --------------------------------------------------------
// Rotation controls
// --------------------------------------------------------

mat3 sphericalMatrix(float theta, float phi) {
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

mat3 mouseRotation(bool enable, vec2 xy) {
    if (enable) {
        vec2 mouse = mousee.xy / iResolution.xy;

        if (mouse.x != 0. && mouse.y != 0.) {
            xy.x = mouse.x;
            xy.y = mouse.y;
        }
    }
    float rx, ry;

    xy.x -= .5;
    //xy *= 2.;

    rx = (xy.y + .5) * PI;
    ry = (-xy.x) * 2. * PI;

    return sphericalMatrix(rx, ry);
}

mat3 modelRotation() {
    mat3 m = mouseRotation(MOUSE_CONTROL==1, MODEL_ROTATION);
    return m;
}

mat3 cameraRotation() {
    mat3 m = mouseRotation(MOUSE_CONTROL==2, CAMERA_ROTATION);
    return m;
}


// --------------------------------------------------------
// HG_SDF
// --------------------------------------------------------

#define saturate(x) clamp(x, 0., 1.)

void pR(inout vec2 p, float a) {
    p = cos(a)*p + sin(a)*vec2(p.y, -p.x);
}

float smax(float a, float b, float r) {
    float m = max(a, b);
    if ((-a < r) && (-b < r)) {
        return max(m, -(r - sqrt((r+a)*(r+a) + (r+b)*(r+b))));
    } else {
        return m;
    }
}

float smin(float a, float b, float r) {
    float m = min(a, b);
    if ((a < r) && (b < r) ) {
        return min(m, r - sqrt((r-a)*(r-a) + (r-b)*(r-b)));
    } else {
     return m;
    }
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

float fCapsule(vec3 p, vec3 a, vec3 b, float r, float sep) {
    vec3 m = mix(b, a, .5);
    float s1 = fLineSegment(p, a, mix(m, a, sep));
    float s2 = fLineSegment(p, b, mix(m, b, sep));
    return min(s1 - r, s2 - r);
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
    
    
vec3 nc,pab,pbc,pca;
Tri triV;
TriPlanes triP;

int Type = 5;

void init() {//setup folding planes and vertex
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
}


void fold(inout vec3 p) {
    for(int i=0;i<5 /*Type*/;i++){
        p.xy = abs(p.xy);
        p -= 2. * min(0., dot(p,nc)) * nc;
    }
}

vec3 icosahedronVertex(vec3 p) {
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


// Nearest dodecahedron vertex
vec3 dodecahedronVertex(vec3 p) {
    vec3 sp, v1, v2, v3, v4, result, plane;
    sp = sign(p);
    v1 = sp;
    v2 = vec3(0, 1, PHI + 1.) * sp;
    v3 = vec3(1, PHI + 1., 0) * sp;
    v4 = vec3(PHI + 1., 0, 1) * sp;

    plane = vec3(-1. - PHI, -1, PHI) * sp;
    result = mix(v1, v2, max(sign(dot(p, plane)), 0.));
    
    plane = vec3(-1, PHI, -1. - PHI) * sp;
    result = mix(result, v3, max(sign(dot(p, plane)), 0.));
    
    plane = vec3(PHI, -1. - PHI, -1) * sp;
    result = mix(result, v4, max(sign(dot(p, plane)), 0.));
    
    return normalize(result);
}

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

vec3 pal1(float n) {
    return pal(
        n,
        vec3(0.640,0.469,0.506),
        vec3(0.470,0.423,0.257),
        vec3(0.340,0.136,0.505),
        vec3(0.114,0.345,0.743)
    );
}

vec3 pal2(float n) {
    return pal(
        n,
        vec3(0.840,0.467,0.661),
        vec3(0.485,0.269,0.199),
        vec3(0.436,0.530,0.456),
        vec3(0.114,0.345,0.743)
    );
}

vec3 pal4(float n) {
    return pal(
        n,
        vec3(0.001,0.780,0.063),
        vec3(0.765,0.630,0.736),
        vec3(0.420,0.416,0.141),
        vec3(0.560,0.131,0.007)
    );
}

// --------------------------------------------------------
// Modelling
// --------------------------------------------------------

struct Model {
    float dist;
    float id;
    vec3 uv;
    bool isBound;
};

Model makeBounds(float dist) {
    return Model(dist, 0., vec3(0), true);
}

    
// checks to see which intersection is closer
Model opU( Model m1, Model m2 ){
    if (m1.dist < m2.dist) {
        return m1;
    } else {
        return m2;
    }
}


float gain(float x, float P) {
    if (x > 0.5)
        return 1.0 - 0.5*pow(2.0-2.0*x, P);
    else
        return 0.5*pow(2.0*x, P);
}

float gainIn(float x, float P) {
    return gain(x * .5, P) * 2.;
}

float gainOut(float x, float P) {
    return 1. - gain(.5 - x * .5, P) * 2.;
}

float sinstep(float start, float end, float x) {
    float len = end -start;
    x = (x - start) * (1./len);
    x = clamp(x, 0., 1.);
    return sin(x * PI - PI * .5) * .5 + .5;
}

float sinstep(float x) {
    return sinstep(0., 1., x);
}

float squareSine(float x, float e) {
    x = mod(x, PI * 2.);
    float period = x / mod((PI / 2.), 4.);
    float a = pow(abs(period - 3.), e) - 1.;
    float b = -pow(abs(period - 1.), e) + 1.;
    return period > 2. ? a : b;
}

float squarestep(float start, float end, float x, float e) {
    float len = end -start;
    x = (x - start) * (1./len);
    x = clamp(x, 0., 1.);
    return squareSine(x * PI - PI * .5, e) * .5 + .5;
}

float squarestep(float x, float e) {
    return squarestep(0., 1., x, e);
}


float hardstep(float a, float b, float t) {
    float s = 1. / (b - a);
    return clamp((t - a) * s, 0., 1.);
}

float kink(float x, vec2 p, float e1, float e2) {
    float a = (1. - pow(1. - x / p.x, e1)) * p.y;
    float b = pow((x - p.x) / (1. - p.x), e2) * (1. - p.y) + p.y;
    return mix(a, b, step(p.x, x));
}



float stepScale = .275;
float stepMove = 2.;
float stepDuration = 2.;
float loopDuration;
float ballSize = 1.5;
float stepSpeed = .5;

// #define SHOW_ANIMATION
// #define SHOW_PATHS
// #define SHOW_BOUNDS;
// #define SHOW_ITERATIONS

#define USE_BOUNDS;
#define USE_OUTER_BOUNDS;
// #define BOUNCE_INNER;

#ifdef SHOW_ANIMATION
    const float initialStep = 0.;
    const float MODEL_STEPS = 4.;
#else
    const float initialStep = 2.;
    const float MODEL_STEPS = 3.;
#endif



float tweakAnim(float x) {
    return mix(x, kink(x, vec2(.4), 2., .8), .5);
}

float makeAnimStep(float t, float stepIndex) {
    float x = t;
    x -= 1. / MODEL_STEPS * stepIndex;
    x = mod(x, 1.);
    x *= MODEL_STEPS;
    x *= stepSpeed;
    x = tweakAnim(x);
    return x;
}

float makeAnimStep(float t, float stepIndex, float delay) {
    return makeAnimStep(t - delay, stepIndex);
}

float makeAnimStepNomod(float t, float stepIndex) {
    float x = t;
    x -= 1. / MODEL_STEPS * stepIndex;
    x *= MODEL_STEPS;
    x *= stepSpeed;
    x = tweakAnim(x);
    return x;
}

float makeAnimStepNomod(float t, float stepIndex, float delay) {
    return makeAnimStepNomod(t - delay, stepIndex);
}

float moveAnim(float x) {
    float a = 1.;
    float h = 1.;
    float blend = x;
    blend = squarestep(-a, a, blend, 2.) * h * 2. - h;
    blend = squarestep(blend, 1.5);
    return blend;
}

float scaleAnim(float x) {
    x /= stepSpeed;
    float a = 1.;
    float h = 1.;
    float blend = x;
    blend = hardstep(0., .85, x);
    blend = squarestep(-a, a, blend, 1.2) * h * 2. - h;
    blend = squarestep(blend, 1.2);
    return blend;
}

float wobble2(float x, float freq) {
    float w = sin(x * PI * 2. * freq - PI * .5) * .5 + .5;
    w *= sin(x * PI + PI * .5) * .5 + .5;
    return w;
}

float wobble(float x) {
    return wobble2(x, 3.5);
}

float wobbleScaleAnim(float x) {
    float blend = scaleAnim(x);
    x /= stepSpeed;
    blend -= wobble(hardstep(.6, 2.2, x)) * .1;
    return blend;
}

const float ANIM_CAM_START = .96;

float animCamRotateA(float x) {
    return mix(gainOut(x, 3.), x, .9);
}

float animCamRotate(float x) {
    float o = ANIM_CAM_START;
    return animCamRotateA(mod(x - o, 1.)) + (1. - animCamRotateA(1. - o)) + floor(x - o);    
}

float circleEaseIn(float radius, float slope, float x) {
    float iSlope = 1. - slope;
    float scale = radius / length(vec2(iSlope, 1.));
    float u = (iSlope * -scale + radius) * iSlope + slope;
    float line = (x - slope) / iSlope;
    float uu = u - scale;
    float circle = -sqrt(radius * radius - (x - uu) * (x - uu)) + radius;
    float ramp = mix(0., circle, step(uu, x));
    return mix(max(ramp, 0.), line, step(u, x));
}

float circleEaseOut(float radius, float slope, float x) {
    return 1. - circleEaseIn(radius, slope, 1. - x);
}

float animTimeA(float x) {
    return circleEaseOut(.25, .3, x);
}

float animTime(float x) {
    float o = .78;
    return animTimeA(mod(x - o, 1.)) + (1. - animTimeA(1. - o)) + floor(x - o);
}

float modelScale;

float boundsForStep(vec3 p, float move, float size, float scale) {
    float overfit = .3;
    p /= scale;
    float d = (length(p) - move - size - overfit);
    d *= scale;
    return d;
}

struct ModelSpec {
    float move;
    float size;
    float sizeCore;
    float bounds;
};

ModelSpec specForStep(vec3 p, float x, float scale) {
    float move = moveAnim(x) * stepMove;
    float sizeScale = mix(1., stepScale, wobbleScaleAnim(x));
    float sizeScaleCore = mix(1., stepScale, scaleAnim(x));
    float size = ballSize * sizeScale;
    float sizeCore = ballSize * sizeScaleCore;
    float bounds = boundsForStep(p, move, size, scale);
    return ModelSpec(move, size, sizeCore, bounds);
}

float modelIterations;
bool debugSwitch = false;
float boundsThreshold;

Model makeModel(vec3 p, float x, float scale, float level) {
    float d, part;

    ModelSpec spec = specForStep(p, x, scale);
    
    float move = spec.move;
    float size = spec.size;
    float sizeCore = spec.sizeCore;

    #ifdef SHOW_BOUNDS
        return makeBounds(spec.bounds);
    #endif
    #ifdef USE_BOUNDS
        if (spec.bounds > boundsThreshold) {
            return makeBounds(spec.bounds);
        }
    #endif
    
    p /= scale;

    fold(p);

    // Setup smoothing

    float rBlend = hardstep(.1, .4, x);
    //rBlend -= hardstep(.4, .5, x);
    rBlend = smoothstep(0., 1., rBlend);
    float r = mix(0., .4, rBlend);
    // r = 0.;

    // Center ball

    vec3 vA = vec3(0);

    #ifdef BOUNCE_INNER
        part = length(p - vA) - size;
    #else
        part = length(p - vA) - sizeCore;
    #endif
    d = part;

    // level += min(
    //     step(.35, x),
    //     1.- smoothstep(0., move - size * 2., part)
    // );

    // level += min(
    //     step(.35, x),
    //     1.- smoothstep(0., move - size * 2., part)
    // );

    float la = 0.;
    float lb = 1.-hardstep(.1, 20., part);
    float lc = step(part, (move - size * 1.5) * .5);
    
    // level += mix(mix(la, lb, step(.3, x)), lc, step(.4, x));
    level += mix(0., lc, step(.3, x));

    // Setup ball

    vec3 vB = triV.a * move;

    // Setup bridge

    float cr = 0.04;
    float rSep = hardstep(.4, .5, x);
    // sep = squareStep(sep, 20.);
    float sep = mix(0., 1., rSep);

    // Ball and bridge

    d = smin(d, fCapsule(p, vA, vB, cr, sep), r);
    d = smin(d, length(p - vB) - size, r);
    
    // First reflection

    vec3 rPlane = triP.bc;
    p = reflect(p, rPlane);
    
    d = smin(d, fCapsule(p, vA, vB, cr, sep), r);
    d = smin(d, fCapsule(p, vB, reflect(vB, rPlane), cr, sep), r);
    d = smin(d, length(p - vB) - size, r);

    // Second reflection

    vec3 rPlane2 = reflect(triP.ca, rPlane);
    p = reflect(p, rPlane2);

    d = smin(d, fCapsule(p, vA, vB, cr, sep), r);
    d = smin(d, fCapsule(p, vB, reflect(vB, rPlane2), cr, sep), r);
    d = smin(d, fCapsule(p, vB, reflect(reflect(vB, rPlane), rPlane2), cr, sep), r);
    d = smin(d, length(p - vB) - size, r);

    d *= scale;

    modelIterations += 1.;

    return Model(d, level, vec3(0.), false);
}


float hash( const in vec3 p ) {
    return fract(sin(dot(p,vec3(127.1,311.7,758.5453123)))*43758.5453123);
}


Model subDModel(vec3 p) {

    float stepIndex = -initialStep;
    float scale = 1.;
    
    vec3 iv;
    float delay = 0.;

    float innerBounds = 1e12;
    float bounds = 1e12;
    bool hasBounds = false;
    float boundsCandidate;

    float threshold = .3;

    float midSizeScale = mix(1., stepScale, scaleAnim(stepSpeed));

    float prevStepIndex, x, move, sizeScale, size;

    float css = 1.;

    #ifndef SHOW_ANIMATION
        float time = animTime(time);
    #endif

    float stepX;

    float level= 0.;

    for (float i = 1. - initialStep; i < MODEL_STEPS; i++) {

        stepX = makeAnimStepNomod(time, i, delay);

        if (stepX >= 0. && ! hasBounds) {
            // modelIterations += 1.;

            // level -= 1.;
            stepIndex = i;
            prevStepIndex = stepIndex - 1.;

            if (stepIndex == 0.) {
                css = 1.;
            } else {
                css *= midSizeScale;
            }

            scale = pow(midSizeScale, prevStepIndex);


            x = makeAnimStepNomod(time, prevStepIndex, delay);
            ModelSpec spec = specForStep(p, x, scale);
            move = spec.move;

            p /= scale;


            #ifdef BOUNCE_INNER
                css = pow(midSizeScale, prevStepIndex) * mix(1., stepScale, wobbleScaleAnim(x));
            #else
                css = pow(midSizeScale, prevStepIndex) * mix(1., stepScale, scaleAnim(x));
            #endif

            vec3 pp = p;

            innerBounds = (length(p) - move * .55) * scale;
            if (innerBounds > 0.) {
                fold(p);
                p -= triV.a * move;
                #ifndef BOUNCE_INNER
                    css = pow(midSizeScale, prevStepIndex) * mix(1., stepScale, wobbleScaleAnim(x));
                #endif
            } else {
                level += 1.;
            }
            p *= scale;
            
            if (innerBounds > 0.) {
                iv = icosahedronVertex(pp);
                // delay += .6;
                delay += hash(iv + spectrum(mod(level, 3.) / 3.)) * .6;
                // delay += hash(vec3(mod(level, 3.) / 3. + 1.)) * .6;
            }

            #ifdef USE_OUTER_BOUNDS
                
                boundsCandidate = spec.bounds;

                if (boundsCandidate > -.0) {
                    bounds = min(bounds, boundsCandidate);
                    hasBounds = true;
                }
            #endif
        }
    }

    // threshold *= scale;

    #ifdef SHOW_BOUNDS
        if (hasBounds) {
            return makeBounds(bounds);
        }
    #endif
    #ifdef USE_OUTER_BOUNDS
        if (bounds > boundsThreshold && hasBounds) {
            return makeBounds(bounds);
        }
    #endif

    float mx = makeAnimStepNomod(time, stepIndex, delay);
    Model model = makeModel(p, mx, css, level);
    
    // innerBounds -= threshold;
    innerBounds -= .1;

    #ifdef SHOW_BOUNDS
        return makeBounds(min(model.dist, innerBounds));
    #endif
    #ifdef USE_OUTER_BOUNDS
        if (innerBounds > boundsThreshold) {
            // return makeBounds(innerBounds);
            model.dist = min(model.dist, innerBounds);
        }
    #endif

    return model;
}

Model map( vec3 p ){
    mat3 m = modelRotation();
    p /= modelScale;
    boundsThreshold = 1. / modelScale;
    Model model = subDModel(p);
    model.dist *= modelScale;
    return model;
}


vec3 camPos;
float camDist;
vec3 camTar;


float camZoomInOutA(float x) {
    float back = -.1;
    float p = .5;
    
    float zIn, zOut;

    zOut = hardstep(0., p + .1, x);
    zOut = gainOut(zOut, 5.);
    
    zIn = hardstep(p - .3, 1., x);
    zIn = gain(zIn, 1.5);
    zIn = gainIn(zIn, 2.5);

    return zOut * back + zIn * (1.- back);
}

float camZoomInOut(float x) {
    float o = ANIM_CAM_START;
    float y = camZoomInOutA(mod(x - o, 1.)) + (1. - camZoomInOutA(1. - o)) + floor(x - o);
    y -= .2;
    return y;
}

float scaleForStep(float step) {
    return pow(1./stepScale, step);
}

void doCamera(out vec3 camPos, out vec3 camTar, out vec3 camUp, in vec2 mouse) {
    float x = time;

    camDist = 3. / stepScale;
    camDist = 8.;

    float camZoom = camZoomInOut(x);
    modelScale = scaleForStep(camZoom * 3.);

    camUp = vec3(0,-1,0);
    camTar = vec3(0.);
    camPos = vec3(0,0,camDist);
        
    pR(camPos.xz, animCamRotate(x) * PI * 2.);

    #ifdef SHOW_ANIMATION
        camDist = 6.5;
        modelScale = 6.;
        
        camDist /= 2.;

        // if (debugSwitch) {
        //     modelScale = scaleForStep(-2.5);
        //     camDist *= modelScale;
        // }

        camPos = vec3(0,0,camDist);
    #endif

    camPos *= cameraRotation();
}



// --------------------------------------------------------
// Ray Marching
// Adapted from: https://www.shadertoy.com/view/Xl2XWt
// --------------------------------------------------------

const float MAX_TRACE_DISTANCE = 30.; // max trace distance
const float INTERSECTION_PRECISION = .001; // precision of the intersection
const int NUM_OF_TRACE_STEPS = 100;
const float FUDGE_FACTOR = 1.; // Default is 1, reduce to fix overshoots

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

    float currentDist = INTERSECTION_PRECISION * 2.0;
    bool isBound = false;
    int iterations = 0;
    modelIterations = 0.;
    Model model;

    Ray ray = Ray(castRay.origin, castRay.direction, 0.);

    for( int i=0; i< NUM_OF_TRACE_STEPS ; i++ ){
        if (currentDist < INTERSECTION_PRECISION || ray.len > MAX_TRACE_DISTANCE) {
            break;
        }
        iterations += 1;
        model = map(ray.origin + ray.direction * ray.len);
        // isBound = model.isBound;
        currentDist = model.dist;
        ray.len += currentDist * FUDGE_FACTOR;
    }

    bool isBackground = false;
    vec3 pos = vec3(0);
    vec3 normal = vec3(0);
    vec3 color = vec3(iterations);

    if (ray.len > MAX_TRACE_DISTANCE) {
        isBackground = true;
    } else {
        pos = ray.origin + ray.direction * ray.len;
        normal = calcNormal(pos);
    }

    return Hit(ray, model, pos, isBackground, normal, color);
}


// --------------------------------------------------------
// Rendering
// --------------------------------------------------------

void shadeSurface(inout Hit hit){

    vec3 background = vec3(.1)* vec3(.5,0,1);

    background = pal1(1.) * .6;

    #ifndef SHOW_ITERATIONS
        if (hit.isBackground) {
            hit.color = background;
            return;
        }
    #endif

    //hit.normal += sin(hit.model.uv * .4) * .4;
    //hit.normal = normalize(hit.normal);

    vec3 light = normalize(vec3(.5,1,0));
    vec3 diffuse = vec3(dot(hit.normal, light) * .5 + .5);
    diffuse = mix(diffuse, vec3(1), .1);
    
    vec3 colA = vec3(.1,.75,.75) * 1.5;
    
    //diffuse *= hit.model.uv;
    diffuse = sin(diffuse);
    diffuse *= 1.3;
    
    float fog = clamp((hit.ray.len - 5.) * .5, 0., 1.);
    fog = mix(0., 1., length(camTar - hit.pos) / pow(camDist, 1.5)) * 1.;

    fog = abs(length(camTar - hit.pos)) / (camDist * 1.75);

    fog = clamp(fog, 0., 1.);


    //*
    diffuse = vec3(.3) * vec3(.9, .3, .8);
    #ifdef SHOW_ANIMATION
        diffuse = vec3(hit.model.id);
    #endif
    vec3 highlight = vec3(1.2) * vec3(.8,.5,1.2);
    float glow = 1. - dot(normalize(camPos), hit.normal);
    glow += .5 * (1.-dot(hit.normal, normalize(hit.pos)));
    glow *= .5;
    glow = squarestep(glow, 2.);
    diffuse = pal1(clamp(fog*2. - glow * .1 + .2, 0., 1.));
    diffuse = mix(diffuse, diffuse * 3., glow);
    
    // diffuse = hit.color;
    float level = hit.model.id;
    diffuse = spectrum(level / 3. + .1);
    // diffuse = vec3(mod(level, 3.) / 3.);

    diffuse = mix(diffuse * .75, diffuse * 1.5, glow);

    diffuse = mix(diffuse, background, fog);

    // diffuse = vec3(glow);
    //*/
    // diffuse = vec3(length(diffuse * .5));
    #ifdef SHOW_ITERATIONS
        hit.color = spectrum(hit.color.x / 50.);
        // hit.color = spectrum(modelIterations / 100.);
    #else
        hit.color = diffuse;
    #endif
    // hit.color = hit.normal * .5 + .5;
    // hit.color = spectrum(hit.ray.len / 10.);
    //hit.color = hit.model.uv;
}


vec3 render(Hit hit){

#ifdef DEBUG
    return hit.normal * .5 + .5;
#endif

    shadeSurface(hit);

    return hit.color;
}


// --------------------------------------------------------
// Camera
// https://www.shadertoy.com/view/Xl2XWt
// --------------------------------------------------------

mat3 calcLookAtMatrix( in vec3 ro, in vec3 ta, in vec3 up )
{
    vec3 ww = normalize( ta - ro );
    vec3 uu = normalize( cross(ww,up));
    vec3 vv = normalize( cross(uu,ww));
    return mat3( uu, vv, ww );
}



// --------------------------------------------------------
// Gamma
// https://www.shadertoy.com/view/Xds3zN
// --------------------------------------------------------

const float GAMMA = 1.;

vec3 gamma(vec3 color, float g) {
    return pow(color, vec3(g));
}

vec3 linearToScreen(vec3 linearRGB) {
    return gamma(linearRGB, 1.0 / GAMMA);
}


float plot(float height, vec2 p, float y){
    float thick = .005;
    y *= height;
    return (
        smoothstep( y - thick, y, p.y) - 
        smoothstep( y, y + thick, p.y)
    );
}

vec3 hlCol(vec3 color, float highlight) {
    return mix(color, vec3(1), highlight);
}

float plotFade(float x) {
    return smoothstep(1., .5, x);
}

void renderPaths(inout vec3 color, vec2 fragCoord) {
    vec2 p = fragCoord.xy / iResolution.xy;
    p.y -= .02;
    float height = 1./4.;
    float focus = .25;

    if (p.y > height + .02) {
        return;
    }

    // p *= 2.;
    // p -= .5;

    float x = p.x;

    // x = mod(x - .5, 1.);
    color = vec3(0);

    // x *= focus;
    // x += time;
    // x -= .5 * focus;

    float hp = time - x;
    float hl = smoothstep(.1, .0, hp) - smoothstep(.0, -.005, hp);

    
    color += plot(height, p, animCamRotate(x)) * hlCol(vec3(0,1,1), hl);
    color += plot(height, p, camZoomInOut(x)) * hlCol(vec3(0,1,0), hl);
    // color += plot(height, p, camZoomIn(x)) * hlCol(vec3(0,1,0), hl);
    
    // float stepX;

    // stepX = makeAnimStep(x, 0.);
    // color += plot(height, p, wobbleScaleAnim(stepX)) * hlCol(vec3(1,0,0), hl) * plotFade(stepX);
    // color += plot(height, p, moveAnim(stepX)) * hlCol(vec3(1,0,1), hl) * plotFade(stepX);
    
    // stepX = makeAnimStep(x, 1.);
    // color += plot(height, p, wobbleScaleAnim(stepX)) * hlCol(vec3(1,0,0), hl) * plotFade(stepX);
    // color += plot(height, p, moveAnim(stepX)) * hlCol(vec3(1,0,1), hl) * plotFade(stepX);
    
    // stepX = makeAnimStep(x, 2.);
    // color += plot(height, p, wobbleScaleAnim(stepX)) * hlCol(vec3(1,0,0), hl) * plotFade(stepX);
    // color += plot(height, p, moveAnim(stepX)) * hlCol(vec3(1,0,1), hl) * plotFade(stepX);
 
    // color = vec3(0);

    // color += plot(height, p, makeAnimStepNomod(x, 0., 0.)) * vec3(1);
    // color += plot(height, p, makeAnimStepNomod(x, 1., 0.)) * vec3(1);
    // color += plot(height, p, makeAnimStepNomod(x, 2., 0.)) * vec3(1);

    color += plot(height, p, animTime(x)) * hlCol(vec3(1,0,0), hl);


    color += vec3(1,1,0) * smoothstep(.015, .005, length(p - vec2(.95, .84) * vec2(1.,height)));
    color += vec3(0,1,1) * smoothstep(.015, .005, length(p - vec2(.85, .7) * vec2(1.,height)));

    // vec2 d = abs(p * 2. - 1.) - 1.;
    // float e = min(max(d.x,d.y), 0.) + length(max(d, 0.));

    // color += smoothstep(.01, .0, abs(e)) * vec3(0,1,0);

}




void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
    init();

    loopDuration = (MODEL_STEPS + 0.) * stepDuration;
    
    #ifdef SHOW_ANIMATION
        // loopDuration /= stepSpeed;
    #endif

    time = iGlobalTime;
    // time = 3.84;
    time *= 1.5;
    // time *= 2.;
    // time /=2.;
    //time += .1;
    time = mod(time, loopDuration);
    time = time/loopDuration;
    //time = loopDuration;
    //time= 0.;
    //time /= 2.;
    //time = mod(time, 1.);
    // t = 1. - t;
    // time = animTime(time);



    mousee = iMouse.xy;


    mousee = (vec2(
        0.5,
        0.66
    )) * iResolution.xy;
    

    vec2 p = (-iResolution.xy + 2.0*fragCoord.xy)/iResolution.y;
    vec2 m = mousee.xy / iResolution.xy;
    
    debugSwitch = p.x > 0.;
    


//    time = m.x * loopDuration;

    camPos = vec3( 0., 0., 2.);
    camTar = vec3( 0. , 0. , 0. );
    vec3 camUp = vec3(0., 1., 0.);

    // camera movement
    doCamera(camPos, camTar, camUp, m);

    // camera matrix
    mat3 camMat = calcLookAtMatrix( camPos, camTar, camUp );  // 0.0 is the camera roll

    // create view ray
    vec3 rd = normalize( camMat * vec3(p.xy,2.0) ); // 2.0 is the lens length

    vec3 color = vec3(0.);

    Hit hit = raymarch(CastRay(camPos, rd));
    color = render(hit);

    #ifndef DEBUG
       color = linearToScreen(color);
    #endif

    #ifdef SHOW_PATHS
        renderPaths(color, fragCoord);
    #endif

   // color = linearToScreen(color);
   
    fragColor = vec4(color,1.0);
}
