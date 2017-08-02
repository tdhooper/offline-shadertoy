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

float squareSine(float x, float e) {
    x = mod(x, PI * 2.);
    float period = x / mod((PI / 2.), 4.);
    float a = pow(abs(period - 3.), e) - 1.;
    float b = -pow(abs(period - 1.), e) + 1.;
    return period > 2. ? a : b;
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

float sineInOut(float t) {
  return -0.5 * (cos(PI * t) - 1.0);
}

float sineOutIn(float t) {
  return asin(t * 2. - 1.) / PI + .5;
}

float sineOut(float t) {
  return sin(t * HALF_PI);
}

float squareOut(float t, float e) {
  t = clamp(t, 0., 1.);
  return squareSine(t * HALF_PI, e);
}

float sineIn(float t) {
  return sin((t - 1.0) * HALF_PI) + 1.0;
}

float squareIn(float t, float e) {
  return squareSine((t - 1.0) * HALF_PI, e) + 1.0;
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

float squaresteploop(float x, float e) {
    float o = floor(x / 1.);
    x -= o;
    return squarestep(0., 1., x, e) + o;
}

float squareOutLoop(float x, float e) {
    float o = floor(x / 1.);
    x -= o;
    return squareOut(x, e) + o;
}

float squarestepIn(float start, float end, float x, float e) {
    float len = end -start;
    x = (x - start) * (1./len);
    // x = clamp(x, 0., 1.);
    return squareIn(x, e);
}

// Like step, but specify the start offset and length,
// then it will loop around so that output at 0. == output at 1.
float squarestepOutOffset(float offset, float len, float x, float e) {
    float a = squareOut((1. - offset) / len, e);
    return squareOut(mod(x - offset, 1.) / len, e) - a + step(offset, x);
}

float squarestepOut(float start, float end, float x, float e) {
    float len = end -start;
    x = (x - start) * (1./len);
    x = clamp(x, 0., 1.);
    return squareOut(x, e);
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
#define SHOW_PATHS

#ifdef SHOW_ANIMATION
    const float initialStep = 0.;
    const float MODEL_STEPS = 2.;
#else
    const float initialStep = 2.;
    const float MODEL_STEPS = 3.;
#endif

//#define SHOW_BOUNDS;
#define USE_BOUNDS;
// #define BOUNCE_INNER;


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

float makeAnimStepNomodNotweak(float t, float stepIndex) {
    // return makeAnimStepNomod(t, stepIndex);
    float x = t;
    x -= 1. / MODEL_STEPS * stepIndex;
    x *= MODEL_STEPS;
    x *= stepSpeed;
    return x;
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

float wobble(float x) {
    // float freq = 5.;
    float freq = mix(3.5, 2., x);
    freq = 3.5;
    // freq = 10.;
    float w = sin(x * PI * 2. * freq - PI * .5) * .5 + .5;
    // w *= 1. - x;
    w *= sin(x * PI + PI * .5) * .5 + .5;
    return w;
}

float wobble2(float x, float freq) {
    // float freq = 5.;
    // freq = 10.;
    float w = sin(x * PI * 2. * freq - PI * .5) * .5 + .5;
    // w *= 1. - x;
    w *= sin(x * PI + PI * .5) * .5 + .5;
    return w;
}

float wobbleScaleAnim(float x) {
    float blend = scaleAnim(x);
    x /= stepSpeed;
    blend -= wobble(hardstep(.6, 2.2, x)) * .1;
    return blend;
}

float newBlendA(float x) {
    float blend;
    blend = sinstep(x / 2. + .5) * 2. - 1.;
    blend = squarestep(blend, 3.);
    return blend;
}

float newBlend(float x) {
    float m = 0.84;
    float o = newBlendA(m);
    return newBlendA(mod(x + m, 1.)) - o + max(sign(x + m - 1.), 0.);
}

const float ANIM_CAM_START = .96;

float animCamRotateA(float x) {
    // return x;
    // return mix(gain(x, 2.), x, .5);
    return mix(gainOut(x, 6.), x, .92);
    return x;
    float y = mix(0., 1. - gainOut(x, 3.), gainIn(x + .9, 10.) * .5);
    y = mix(x, y, .4);
    return y;
}

float animCamRotate(float x) {
    // return 0.15;
    // return x;
    // return animCamRotateA(x); 
    float o = ANIM_CAM_START;
    return animCamRotateA(mod(x - o, 1.)) + (1. - animCamRotateA(1. - o)) + floor(x - o);    

    return squarestepOutOffset(ANIM_CAM_START, 1., x, 5.);
    float r;
    x = mod(x +.7, 1.);
    r = squarestep(0., 1., x, 3.);
    r = mix(r, x, .3);
    return r;
}

float animModelScaleA(float x) {
    // x -= ANIM_CAM_START;
    // x = clamp(x, 0., 1.);
    // x *= 8.;
    // x = clamp(x, 0., 1.);
    // float r = 1.;
    // return 1.-sqrt(pow(r, 2.) - pow(x, 2.));
    // return gainIn(x, 38.);
    // return mix(gain(x, 20.), 1., gain(x, 200.));

    // float w = wobble2(hardstep(.75, 1., x), 2.);

    float y = mix(0., gain(x, 5.5), gain(x, 100.));
    // x -= .38;
    x = hardstep(.4, .8, x);
    y = gainIn(x, 2.5);
    // y = clamp(y * 1.5 - .2, 0., 1.);
    y = squarestep(y, 1.5);

    // y += w * .3;
    // y += .5;

    // float r = gain(hardstep(.0, 1., x), 20.);
    // y = mix(x, r, .5);
    return y;
}

float animModelScale(float x) {
    // return hardstep(.55, .75, x);
    x = hardstep(.35, .65, x);
    x = sinstep(x);
    // return x;
    return gainIn(x, 3.);
    return 1.;
    return animModelScaleA(x);
    // return animModelScaleA(mod(x - o, 1.)) + (1. - animModelScaleA(1. - o)) - step(x, o);    

    // return squarestepOutOffset(ANIM_CAM_START, 1.-ANIM_CAM_START, x, 2.);
    // return squarestepIn(.4, .9666, x, 2.);
}

float circlestep(float r, float x) {
    float y = sqrt(r * r - (x - r) * (x - r)) + (1.-r);
    if (step(r, x)> 0.) {
        return 1.;
    }
    //y = mix(y, 1., step(r, x));
    return y;
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
    // return gain(x, 3.5);
    // return gainOut(gainIn(x, 2.), 5.);
    // return gainIn(gainOut(x, 4.), 2.);
    return gainOut(x, 3.5);
}

float animTime(float x) {

    float xoo = .0;
    float q = .78;
    float h = - .0;
    // xoo = q = h = 0.;
    // xoo = h = 0.;
    x += xoo;
    return animTimeA(mod(x - q, 1.)) + (1. - animTimeA(1. - q)) + floor(x - q) + h;
    
    // return x;
    // return gainOut(hardstep(0., .8, x), 4.) * .7 + hardstep(.85, 1., x) * .3;
    return hardstep(0., .3, x) * .7 + hardstep(.85, 1., x) * .3;
    return gainIn(hardstep(0., .5, x), 2.5);

    // return x;
    vec2 p1 = vec2(.5, .68);
    vec2 p2 = vec2(.68);
    float y;
    y = hardstep(0., p1.x, x) * p1.y;
    y += hardstep(p1.x, p2.x, x) * (p2.y - p1.y);
    y += hardstep(p2.x, 1., x) * (1. - p2.y);
    return y;
    // return  + hardstep(p1.x, 1., x) * (1. - p1.y);

    // return x;
    float o = .5;

    float radius = .0;
    float slope = .6;

    // return mix(kink(x, vec2(.93), 2.5, 1./1.), x, .5);

    // return circleEaseIn(radius, slope, x);

    float xo = x;

    x -= o * (1. - slope);
    float yy = circleEaseIn(radius, slope, 1. - x * 2.) * -.5 + .5;
    radius = .0;
    yy += circleEaseIn(radius, slope, x * 2. - 1.) * .5;
    yy += o;

    yy= mix(yy, xo, .2);

    return yy;
    x = xo;
    o = -.8;
    float e = 10.;
    return mix(1.-pow(1.-x, e), x, .2);
    float n = squaresteploop(x + o, e) - squaresteploop(o, e);
    return mix(x, n, 1.);
}

float modelScale;


Model makeModel(vec3 p, float x, float scale) {
    float d, part;
    
    
    float move = moveAnim(x) * stepMove;
    float sizeScale = mix(1., stepScale, wobbleScaleAnim(x));
    float sizeScaleCore = mix(1., stepScale, scaleAnim(x));
    float size = ballSize * sizeScale;
    float sizeCore = ballSize * sizeScaleCore;

    float bounds = (length(p / scale) - move - size - .3) * scale;

    float threshold = .01;

    #ifdef SHOW_BOUNDS
        return Model(bounds, 0., vec3(0.), true);
    #endif
    #ifdef USE_BOUNDS
        if (bounds > threshold) {
            return Model(bounds, 0., vec3(0.), true);
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

    return Model(d, x, vec3(0.), false);
}

float makeModelScale(float x) {
    float scale = 1.;
    float stepX;
    for (float i = 0.; i < MODEL_STEPS; i++) {
        stepX = makeAnimStepNomodNotweak(time, i);
        scale *= mix(1., stepScale, scaleAnim(stepX));
    }
    return 1. / scale;
}

float scaleForStep(float step) {
    return pow(1./stepScale, step);
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

    //css /= midSizeScale;

    float time = animTime(time);

    float stepX;

    for (float i = 1. - initialStep; i < MODEL_STEPS; i++) {

        stepX = makeAnimStepNomod(time, i, delay);

        if (stepX >= 0. && ! hasBounds) {
            stepIndex = i;
            prevStepIndex = stepIndex - 1.;

            if (stepIndex == 0.) {
                css = 1.;
            } else {
                css *= midSizeScale;
            }


            scale = pow(midSizeScale, prevStepIndex);
            p /= scale;

            x = makeAnimStepNomod(time, prevStepIndex, delay);
            move = moveAnim(x) * stepMove;

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
            }
            p *= scale;
            
            if (innerBounds > 0.) {
                iv = icosahedronVertex(pp);
                delay += hash(iv) * .8;
            }



            #ifdef USE_BOUNDS
                sizeScale = mix(1., stepScale, wobbleScaleAnim(x));
                size = ballSize * sizeScale;

                boundsCandidate = length(pp) - move - size - threshold;
                boundsCandidate *= scale;
                boundsCandidate -= .1;    
                
                if (boundsCandidate > -.0) {
                    bounds = min(bounds, boundsCandidate);
                    hasBounds = true;
                }
            #endif            
        }
    }

    threshold *= scale;

    #ifdef SHOW_BOUNDS
        if (hasBounds) {
            return Model(bounds, 0., vec3(0.), true);
        }
    #endif
    #ifdef USE_BOUNDS
        if (bounds > threshold && hasBounds) {
            return Model(bounds, 0., vec3(0.), true);
        }
    #endif

    float mx = makeAnimStepNomod(time, stepIndex, delay);
    Model model = makeModel(p, mx, css);
    
    innerBounds -= threshold;

    #ifdef SHOW_BOUNDS
        return Model(min(model.dist, innerBounds), 0., vec3(0.), true);
    #endif
    #ifdef USE_BOUNDS
        if (innerBounds > threshold) {
            model.dist = min(model.dist, innerBounds);
        }
    #endif

    return model;
}

Model map( vec3 p ){
    mat3 m = modelRotation();
    p /= modelScale;
    Model model = subDModel(p);
    model.dist *= modelScale;
    return model;
}


float camDist;
vec3 camTar;

float camZoomOut(float x) {
    // return x;
    float p = .5;
    float y;
    y = hardstep(0., p + .1, x);
    return gainOut(y, 5.);
    return gainOut(sinstep(y), 2.5);
}

float camZoomIn(float x) {
    // return 1.;
    float p = .5;
    float y;
    y = hardstep(p - .3, 1., x);
    return gainIn(sinstep(y), 2.5);
    y = circleEaseIn(.2, .4, y);
    y = gainOut(y, 2.);
    // y = sinstep(x);
    return y;
    return gainIn(y, 2.5);
}

float camZoomInOutA(float x) {
    float s1, s2, s3;
    s1 = .0;
    s2 = -.12;
    s3 = 1.;

    float part1 = mix(s1, s2, camZoomOut(x));
    float part2 = mix(0., s3 - s2, camZoomIn(x));

    // return part1;
    float step = part1 + part2;
    return step;
}

float camZoomInOut(float x) {
    // x *= 1.01;
    float o = ANIM_CAM_START;
    float y = camZoomInOutA(mod(x - o, 1.)) + (1. - camZoomInOutA(1. - o)) + floor(x - o);
    y -= .15;
    return y;
}

void doCamera(out vec3 camPos, out vec3 camTar, out vec3 camUp, in vec2 mouse) {
    float x = time;

    camDist = 3. / stepScale;
    camDist = 8.;

    modelScale = mix(1., makeModelScale(x), animModelScale(x));
    modelScale = mix(scaleForStep(-1.), scaleForStep(2.), animModelScale(x));
    // modelScale= 1.;


    // TODO: Can we make the zoom out happen earlier?
    // Maybe generate the zoom at the scaleForStep value
    // then we can offset it
    // 0.  >  -.05  >  2.
    // offset:
    // -.2  >  -.05  >  2.  >  1.8
    float camZoom = camZoomInOut(x);
    modelScale = scaleForStep(camZoom * 3.);

    //     x = hardstep(.35, .65, x);
    // x = sinstep(x);
    // // return x;
    // return gainIn(x, 3.);


    camUp = vec3(0,-1,0);
    camTar = vec3(0.);
    camPos = vec3(0,0,camDist);
        
    pR(camPos.xz, animCamRotate(x) * PI * 2.);
    // pR(camPos.xz, x * PI * 2.);

    #ifdef SHOW_ANIMATION
        camDist = 4.5;
        modelScale = 1.;
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
    Model model;

    Ray ray = Ray(castRay.origin, castRay.direction, 0.);

    for( int i=0; i< NUM_OF_TRACE_STEPS ; i++ ){
        if (currentDist < INTERSECTION_PRECISION || ray.len > MAX_TRACE_DISTANCE) {
            break;
        }
        model = map(ray.origin + ray.direction * ray.len);
        currentDist = model.dist;
        ray.len += currentDist * FUDGE_FACTOR;
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

    return Hit(ray, model, pos, isBackground, normal, color);
}


// --------------------------------------------------------
// Rendering
// --------------------------------------------------------

vec3 camPos;

void shadeSurface(inout Hit hit){

    vec3 background = vec3(.1)* vec3(.5,0,1);

    background = pal1(1.) * .8;

    if (hit.isBackground) {
        hit.color = background;
        return;
    }

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
    diffuse = mix(diffuse, background, fog);
    // diffuse = vec3(glow);
    //*/
    // diffuse = vec3(length(diffuse * .5));
    
    hit.color = diffuse;
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
