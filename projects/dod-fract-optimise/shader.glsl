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

mat3 cameraRotation() {
    vec2 r = vec2(.66 + .5, 0.) * PI;
    return sphericalMatrix(r.x, r.y);
}


// --------------------------------------------------------
// HG_SDF
// --------------------------------------------------------

#define saturate(x) clamp(x, 0., 1.)

void pR(inout vec2 p, float a) {
    p = cos(a)*p + sin(a)*vec2(p.y, -p.x);
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

// fCapsule, but with a break at the midpoint
float fCapsule(vec3 p, vec3 a, vec3 b, float r, float breakSize) {
    vec3 m = mix(b, a, .5);
    float s1 = fLineSegment(p, a, mix(m, a, breakSize));
    float s2 = fLineSegment(p, b, mix(m, b, breakSize));
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
    
    
vec3 nc;
Tri triV;
TriPlanes triP;

int Type = 5;

void init() {//setup folding planes and vertex
    vec3 pab,pbc,pca;
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


// --------------------------------------------------------
// Closest icosahedron vertex
// Branchless version of the one in
// https://www.shadertoy.com/view/Mtc3RX
// --------------------------------------------------------

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


// --------------------------------------------------------
// Easing
// --------------------------------------------------------

float hardstep(float a, float b, float t) {
    float s = 1. / (b - a);
    return clamp((t - a) * s, 0., 1.);
}

// https://www.shadertoy.com/view/ldBfR1
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

float gainStep(float start, float end, float x, float e) {
    return gain(hardstep(start, end, x), e);
}

float gainStep(float x, float e) {
    return gainStep(0., 1., x, e);
}

float kink(float x, vec2 p, float e1, float e2) {
    float a = (1. - pow(1. - x / p.x, e1)) * p.y;
    float b = pow((x - p.x) / (1. - p.x), e2) * (1. - p.y) + p.y;
    return mix(a, b, step(p.x, x));
}

float wobble(float x, float freq) {
    float w = sin(x * PI * 2. * freq - PI * .5) * .5 + .5;
    w *= sin(x * PI + PI * .5) * .5 + .5;
    return w;
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
// Noise
// --------------------------------------------------------

float hash( const in vec3 p ) {
    return fract(sin(dot(p,vec3(127.1,311.7,758.5453123)))*43758.5453123);
}


// --------------------------------------------------------
// Config
// --------------------------------------------------------

float stepScale = .275;
float stepMove = 2.;
float stepDuration = 2.;
float ballSize = 1.5;

// How far into the subdivision animation do we start animating
// the next subdivision
float transitionPoint = .5; 

// #define DEBUG_ANIMATION

#ifdef DEBUG_ANIMATION
    const float initialStep = 0.;
    const float MODEL_STEPS = 2.;
#else
    const float initialStep = 2.;
    const float MODEL_STEPS = 3.;
#endif


// --------------------------------------------------------
// Animation
// --------------------------------------------------------

float tweakAnim(float x) {
    return mix(x, kink(x, vec2(.4), 2., .8), .5);
}

// Get the timeline for a single subdivision iteration step
float animStep(float t, float stepIndex) {
    float x = t;
    x *= MODEL_STEPS;
    x -= stepIndex;
    x *= transitionPoint;
    x = tweakAnim(x);
    return x;
}

float animStep(float t, float stepIndex, float delay) {
    return animStep(t - delay, stepIndex);
}

float moveAnim(float x) {
    float a = 1.;
    float h = 1.;
    float blend = x;
    blend = gainStep(-a, a, blend, 2.) * h * 2. - h;
    blend = gainStep(blend, 1.5);
    return blend;
}

float scaleAnim(float x) {
    x /= transitionPoint;
    float a = 1.;
    float h = 1.;
    float blend = x;
    blend = hardstep(0., .85, x);
    blend = gainStep(-a, a, blend, 1.2) * h * 2. - h;
    blend = gainStep(blend, 1.2);
    return blend;
}

float wobbleScaleAnim(float x) {
    float blend = scaleAnim(x);
    x /= transitionPoint;
    blend -= wobble(hardstep(.6, 2.2, x), 3.5) * .1;
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

float animTimeA(float x) {
    return circleEaseOut(.25, .3, x);
}

float animTime(float x) {
    float o = .78;
    return animTimeA(mod(x - o, 1.)) + (1. - animTimeA(1. - o)) + floor(x - o);
}


// --------------------------------------------------------
// Modelling
// --------------------------------------------------------

struct Model {
    float dist;
    float level;
};

Model makeBounds(float dist) {
    return Model(dist, 0.);
}

// checks to see which intersection is closer
Model opU( Model m1, Model m2 ){
    if (m1.dist < m2.dist) {
        return m1;
    } else {
        return m2;
    }
}


// Description of a single ball subdivision animation
// at a given iteration step
struct ModelSpec {
    float move; // How much the outer ball moves by
    float sizeScale; // How much to shrink the outer ball
    float sizeScaleCore; // How much to shrink the inner ball
    float bounds; // Bounding distance for the model
    float level; // Iteration/subdivision level for this animation
};

float boundsForStep(vec3 p, float move, float sizeScale, float scale) {
    float overfit = .3;
    p /= scale;
    float d = (length(p) - move - ballSize * sizeScale - overfit);
    d *= scale;
    return d;
}

float levelStep(vec3 p, float move, float size, float x) {
    float transition = smoothstep(0., .1, x);
    float blend = hardstep(move + size, size, length(p));
    blend = mix(0., blend, transition);
    return blend;
}

ModelSpec specForStep(vec3 p, float x, float scale) {
    float move = moveAnim(x) * stepMove;
    float sizeScale = mix(1., stepScale, wobbleScaleAnim(x));
    float sizeScaleCore = mix(1., stepScale, scaleAnim(x));
    float bounds = boundsForStep(p, move, sizeScale, scale);
    float level = levelStep(p / scale, move, sizeScale * ballSize, x);
    return ModelSpec(move, sizeScale, sizeScaleCore, bounds, level);
}


float boundsThreshold;

// Animation of the ball subdividing with smooth blending
// and connective struts that snap
Model blendedModel(vec3 p, float x, float scale, float level) {
    ModelSpec spec = specForStep(p, x, scale);
    level += spec.level;
    
    if (spec.bounds > boundsThreshold) {
        return makeBounds(spec.bounds);
    }
    
    p /= scale;
    fold(p);

    float move = spec.move;
    float size = spec.sizeScale * ballSize;
    float sizeCore = spec.sizeScaleCore * ballSize;

    // Setup smoothing

    float radiusBlend = hardstep(.1, transitionPoint * .8, x);
    radiusBlend = smoothstep(0., 1., radiusBlend);
    float radius = mix(0., .4, radiusBlend);

    // Core ball

    vec3 posCore = vec3(0);
    float d = length(p - posCore) - sizeCore;

    // Setup outer ball

    vec3 posOuter = triV.a * move;

    // Setup connective strut

    float capRadius = 0.04;
    float gapBlend = hardstep(transitionPoint * .8, transitionPoint, x);
    float gap = mix(0., 1., gapBlend);

    // Ball and bridge

    d = smin(d, fCapsule(p, posCore, posOuter, capRadius, gap), radius);
    d = smin(d, length(p - posOuter) - size, radius);
    
    // First reflection

    vec3 rPlane = triP.bc;
    p = reflect(p, rPlane);
    
    d = smin(d, fCapsule(p, posCore, posOuter, capRadius, gap), radius);
    d = smin(d, fCapsule(p, posOuter, reflect(posOuter, rPlane), capRadius, gap), radius);
    d = smin(d, length(p - posOuter) - size, radius);

    // Second reflection

    vec3 rPlane2 = reflect(triP.ca, rPlane);
    p = reflect(p, rPlane2);

    d = smin(d, fCapsule(p, posCore, posOuter, capRadius, gap), radius);
    d = smin(d, fCapsule(p, posOuter, reflect(posOuter, rPlane2), capRadius, gap), radius);
    d = smin(d, fCapsule(p, posOuter, reflect(reflect(posOuter, rPlane), rPlane2), capRadius, gap), radius);
    d = smin(d, length(p - posOuter) - size, radius);

    d *= scale;

    return Model(d, level);
}


// Iterates through each subdivision of the ball, restricting the
// smooth blending method above to just the currently-animated level
Model iteratedModel(vec3 p) {

    float stepIndex = -initialStep;
    float prevStepIndex;
    float x;
    float scale = 1.;
    float sizeScale = 1.;

    // Scale at the transition point
    float midSizeScale = mix(1., stepScale, scaleAnim(transitionPoint));
    
    float level = 0.; // Iteration level used for colouring
    float delayLevel = 0.;
    vec3 iv;
    float delay = 0.;

    float coreBoundry;
    float coreOverstepBounds = 1e12;
    float coreOverstepBoundsCandidate;

    #ifndef DEBUG_ANIMATION
        float time = animTime(time);
    #endif

    for (float i = 1. - initialStep; i < MODEL_STEPS; i++) {

        x = animStep(time, i, delay);

        if (x >= 0.) {
 
            stepIndex = i;
            prevStepIndex = stepIndex - 1.;

            scale = pow(midSizeScale, prevStepIndex);

            x = animStep(time, prevStepIndex, delay);
            ModelSpec spec = specForStep(p, x, scale);
            level += spec.level;

            coreBoundry = (length(p / scale) - spec.move * .55) * scale;
            if (coreBoundry > 0.) {
                iv = icosahedronVertex(p);
                fold(p);
                p -= triV.a * spec.move * scale;
                sizeScale = spec.sizeScale;
                // Adjust the start time of each ball for some variety
                delay += hash(iv * 1.5 - spectrum(mod(delayLevel, 3.) / 6.)) * .6;
            } else {
                sizeScale = spec.sizeScaleCore;
                delayLevel += 1.;
            }
            
            coreOverstepBoundsCandidate = coreBoundry + .3 * scale;
            if (coreOverstepBoundsCandidate > -.0) {
                coreOverstepBounds = min(coreOverstepBounds, coreOverstepBoundsCandidate);
            }
        }
    }

    x = animStep(time, stepIndex, delay);
    Model model = blendedModel(p, x, scale * sizeScale, level);
    
    if (coreOverstepBounds > boundsThreshold) {
        model.dist = min(model.dist, coreOverstepBounds);
    }

    return model;
}

float modelScale;

Model map( vec3 p ){
    p /= modelScale;
    boundsThreshold = .1 / modelScale;
    Model model = iteratedModel(p);
    model.dist *= modelScale;
    return model;
}

vec3 camPos;
float camDist;
vec3 camTar;

// Return the modelScale for the given subdivision iteration step,
// such that all steps look the same size
float scaleForStep(float step) {
    return pow(1./stepScale, step);
}

void doCamera(out vec3 camPos, out vec3 camTar, out vec3 camUp) {
    float x = time;

    camDist = 8.;

    float camZoom = camZoomInOut(x);
    modelScale = scaleForStep(camZoom * MODEL_STEPS);

    camUp = vec3(0,-1,0);
    camTar = vec3(0.);
    camPos = vec3(0,0,camDist);
        
    pR(camPos.xz, animCamRotate(x) * PI * 2.);

    #ifdef DEBUG_ANIMATION
        camDist = 5.;
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
// Shading
// --------------------------------------------------------

void shadeSurface(inout Hit hit){

    vec3 background = vec3(.95, .95, 1.);

    if (hit.isBackground) {
        hit.color = background;
        return;
    }

    float glow = 1. - dot(normalize(camPos), hit.normal);
    glow += .5 * (1. - dot(hit.normal, normalize(hit.pos)));
    glow *= .5;
    glow = gainStep(glow, 2.);
    
    float level = hit.model.level;
    vec3 diffuse = spectrum(level / MODEL_STEPS + .1 - 1./3.);
    diffuse = mix(diffuse * 1., diffuse * 1.5, glow);

    float fog = smoothstep(camDist *.1, camDist, length(camTar - hit.pos)) * .5;
    fog = mix(fog, 1., smoothstep(0., camDist * 2.5, length(camTar - hit.pos)));

    diffuse = mix(diffuse, background, fog);
    hit.color = diffuse;
}


vec3 render(Hit hit){
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



void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
    init();

    vec2 p = (-iResolution.xy + 2. * fragCoord.xy) / iResolution.y;

    float loopDuration = 3. * stepDuration;

    time = iGlobalTime;
    time *= 1.5;
    time = time/loopDuration;
    time = mod(time, 1.);

    camPos = vec3( 0., 0., 2.);
    camTar = vec3( 0. , 0. , 0. );
    vec3 camUp = vec3(0., 1., 0.);
    doCamera(camPos, camTar, camUp);

    mat3 camMat = calcLookAtMatrix( camPos, camTar, camUp );  // 0.0 is the camera roll
    vec3 rd = normalize( camMat * vec3(p, 2.) ); // 2.0 is the lens length
    Hit hit = raymarch(CastRay(camPos, rd));
    vec3 color = render(hit);

    color = linearToScreen(color);
    fragColor = vec4(color,1.0);
}
