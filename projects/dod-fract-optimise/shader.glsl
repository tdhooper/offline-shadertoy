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

bool debugSwitch = false;

float stepScale = .275;
float stepMove = 2.;
float stepDuration = 2.;
float loopDuration;
float ballSize = 1.5;
float transitionPoint = .5;

// #define SHOW_ANIMATION
// #define SHOW_BOUNDS;
// #define SHOW_ITERATIONS

// #define USE_OUTER_BOUNDS;

#ifdef SHOW_ANIMATION
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

float makeAnimStep(float t, float stepIndex) {
    float x = t;
    x *= MODEL_STEPS;
    x -= stepIndex;
    x *= transitionPoint;
    x = tweakAnim(x);
    return x;
}

float makeAnimStep(float t, float stepIndex, float delay) {
    return makeAnimStep(t - delay, stepIndex);
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
    vec2 level;
    bool isBound;
};

Model makeBounds(float dist) {
    return Model(dist, vec2(0), true);
}

// checks to see which intersection is closer
Model opU( Model m1, Model m2 ){
    if (m1.dist < m2.dist) {
        return m1;
    } else {
        return m2;
    }
}


struct ModelSpec {
    float move;
    float size;
    float sizeCore;
    float bounds;
};

float boundsForStep(vec3 p, float move, float size, float scale) {
    float overfit = .3;
    p /= scale;
    float d = (length(p) - move - size - overfit);
    d *= scale;
    return d;
}

ModelSpec specForStep(vec3 p, float x, float scale) {
    float move = moveAnim(x) * stepMove;
    float sizeScale = mix(1., stepScale, wobbleScaleAnim(x));
    float sizeScaleCore = mix(1., stepScale, scaleAnim(x));
    float size = ballSize * sizeScale;
    float sizeCore = ballSize * sizeScaleCore;
    float bounds = boundsForStep(p, move, size, scale);
    return ModelSpec(move, size, sizeCore, bounds);
}

vec2 levelStep(vec3 p, float move, float size, float x) {
    float transition = smoothstep(0., .1, x);
    float level = hardstep(max(move - size, size + .01), size, length(p));
    level = mix(0., level, transition);
    float blend = hardstep(move + size, size, length(p));
    blend = mix(0., blend, transition);
    return vec2(level, blend);
}

float boundsThreshold;

Model makeModel(vec3 p, float x, float scale, vec2 level) {
    float d, part;

    ModelSpec spec = specForStep(p, x, scale);
    
    float move = spec.move;
    float size = spec.size;
    float sizeCore = spec.sizeCore;

    if (spec.bounds > boundsThreshold) {
        return makeBounds(spec.bounds);
    }
    
    p /= scale;
    fold(p);

    // Setup smoothing

    float rBlend = hardstep(.1, transitionPoint * .8, x);
    rBlend = smoothstep(0., 1., rBlend);
    float r = mix(0., .4, rBlend);

    // Center ball

    vec3 vA = vec3(0);

    part = length(p - vA) - sizeCore;
    d = part;

    level += levelStep(p, move, size, x);

    // Setup ball

    vec3 vB = triV.a * move;

    // Setup bridge

    float cr = 0.04;
    float rSep = hardstep(transitionPoint * .8, transitionPoint, x);
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

    return Model(d, level, false);
}



Model subDModel(vec3 p) {

    float stepIndex = -initialStep;
    float scale = 1.;
    
    vec3 iv;
    float delay = 0.;

    float innerBounds = 1e12;
    float bounds = 1e12;
    float innerB = 1e12;
    float boundsCandidate;
    float innerBoundsCandidate;

    float threshold = .3;

    float midSizeScale = mix(1., stepScale, scaleAnim(transitionPoint));

    float prevStepIndex, x, move, sizeScale, size;

    float css = 1.;

    #ifndef SHOW_ANIMATION
        float time = animTime(time);
    #endif

    float stepX;

    vec2 level = vec2(0);
    float delayLevel = 0.;

    for (float i = 1. - initialStep; i < MODEL_STEPS; i++) {

        stepX = makeAnimStep(time, i, delay);

        if (stepX >= 0.) {
 
            stepIndex = i;
            prevStepIndex = stepIndex - 1.;

            if (stepIndex == 0.) {
                css = 1.;
            } else {
                css *= midSizeScale;
            }

            scale = pow(midSizeScale, prevStepIndex);


            x = makeAnimStep(time, prevStepIndex, delay);
            ModelSpec spec = specForStep(p, x, scale);
            move = spec.move;
            size = spec.size;

            
            p /= scale;
            
            level += levelStep(p, move, size, x);

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
                delay += hash(iv * 1.5 - spectrum(mod(delayLevel, 3.) / 6.)) * .6;
            } else {
                delayLevel += 1.;
            }


            innerBoundsCandidate = innerBounds + .3 * scale;
            if (innerBoundsCandidate > -.0) {
                innerB = min(innerB, innerBoundsCandidate);
            }

        }
    }

    float mx = makeAnimStep(time, stepIndex, delay);
    Model model = makeModel(p, mx, css, level);
    
    if (innerB > boundsThreshold) {
        model.dist = min(model.dist, innerB);
    }

    return model;
}

float modelScale;

Model map( vec3 p ){
    p /= modelScale;
    boundsThreshold = .1 / modelScale;
    Model model = subDModel(p);
    model.dist *= modelScale;
    return model;
}


vec3 camPos;
float camDist;
vec3 camTar;



float scaleForStep(float step) {
    return pow(1./stepScale, step);
}

void doCamera(out vec3 camPos, out vec3 camTar, out vec3 camUp) {
    float x = time;

    camDist = 3. / stepScale;
    camDist = 8.;

    float camZoom = camZoomInOut(x);
    modelScale = scaleForStep(camZoom * MODEL_STEPS);

    camUp = vec3(0,-1,0);
    camTar = vec3(0.);
    camPos = vec3(0,0,camDist);
        
    pR(camPos.xz, animCamRotate(x) * PI * 2.);

    #ifdef SHOW_ANIMATION
        camDist = 6.5;
        modelScale = 1.;
        
        // camDist /= 2.;

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

    background = vec3(1.);
    background = vec3(.95, .95, 1.);
    
    
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

    fog = abs(length(camTar - hit.pos)) / (camDist * 2.5);
    fog = clamp(fog, 0., 1.);



    //*
    diffuse = vec3(.3) * vec3(.9, .3, .8);
    vec3 highlight = vec3(1.2) * vec3(.8,.5,1.2);
    float glow = 1. - dot(normalize(camPos), hit.normal);
    glow += .5 * (1.-dot(hit.normal, normalize(hit.pos)));
    glow *= .5;
    glow = gainStep(glow, 2.);
    diffuse = mix(diffuse, diffuse * 3., glow);
    
    // diffuse = hit.color;
    vec2 level = hit.model.level;
    // level = floor(level);
    diffuse = spectrum(level.y / MODEL_STEPS + .1 - 1./3.);
    // diffuse = pal5(mod(level / 4., 1.));
        // diffuse = vec3(mod(level, 3.) / 3.);

    diffuse = mix(diffuse * 1., diffuse * 1.5, glow);

    fog = smoothstep(camDist *.1, camDist, length(camTar - hit.pos)) * .5;
    fog = mix(fog, 1., smoothstep(0., camDist * 2.5, length(camTar - hit.pos)));


    // fog = (dot(normalize(camTar - camPos), hit.pos) + camDist) / camDist * .25;
    
    diffuse = mix(diffuse, background, fog);

    // diffuse = vec3(pow(fog, 2.));

    // diffuse = clamp(diffuse, 0., 1.);
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



float round(float a) {
    return floor(a + .5);
}

const float SAMPLES = 1.;




void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
    init();

    vec2 p = (-iResolution.xy + 2.0*fragCoord.xy)/iResolution.y;
    
    debugSwitch = p.x > 0.;
    

    loopDuration = 3. * stepDuration;
    

    time = iGlobalTime;
    // time = 3.84;
    time *= 1.5;

    // if ( ! debugSwitch) {
    //     transitionPoint = .7;
    // }
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




//    time = m.x * loopDuration;

    camPos = vec3( 0., 0., 2.);
    camTar = vec3( 0. , 0. , 0. );
    vec3 camUp = vec3(0., 1., 0.);

    // camera movement
    doCamera(camPos, camTar, camUp);

    mat3 camMat;
    vec3 rd;
    vec3 color = vec3(0.);
    Hit hit;

    vec3 sCamPos;
    float j, k, l;

    float radius = .0;

    vec2 pp = p;

    for(float i = 0.; i < SAMPLES; i++) {
        j = i / SAMPLES;
        k = hash(vec3(p, j));
        l = hash(vec3(j, p) * 2.);
        sCamPos = camPos;
        l = pow(l, .5);
        // l = .05;
        // k = j;
        vec2 offset = vec2(
            sin(k * PI * 2.) * radius * l,
            cos(k * PI * 2.) * radius * l
        );
        vec3 camN = normalize(camTar - camPos);
        vec3 camX = cross(camUp, camN);
        vec3 camY = cross(camX, camN);
        sCamPos += offset.x * camX;
        sCamPos += offset.y * camY;
        camMat = calcLookAtMatrix( sCamPos, camTar, camUp );  // 0.0 is the camera roll
        // pp += offset * .05;
        rd = normalize( camMat * vec3(pp, 2.) ); // 2.0 is the lens length
        hit = raymarch(CastRay(sCamPos, rd));
        color += render(hit);
    }

    color /= SAMPLES;

    // if (p.y < -.6) {
    //     color = pal5(round(p.x * MODEL_STEPS) / MODEL_STEPS);
    // }

    // if (p.y < -.8) {
    //     color = pal5(p.x);
    // }

    #ifndef DEBUG
       color = linearToScreen(color);
    #endif

   // color = linearToScreen(color);
   
    fragColor = vec4(color,1.0);
}
