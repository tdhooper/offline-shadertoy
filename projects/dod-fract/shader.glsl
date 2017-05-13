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
#define TAU 6.28318530718


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

// Cone with correct distances to tip and base circle. Y is up, 0 is in the middle of the base.
float fCone(vec3 p, float radius, float height) {
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
}

float fCone(vec3 p, float radius, float height, vec3 direction, float offset) {
    p -= direction * offset;
    p = reflect(p, normalize(mix(vec3(0,1,0), -direction, .5)));
    //p -= vec3(0,height,0);
    return fCone(p, radius, height);
}

// Reflect space at a plane
float pReflect(inout vec3 p, vec3 planeNormal, float offset) {
    float t = dot(p, planeNormal)+offset;
    if (t < 0.) {
        p = p - (2.*t)*planeNormal;
    }
    return sign(t);
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
    
    facePlane = pca;
    uPlane = cross(vec3(1,0,0), facePlane);
    vPlane = vec3(1,0,0);    
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

void fold(inout vec3 p) {
    for(int i=0;i<5 /*Type*/;i++){
        p.xy = abs(p.xy);
        p -= 2. * min(0., dot(p,nc)) * nc;
    }
}



// --------------------------------------------------------
// Modelling
// --------------------------------------------------------

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

float easeWithRest(float t) {
    float blend = cos(t * PI * 2.) * -.5 + .5;
    float slope = .2;
    float lin = (t - .5) * slope + .5;
    return mix(sineInOut(t), lin, blend);
}

float bDelay(float delay, float duration, float loop) {
    float t = mod(time, loop);
    if (t < delay) {
        return 0.;
    }
    if (t > duration + delay) {
        return 1.;
    }
    t -= delay;
    t /= duration;
    return t;
    //return sineOutIn(t);
    //return easeWithRest(t);
    //return sineInOut(t);
}

float stepScale = .275;
float stepMove = 2.;
float stepDuration = 2.;
float loopDuration;
float transitionPoint = .0;
float camOffset = 2.;
float ballSize = 1.;
float stepSpeed = .5;

const float initialStep = 0.;
const float MODEL_STEPS = 3.;

float squareSine(float x, float e) {
    //return sin(x);
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

float makeAnim(float localTime) {
    float blend = localTime / stepDuration * stepSpeed;
    blend = clamp(blend, 0., 1.);
    return blend;
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
    return moveAnim(x);
}

Model makeModel(vec3 p, float localTime, float scale) {
    float d, part;
    
    float x = makeAnim(localTime);
    float move = moveAnim(x) * stepMove;

    float size = mix(ballSize, ballSize * stepScale, scaleAnim(x));
    //size = ballSize;
    p /= scale;
    fold(p);

    part = length(p) - size;
    d = part;
    //d = 1e12;

    //d = part;

    //d = min(d, dot(p, triV.a) - amt * scale * .8);
    
    //return Model(d * scale, 0.);


    float r = smoothstep(.05, .5, x) * .4;
    //r = .5 * x;
    //r = .0;

    vec3 n = triV.c;
    
    part = length(p - n * move) - size;
    d = smin(d, part, r);

    
    vec3 rPlane = normalize(cross(triV.b, triV.a));
    n = reflect(n, rPlane);
    part = length(p - n * move) - size;
    d = smin(d, part, r);

    n = reflect(n, triP.ca);
    part = length(p - n * move) - size;
    d = smin(d, part, r);

    //d = min(original, d);
    //d = mix(original, d, blend2);

    d *= scale;
    return Model(d, 0.);
    return Model(d, x);

    //return d;
}

float makeOffsetMax(float level) {
    float scale = pow(stepScale, level);
    return stepMove * scale;
}

float makeOffsetAmt(float level) {
    float localTime = time - (stepDuration * (level - 1.));
    float x = makeAnim(localTime);
    return moveAnim(x) * makeOffsetMax(level);
}

vec3 makeOffset(float level) {
    return triV.c * makeOffsetAmt(level);
}

void makeSpace(inout vec3 p, float localTime, float scale) {
    float x = makeAnim(localTime);
    float move = moveAnim(x);
    p /= scale;
    if (length(p) > move * stepMove * .55) {
       fold(p);
       p -= triV.c * move * stepMove;
    }
    p *= scale;
}

float hardstep(float a, float b, float t) {
    float s = 1. / (b - a);
    return clamp((t - a) * s, 0., 1.);
}

float modelScale;

float makeModelScale() {
    float scale = 1.;
    for (float i = -1.; i < MODEL_STEPS; i++) {
        scale *= mix(
            1.,
            stepScale,
            scaleAnim(
                makeAnim(
                    time - (stepDuration * i)
                )
            )
        );
    }
    float initial = mix(1., stepScale, scaleAnim(makeAnim(stepDuration)));
    return (1. / scale) * initial;
}

Model subDModel(vec3 p) {

    //pReflect(p, -triV.c, camOffset);

    float scale = 1.;
    float level = -1.;

 
    float d;

    
    //float time = loopDuration;

    // p = mod(p + .5, 1.) - .5;
    // d = length(p - vec3(.0,0,0)) - .1;
    // d *= modelScale;
    // return d;

    float offset = .1;

    for (float i = 0.; i < MODEL_STEPS + initialStep; i++) {
        if (time >= stepDuration * (i - initialStep)) {
            level = i - initialStep;
            scale = pow(stepScale, level);
            makeSpace(p, time - (stepDuration * (level - 1.)), scale);
        }
    }


    scale = mix(
        pow(stepScale, level + 0.),
        pow(stepScale, level + 1.),
        //time / loopDuration * stepSpeed
        scaleAnim(makeAnim(time - (stepDuration * (level - 1.))))
        
    );
    //scale = 1.;
    
    return makeModel(p, time - (stepDuration * level), scale);
    //float part = makeModel(pp, time - (stepDuration * level), scale);
    //d = min(d, part);

    
}

Model map( vec3 p ){
    mat3 m = modelRotation();
    //p *= m;

     p /= modelScale;

    float x = time / loopDuration;
    x = smoothstep(0., 1., x);
    //x = squarestep(0., .8, x, 2.);
    float blend = 1.-pow(1.-x, 2.);
    vec3 offset = makeOffset(0.);// + makeOffset(1.);
    offset = mix(vec3(0), offset, blend);
    //p += offset;


    //return makeModel(p, time, 1.);
    

    Model model = subDModel(p);

    //d = min(d, length(p + offset) - .1);

     model.dist *= modelScale;

    
    //model = Model(makeModel(p, time), 1.);
    //model.dist = length(p - facePlane) - .1;
    return model;
}


float camDist;
vec3 camTar;


float newBlendA(float x) {
    float blend;
    //x = sinstep(x);
    blend = sinstep(x / 2. + .5) * 2. - 1.;
    blend = squarestep(blend, 3.);
    return blend;
}

float newBlend(float x) {
    float m = 0.84;
    float o = newBlendA(m);
    return newBlendA(mod(x + m, 1.)) - o + max(sign(x + m - 1.), 0.);
}


void doCamera(out vec3 camPos, out vec3 camTar, out vec3 camUp, in vec2 mouse) {
    float x = time / loopDuration;
    float apex = .6;
    float blend = smoothstep(0., apex, x) - (smoothstep(apex, 1., x));
    blend = sinstep(blend);
    //blend = sin(x * PI * 2. - PI * .5) * .5 + .5;
    //camDist = mix(3., 25., blend);

    //camDist = 2.;
    //camDist = 3.;

    camDist = mix(1.5, 1.8, blend);
    //camDist = 1.8;
    //camDist = mix(2., 20., squarestep(x*2., 2.) - squarestep(max(0.,x*2.-1.), 2.) );

    modelScale = makeModelScale();
    float sb;
    //sb = smoothstep(.8, .95, x) - smoothstep(.95, 1., x);
    //sb = squarestep(.8, 1., x, 1.1);
    float o = .55;
    sb = squarestep(o, 2. - o, x, 5.) * 2.;
    //sb = squarestep(.0, 1., x, 5.);
    modelScale = mix(1., modelScale, sb);
    //modelScale = 1.;
     
    camUp = normalize(vec3(0,-1,0));

    apex = .5;
    x = mod(x + .5, 1.);
    blend = smoothstep(0., apex, x) - smoothstep(apex, 1., x);
    blend = blend * 2. - 1.;

    blend = 0.;

    pR(camUp.zy, blend * .5);
    
    // apex = .333;
    // float overshoot = 0.;
    // blend = smoothstep(0., apex, x) * (1. + overshoot);
    // blend -= smoothstep(apex, 1., x) * overshoot;

    // blend = smoothstep(0., 1., x);
    // camTar = mix(startTar, endTar, blend) * modelScale;

    //camUp = vec3(1,0,0);

    camTar = vec3(0.);

    //blend = sin(x * PI * 2. - PI * .5) * .5 + .5;
    //blend = .2;
    //camTar += triV.b * blend * -20.;
    //pR(camTar.xz, x * PI * 2.);


    camPos = vec3(0,0,camDist);
    
    float e = 2.;
    float ax = .3;
    float ay = squarestep(0., 1., 1.-ax, e);
    float rotBlend = squarestep(0., 1., x + (1.-ax), e) - ay;
    rotBlend += squarestep(0., 1., x - ax, e);
    
    rotBlend = newBlend(x);
    rotBlend = mix(x, rotBlend, .95);
    //rotBlend = x;
    //rotBlend = 0.;
    float r = .25;
    //pR(camPos.xz, sin(x * PI * 2.) * r);
    //pR(camPos.yz, cos(x * PI * 2.) * r);
    
    pR(camPos.xz, rotBlend * PI * 2.);

    //pR(camPos.xz, x * PI * 1.);

    camPos *= cameraRotation();
    
    //camTar = -camPos;
    //pR(camTar.xz, x * PI * 2.);
    //camTar += camPos;

    //camPos += camTar;

}



// --------------------------------------------------------
// Ray Marching
// Adapted from: https://www.shadertoy.com/view/Xl2XWt
// --------------------------------------------------------

const float MAX_TRACE_DISTANCE = 500.; // max trace distance
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

void shadeSurface(inout Hit hit){

    vec3 background = vec3(.1);

    if (hit.isBackground) {
        hit.color = background;
        return;
    }

    vec3 light = normalize(vec3(.5,1,0));
    vec3 diffuse = vec3(dot(hit.normal, light) * .5 + .5);
    diffuse = mix(diffuse, vec3(1), .1);
    
    vec3 colA = vec3(.1,.75,.75) * 1.5;
    vec3 colB = vec3(.75,.1,.75);
    colB = mix(colB, colA, .7);
    
    float blend = sin(hit.model.id * TAU * 3.) * .5 + .5;
    blend = time / loopDuration;
    blend = 0.;
    blend = hit.model.id * 5.;

    diffuse *= mix(colA, colB, blend);
    diffuse = sin(diffuse);
    diffuse *= 1.3;
    
    float fog = clamp((hit.ray.len - 5.) * .5, 0., 1.);
    fog = mix(0., 1., length(camTar - hit.pos) / pow(camDist, 1.5)) * 1.;
    fog = clamp(fog, 0., 1.);
    
    //diffuse = hit.normal * .5 + .5;
    //fog = 0.;

    diffuse =  mix(diffuse, background, fog);
    
    
    hit.color = diffuse;
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

void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
    init();
    
    loopDuration = (MODEL_STEPS + .0) * stepDuration;
    time = iGlobalTime;
    //time += .1;
    time = mod(time, loopDuration);
    //time = loopDuration;
    //time= 0.;
    //time /= 2.;
    //time = mod(time, 1.);

    mousee = iMouse.xy;

    /*
    mousee = (vec2(
        0.4465875370919881,
        0.5849514563106796
    )) * iResolution.xy;
    */

    vec2 p = (-iResolution.xy + 2.0*fragCoord.xy)/iResolution.y;
    vec2 m = mousee.xy / iResolution.xy;

//    time = m.x * loopDuration;

    vec3 camPos = vec3( 0., 0., 2.);
    camTar = vec3( 0. , 0. , 0. );
    vec3 camUp = vec3(0., 1., 0.);

    // camera movement
    doCamera(camPos, camTar, camUp, m);

    // camera matrix
    mat3 camMat = calcLookAtMatrix( camPos, camTar, camUp );  // 0.0 is the camera roll

    // create view ray
    vec3 rd = normalize( camMat * vec3(p.xy,2.0) ); // 2.0 is the lens length

    Hit hit = raymarch(CastRay(camPos, rd));

    vec3 color = render(hit);

    #ifndef DEBUG
       color = linearToScreen(color);
    #endif
   // color = linearToScreen(color);
   
    fragColor = vec4(color,1.0);
}
