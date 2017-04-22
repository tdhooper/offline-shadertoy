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
        vec2 mouse = iMouse.xy / iResolution.xy;

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
float ballSize = .5;
float stepSpeed = .5;

const float initialStep = 1.;
const float MODEL_STEPS = 3.;

float squareSine(float x, float e) {
    //return sin(x);
    x = mod(x, PI * 2.);
    float period = x / mod((PI / 2.), 4.);
    float a = pow(period - 3., e) - 1.;
    float b = -pow(period - 1., e) + 1.;
    return period > 2. ? a : b;
}

float makeAnim(float localTime) {
    float blend = localTime / stepDuration * stepSpeed;
    blend = clamp(blend, 0., 1.);
    blend = squareSine(blend * PI - PI * .5, 2.) * .5 + .5;
    return blend;    
}

float makeOffsetAmt(vec3 p, float localTime) {
    float moveMax = stepMove;
    float blend = makeAnim(localTime);
    blend = clamp(blend, 0., 1.);
    //blend = squareSine(blend * PI - PI * .5, 2.) * .5 + .5;
    return mix(.0, moveMax, blend);    
}

float makeModel(vec3 p, float localTime, float scale) {
    float d, part;
    
    float blend = makeAnim(localTime);

    float size = mix(ballSize, ballSize * stepScale, blend);

    p /= scale;
    fold(p);

    part = length(p) - size;
    d = part;

    p -= triV.c * blend * stepMove;

    part = length(p) - size;
    d = min(d, part);

    //d = min(d, dot(p, triV.a) - amt * scale * .8);

    d *= scale;

    return d;
/*
    p = pp;
    vec3 rPlane = normalize(cross(triV.b, triV.a));
    vec3 n = reflect(triV.c, rPlane);
    part = length(p - n * amt) - size;
    d = smin(d, part, r);

    n = reflect(n, triP.ca);
    part = length(p - n * amt) - size;
    d = smin(d, part, r);

    //d = min(original, d);
    //d = mix(original, d, blend2);


    return d;
*/
}

void makeSpace(inout vec3 p, float localTime, float scale) {
    float blend = makeAnim(localTime);
    p /= scale;
    if (length(p) > blend * stepMove * .525) {
        fold(p);
        p -= triV.c * blend * stepMove;
    }
    p *= scale;
}

float hardstep(float a, float b, float t) {
    float s = 1. / (b - a);
    return clamp((t - a) * s, 0., 1.);
}

vec3 camPath(float t) {
    //t = t - .25;

    t = pow(t, 1.8);

    vec3 A = vec3(-1.2,0,-6);
    vec3 B = vec3(-1.2,0,0);
    vec3 C = vec3(1.8,8,-6);
    vec3 D = vec3(1.8,8,0);

    vec3 startPoint = vec3(0);
    vec3 endPoint = triV.c * (pow(1./stepScale, 3.) * stepMove);

    vec3 lookLine = normalize(-triV.c - vec3(0,.5,0));
    vec3 offsetA = lookLine * 3.;
    vec3 offsetB = lookLine * 1.;
    vec3 offsetC = offsetA + offsetB;
    vec3 offsetD = offsetA;

    A = startPoint + offsetA;
    B = startPoint + offsetB;
    C = endPoint + offsetC;
    D = endPoint + offsetD;

    //end = vec3(20);

    //return triV.c * (1./stepScale * stepMove);
    
  
    vec3 pathA = mix(A, B, hardstep(0., .5, t));
    vec3 pathB = mix(B, C, hardstep(0., 1., t));
    vec3 pathC = mix(C, D, hardstep(.5, 1., t));
   
  //  return pathA;

    vec3 pos = mix(
        mix(pathA, pathB, smoothstep(.0, .5, t)),
        mix(pathB, pathC, smoothstep(.5, 1., t)),
        step(.5, t)
    );

    //pos = pathA;

//    vec3 x = normalize(cross(triV.a, start - end));
  //  vec3 y = normalize(cross(x, start - end));

    //vec3 posTar = pos;
    //pos += x * sin(t * PI * 2.) * 2.5;
    //pos -= y * cos(t * PI * 2.) * 2.5;

    return pos;
}

float modelScale;

float subDModel(vec3 p) {

    //pReflect(p, -triV.c, camOffset);

    float scale = 1.;
    float level = -1.;

   p /= modelScale;

    float d;

    //float time = .1;

    // p = mod(p + .5, 1.) - .5;
    // d = length(p - vec3(.0,0,0)) - .1;
    // d *= modelScale;
    // return d;

    //float time = 1.;


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
        //0.
        makeAnim(time - (stepDuration * (level - 1.)))
    );

    d = makeModel(p, time - (stepDuration * level), scale);
    //float part = makeModel(pp, time - (stepDuration * level), scale);
    //d = min(d, part);

    d *= modelScale;
    return d;
}

Model map( vec3 p ){
    mat3 m = modelRotation();
    //p *= m;
    Model model = Model(subDModel(p), 1.);
    //model = Model(makeModel(p, time), 1.);
    //model.dist = length(p - facePlane) - .1;
    return model;
}


float camDist;
vec3 camTar;




void doCamera(out vec3 camPos, out vec3 camTar, out vec3 camUp, in vec2 mouse) {
    float x = time / loopDuration;
    float apex = .7;
    float blend = smoothstep(0., apex, x) - smoothstep(apex, 1., x);
    camDist = mix(5., 30., blend);


    blend = pow(x, 1.8);

    modelScale = mix(
        1.,
        1. / pow(stepScale, MODEL_STEPS),
        blend
    );
    //modelScale = 1.;
    
 
    camDist = 5. * modelScale;
    
    blend = smoothstep(.0, 1., x);
    camDist = mix(camDist, camDist * pow(stepScale, MODEL_STEPS), blend);
     

    //camDist = 10.;
    //camDist = 1.;
    //camDist = mix(camDist, camDist * pow(stepScale, MODEL_STEPS), pow(x, .4));

    camTar = vec3(0.);
    //camTar = -triV.c * camOffset;

    //camTar = triV.c * -makeOffsetAmt(vec3(0.), time) * 2.;
    
    //camRoll = (sin(PI * x) * .5 + .5) * PI * -.3;
    //camRoll = 0.;
    camUp = normalize(vec3(0,-1,0));
    //pR(camUp.xy, x * PI * 2.);

    camDist = 3.;

    camPos = vec3(0,0,-camDist);

    vec3 startTar = vec3(0);
    vec3 endTar = triV.c * (stepMove * (1./stepScale));
    
    apex = .333;
    float overshoot = 0.;
    blend = smoothstep(0., apex, x) * (1. + overshoot);
    blend -= smoothstep(apex, 1., x) * overshoot;

    blend = smoothstep(0., 1., x);
    camTar = mix(startTar, endTar, blend) * modelScale;

    camPos = camPath(x);
    camTar = camPath(x + .001);

    //camTar = vec3(0.);

    //camUp = normalize(cross(camUp, camTar - camPos));
    //pR(camTar.yx, x * PI * -2.);    

    //pR(camPos.xz, x * PI * 2.);    
    
    // camTar = vec3(0.);
    // camPos = vec3(0,0,10);
    // camPos *= cameraRotation();
    // camPos += camTar;

}



// --------------------------------------------------------
// Ray Marching
// Adapted from: https://www.shadertoy.com/view/Xl2XWt
// --------------------------------------------------------

const float MAX_TRACE_DISTANCE = 500.; // max trace distance
const float INTERSECTION_PRECISION = .001; // precision of the intersection
const int NUM_OF_TRACE_STEPS = 100;
const float FUDGE_FACTOR = .9; // Default is 1, reduce to fix overshoots

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

    diffuse *= mix(colA, colB, blend);
    diffuse = sin(diffuse);
    diffuse *= 1.3;
    
    float fog = clamp((hit.ray.len - 5.) * .5, 0., 1.);
    fog = mix(0., 1., length(camTar - hit.pos) / camDist) * .5;
    fog = clamp(fog, 0., 1.);
    
    //diffuse = hit.normal * .5 + .5;
    fog = 0.;

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
    //time /= 2.;
    //time = mod(time, 1.);

    vec2 p = (-iResolution.xy + 2.0*fragCoord.xy)/iResolution.y;
    vec2 m = iMouse.xy / iResolution.xy;

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
