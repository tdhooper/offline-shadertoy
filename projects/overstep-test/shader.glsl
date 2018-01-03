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
// Modelling
// --------------------------------------------------------

struct Model {
    float dist;
};

    
// checks to see which intersection is closer
Model opU( Model m1, Model m2 ){
    if (m1.dist < m2.dist) {
        return m1;
    } else {
        return m2;
    }
}


Model map( vec3 p ){
    mat3 m = modelRotation();
    fold(p);
    p -= triV.a * 5.;
    float d = length(p) - .1;
    return Model(d);
}


vec3 camPos;
float camDist;
vec3 camTar;


void doCamera(out vec3 camPos, out vec3 camTar, out vec3 camUp, in vec2 mouse) {
    camDist = 10.;
    camUp = vec3(0,-1,0);
    camTar = vec3(0.);
    camPos = vec3(0,0,-camDist);
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

    if (hit.isBackground) {
        hit.color = background;
        return;
    }

    hit.color = hit.normal * .5 + .5;
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

    mousee = iMouse.xy;

    vec2 p = (-iResolution.xy + 2.0*fragCoord.xy)/iResolution.y;
    vec2 m = mousee.xy / iResolution.xy;
    
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
