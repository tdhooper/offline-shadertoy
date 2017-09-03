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
#define MOUSE_CONTROL 1

#define DEBUG

float time;

#define PI 3.14159265359
#define HALF_PI 1.5707963267948966
#define TAU 6.28318530718
#define PHI 1.618033988749895

// #define SHOW_SHAPE


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
    ry = (xy.x) * 2. * PI;

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
// https://www.shadertoy.com/view/Xs3GRB
// --------------------------------------------------------



#define saturate(x) clamp(x, 0., 1.)

float vmax(vec3 v) {
    return max(max(v.x, v.y), v.z);
}

// Rotate around a coordinate axis (i.e. in a plane perpendicular to that axis) by angle <a>.
// Read like this: R(p.xz, a) rotates "x towards z".
// This is fast if <a> is a compile-time constant and slower (but still practical) if not.
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


// Plane with normal n (n is normalized) at some distance from the origin
float fPlane(vec3 p, vec3 n, float distanceFromOrigin) {
    return dot(p, n) + distanceFromOrigin;
}

// Box: correct distance to corners
float fBox(vec3 p, vec3 b) {
    vec3 d = abs(p) - b;
    return length(max(d, vec3(0))) + vmax(min(d, vec3(0)));
}

// Cylindrical coordinates
vec3 cartToPolar(vec3 p) {
    float r = length(p.xy); // distance from center
    float z = p.z; // distance from the plane it lies on
    float a = atan(p.y, p.x); // angle around center
    return vec3(r, z, a);
}

// Cylindrical coordinates
vec3 cartToSpherical(vec3 p) {
    float r = length(p); // distance from center
    float z = acos(p.z / r); // Inclination
    float a = atan(p.y, p.x); // angle around center
    return vec3(r, z, a);
}

float pReflect(inout vec3 p, vec3 planeNormal, float offset) {
    float t = dot(p, planeNormal)+offset;
    if (t < 0.) {
        p = p - (2.*t)*planeNormal;
    }
    return sign(t);
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


// Torus in the XZ-plane
float fTorus(vec3 p, float smallRadius, float largeRadius) {
    return length(vec2(length(p.xz) - largeRadius, p.y)) - smallRadius;
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

// Cylinder standing upright on the xz plane
float fCylinder(vec3 p, float r, float height) {
    float d = length(p.xz) - r;
    d = max(d, abs(p.y) - height);
    return d;
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

float fCone(vec3 p, float radius, float height, vec3 direction) {
    return fCone(p, radius, height, direction, 0.);
}

float fConeI(vec3 p, float radius, float height, vec3 direction) {
    return fCone(p, radius, height, -direction, -height);
}

float fConeI(vec3 p, float radius, float height, vec3 direction, float offset) {
    return fCone(p, radius, height, -direction, -height - offset);
}

float fCone(vec3 p, float radius, vec3 start, vec3 end) {
    float height = length(start - end);
    vec3 direction = normalize(end - start);
    return fCone(p - start, radius, height, direction);
}

// --------------------------------------------------------
// https://github.com/stackgl/glsl-inverse
// --------------------------------------------------------

mat3 inverse(mat3 m) {
  float a00 = m[0][0], a01 = m[0][1], a02 = m[0][2];
  float a10 = m[1][0], a11 = m[1][1], a12 = m[1][2];
  float a20 = m[2][0], a21 = m[2][1], a22 = m[2][2];

  float b01 = a22 * a11 - a12 * a21;
  float b11 = -a22 * a10 + a12 * a20;
  float b21 = a21 * a10 - a11 * a20;

  float det = a00 * b01 + a01 * b11 + a02 * b21;

  return mat3(b01, (-a22 * a01 + a02 * a21), (a12 * a01 - a02 * a11),
              b11, (a22 * a00 - a02 * a20), (-a12 * a00 + a02 * a10),
              b21, (-a21 * a00 + a01 * a20), (a11 * a00 - a01 * a10)) / det;
}



// --------------------------------------------------------
// http://math.stackexchange.com/a/897677
// --------------------------------------------------------

mat3 orientMatrix(vec3 A, vec3 B) {
    mat3 Fi = mat3(
        A,
        (B - dot(A, B) * A) / length(B - dot(A, B) * A),
        cross(B, A)
    );
    mat3 G = mat3(
        dot(A, B),              -length(cross(A, B)),   0,
        length(cross(A, B)),    dot(A, B),              0,
        0,                      0,                      1
    );
    return Fi * G * inverse(Fi);
}


// --------------------------------------------------------
// http://www.neilmendoza.com/glsl-rotation-about-an-arbitrary-axis/
// --------------------------------------------------------

mat3 rotationMatrix(vec3 axis, float angle)
{
    axis = normalize(axis);
    float s = sin(angle);
    float c = cos(angle);
    float oc = 1.0 - c;

    return mat3(
        oc * axis.x * axis.x + c,           oc * axis.x * axis.y - axis.z * s,  oc * axis.z * axis.x + axis.y * s,
        oc * axis.x * axis.y + axis.z * s,  oc * axis.y * axis.y + c,           oc * axis.y * axis.z - axis.x * s,
        oc * axis.z * axis.x - axis.y * s,  oc * axis.y * axis.z + axis.x * s,  oc * axis.z * axis.z + c
    );
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







float makeDots(vec2 uv, float repeat, float size) {
    uv = mod(uv, 1. / repeat) * repeat;
    uv -= .5;
    return smoothstep(size, size * .8, length(uv));
}

float makeLine(float x, float thick) {
    float start = .5 - thick * .5;
    float end = .5 + thick * .5;
    float aa = .01;
    float str = x;
    str = smoothstep(start, start + aa, str) - smoothstep(end -aa, end, str);
    return str;
}

float makeLines(float x, float repeat, float thick) {
    x = mod(x, 1. / repeat) * repeat;
    return makeLine(x, thick);
}




// --------------------------------------------------------
// Materials
// --------------------------------------------------------

struct Material {
    vec3 albedo;
    bool specular;
    float transparency;
    float refractiveIndex;
    float reflection;
};


Material ceramicMaterial = Material(
    vec3(.5),
    false,
    0.,
    0.,
    0.
);  
Material waterMaterial = Material(
    vec3(0.),
    true,
    1.,
    1. / 1.333,
    // 1. / 1.1,
    // 1. / 1.01,
    0.
);
Material mirrorMaterial = Material(
    vec3(.7, .3, .0),
    false,
    0.,
    0.,
    1.
);


// --------------------------------------------------------
// Modelling
// --------------------------------------------------------

bool insideTransparency = false;
bool enableTransparency = true;

struct Model {
    float dist;
    vec2 uv;
    Material material;
};

Model newModel() {
    return Model(
        1e12,
        vec2(0),
        ceramicMaterial
    );
}

// checks to see which intersection is closer
Model opU( Model m1, Model m2 ){
    if (m1.dist < m2.dist) {
        return m1;
    } else {
        return m2;
    }
}



Model modelC(vec3 p) {
    Model model = newModel();

    float part;
    float d = 1e12;

    float rep = 10.;

    pR(p.xz, PI * .5);

    pModPolar(p.yz, rep);
    vec3 pp = p;
    
    float bumpSize = .05;
    float bumpSmooth = .1;
    float bumpOffset = .25;


    p.y -= bumpOffset;
    float bA = length(p) - bumpSize;

    p = pp;
    pR(p.yz, PI / rep * 2.);
    p.y -= bumpOffset;
    float bB = length(p) - bumpSize;

    p = pp;
    pR(p.yz, PI / rep * -2.);
    p.y -= bumpOffset;
    float bC = length(p) - bumpSize;

    float bumps = min(min(bA, bB), bC);

    model.dist  = bumps;

    // return model;

    p = pp;

// 

    // 
    // float sqq = 1.;

    // p.z /= sqq;

    float sep = .1;
    float sz = .2;
    float rr = .2;
    pp = p;

    p = pp;
    d = length(p) - .3;

    p.x -= sz + sep;
    part = length(p) - sz;
    d = smax(d, -part, rr);

    p = pp;
    p.x += sz + sep;
    part = length(p) - sz;
    d = smax(d, -part, rr);

    // d *= sqq;

    d = smin(d, bA, bumpSmooth);
    d = smin(d, bB, bumpSmooth);
    d = smin(d, bC, bumpSmooth);

    model.dist = d;

    return model;
}

Model backModel(vec3 p) {
    p.z -= 1.;
    float sphere = length(p) - 2.;
    float plane = dot(p, vec3(0,0,1));
    float d = max(plane, -sphere);

    p.z += 3.;
    // d = fBox(p, vec3(1.9,1.9,.9));
    d = dot(p, vec3(0,0,1)) - .7;

    return Model(
        d,
        vec2(p.x, p.y),
        ceramicMaterial
    );
}


Model mainModel(vec3 p) {


    float d = 1e12;
    float part;

    Model model = newModel();
    if ( ! enableTransparency) return model;


    model = modelC(p);
    
    if (insideTransparency) model.dist *= -1.;

    model.material = waterMaterial;
    return model;
}

Model map( vec3 p ){
    Model model = backModel(p);
    p *= modelRotation();
    model = opU(model, mainModel(p));
    return model;
}

// --------------------------------------------------------
// Camera
// --------------------------------------------------------

vec3 camPos;
vec3 camTar;
vec3 camUp;


void doCamera() {
    camUp = vec3(0,-1,0);
    camTar = vec3(0.);
    camPos = vec3(0,0,-1.);
    camPos *= cameraRotation();
}



// --------------------------------------------------------
// Ray Marching
// Adapted from: https://www.shadertoy.com/view/Xl2XWt
// --------------------------------------------------------

const float MAX_TRACE_DISTANCE = 5.; // max trace distance
const float INTERSECTION_PRECISION = .001; // precision of the intersection
const int NUM_OF_TRACE_STEPS = 50;
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

    if (ray.len > MAX_TRACE_DISTANCE) {
        isBackground = true;
    } else {
        pos = ray.origin + ray.direction * ray.len;
        normal = calcNormal(pos);
    }

    return Hit(ray, model, pos, isBackground, normal, vec3(0));
}


// --------------------------------------------------------
// Rendering
// Refraction from https://www.shadertoy.com/view/lsXGzH
// --------------------------------------------------------




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



CastRay newCastRay(Hit hit, vec3 rayDirection) {
    float separation = 0.01;
    vec3 rayOrigin;
    float startDistance;
    startDistance = separation / abs(dot(rayDirection, hit.normal));
    rayOrigin = hit.pos + startDistance * rayDirection;
    return CastRay(rayOrigin, rayDirection);
}

const float REFRACT_BOUNCES = 4.;
const float REFRACT_SAMPLES_S = 20.;
float DISPERSION = mix(0., 1., mod(iGlobalTime / 4., 1.));
const float MULT = 5.;
#define ALLOW_ESCAPE

vec3 shade(Hit hit) {
    vec3 color = vec3(0,1,0);

    if (hit.isBackground) {
        color = hit.ray.direction;
        color = mod(color, 1./5.) * 5.;
        color = vec3(0.);
    } else if (hit.model.material.transparency == 0.) {
        float rep = 6.;
        vec2 uv = hit.model.uv;

        float e = uv.y;

        color = vec3(0);
        float repeat = 5.;
        color += makeDots(uv + .5 / repeat, repeat, .1);
        color += makeLines(uv.x, repeat, .025);
        color += makeLines(uv.y, repeat, .025);
    }

    return color;
}

Hit marchTransparent(Hit hit, float wl) {
    enableTransparency = true;
    insideTransparency = false;

    float DISPERSION = mix(.1, .3, smoothstep(.1, .4, time) - smoothstep(.6, .9, time));
     
    DISPERSION = .15;

    for (float i = 0.; i < REFRACT_BOUNCES; i++) {
        if (hit.isBackground || hit.model.material.transparency == 0.) {
            return hit;
        } else {
            float refractiveIndex = hit.model.material.refractiveIndex;
            // float v = 1.05;
            float riMin = refractiveIndex;
            float riMax = refractiveIndex * (1. + DISPERSION);
            refractiveIndex = mix(riMin, riMax, wl);

            if (insideTransparency) {
                refractiveIndex = 1. / refractiveIndex;
            }
            vec3 rayDirection = refract(hit.ray.direction, hit.normal, refractiveIndex);
            if (rayDirection == vec3(0)) {
                rayDirection = reflect(hit.ray.direction, hit.normal);
            } else {
                insideTransparency = ! insideTransparency;
            }
            #ifdef ALLOW_ESCAPE
                if (i == REFRACT_BOUNCES - 1.) {
                    enableTransparency = false;
                    insideTransparency = false;
                }
            #endif
            CastRay castRay = newCastRay(hit, rayDirection);
            hit = raymarch(castRay);
        }
    }
    return hit;
}

vec3 getColor(vec2 p) {
    mat3 camMat = calcLookAtMatrix(camPos, camTar, camUp);
    float focalLength = 2.;
    vec3 rd = normalize(camMat * vec3(p, focalLength));
    float refractiveIndex;

    CastRay castRay = CastRay(camPos, rd);

    Hit hit;
    vec3 rayDirection;

    hit = raymarch(castRay);
    float wl, riMax, riMin;
    // wl = time;
    wl = 0.;

    vec3 color = vec3(0);
    // vec3 sampleColour;

    #ifdef SHOW_SHAPE
        return hit.normal * .5 + .5;
    #endif

    if (hit.isBackground || hit.model.material.transparency == 0.) {
        color = shade(hit);
    } else {
        for(float r = 0.; r < REFRACT_SAMPLES_S; r++){
            wl = r / REFRACT_SAMPLES_S;
            // wl = time;

            Hit hit2 = marchTransparent(hit, wl);
            color += (shade(hit2) * spectrum(wl)) / REFRACT_SAMPLES_S * MULT;
            // color += sampleColour / REFRACT_SAMPLES_S;
        }
    }

    return color;
}

void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
    mousee = iMouse.xy;

    // mousee = vec2(.257,.45) * iResolution;

    vec2 p = (-iResolution.xy + 2.0*fragCoord.xy)/iResolution.x;
    vec2 m = mousee.xy / iResolution.xy;

    time = iGlobalTime;
    time /= 4.;
    time = mod(time, 1.);

    doCamera();

    vec3 color = getColor(p);

    color = linearToScreen(color);
    fragColor = vec4(color,1.0);
}


