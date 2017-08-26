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
#define MOUSE_CONTROL 0

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
    ry = (xy.x) * 3. * PI;

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

#define GDFVector0 vec3(1, 0, 0)
#define GDFVector1 vec3(0, 1, 0)
#define GDFVector2 vec3(0, 0, 1)

#define GDFVector3 normalize(vec3(1, 1, 1 ))
#define GDFVector4 normalize(vec3(-1, 1, 1))
#define GDFVector5 normalize(vec3(1, -1, 1))
#define GDFVector6 normalize(vec3(1, 1, -1))

#define GDFVector7 normalize(vec3(0, 1, PHI+1.))
#define GDFVector8 normalize(vec3(0, -1, PHI+1.))
#define GDFVector9 normalize(vec3(PHI+1., 0, 1))
#define GDFVector10 normalize(vec3(-PHI-1., 0, 1))
#define GDFVector11 normalize(vec3(1, PHI+1., 0))
#define GDFVector12 normalize(vec3(-1, PHI+1., 0))

#define GDFVector13 normalize(vec3(0, PHI, 1))
#define GDFVector14 normalize(vec3(0, -PHI, 1))
#define GDFVector15 normalize(vec3(1, 0, PHI))
#define GDFVector16 normalize(vec3(-1, 0, PHI))
#define GDFVector17 normalize(vec3(PHI, 1, 0))
#define GDFVector18 normalize(vec3(-PHI, 1, 0))


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

float pReflect(inout vec3 p, vec3 planeNormal, float offset) {
    float t = dot(p, planeNormal)+offset;
    if (t < 0.) {
        p = p - (2.*t)*planeNormal;
    }
    return sign(t);
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



float wave1(vec3 p, vec3 v) {    
    float angle = acos(dot(normalize(p), v));
    float waveA = 0.;
    waveA += cos(angle * 6. * 2.)*4.;
    return waveA;
}


float wave(vec3 p, vec3 v) {    
    return wave1(p, v);
}

Model modelCe(vec3 p) {
    float scale= .8;
    p.z /= scale;
    float d = length(p) - .5;
    p.z -= .3;
    float part = length(p) - .5;
    d = max(d, -part);
    d *= scale;
    return Model(d, vec2(0), waterMaterial);
}

Model modelCx(vec3 p) {
    pReflect(p, vec3(0,0,1), 0.);
    float d = length(p) - .5;
    
    float part = 1e12;
    float s = 1.;
    float a = 1.45;
    part = min(part, length(p - GDFVector13 * a) - s);
    part = min(part, length(p - GDFVector14 * a) - s);
    part = min(part, length(p - GDFVector15 * a) - s);
    part = min(part, length(p - GDFVector16 * a) - s);
    part = min(part, length(p - GDFVector17 * a) - s);
    part = min(part, length(p - GDFVector18 * a) - s);
    d = max(d, -part);
    return Model(d, vec2(0), waterMaterial);
}

Model modelC(vec3 p) {
    Model model = newModel();

    float part;
    float d = 1e12;


    pR(p.xy, PI * .5);
    pR(p.xz, PI * .43);
    // pR(p.xy, PI * .5);
    
    float sep = .1;
    float sz = .5;
    float rr = .1;
    vec3 pp = p;

    p = pp;
    d = length(p) - .3;

    p.x -= sz + sep;
    part = length(p) - sz;
    d = smax(d, -part, rr);

    // p = pp;
    // p.x += sz + sep;
    // part = length(p) - sz;
    // d = smax(d, -part, rr);

    model.dist = d;

    return model;


    p.z += .07;
    float squish = 1.;
    p.z /= squish;
    part = length(p) - .3;
    d = part;
    p.z += .1;
    part = length(p) - .3;
    d = smax(d, -part, .02);
    d *= squish;
    model.dist = d;
    return model;


    // model.dist = fBox(p, vec3(.15)) - .05;

    // pR(p.xy, PI * .5);
    // model.dist = fTorus(p, .1, .2);

    // return model;

    // model.dist = length(p) - .5;
    // return model;
    
    // p.y /= .7;
    // model.dist = (length(p) - .5);
    // p.y -= .6;
    // model.dist = smax(model.dist, -(length(p) - .5), .2);

    // model.dist *= .7;
    
    // model.dist = fCapsule(p, vec3(0,0,-.15), vec3(0,0,.15), .15);

    // p.x += .15;
    // d = length(p) - .15;

    // p.x -= .3;
    // part = length(p) - .15;

    // model.dist = smin(d, part, .1);



    // float sep = .1;
    // float sz = .2;
    // float rr = .2;
    // vec3 pp = p;

    // p = pp;
    // d = length(p) - .3;

    // p.x -= sz + sep;
    // part = length(p) - sz;
    // d = smax(d, -part, rr);

    // p = pp;
    // p.x += sz + sep;
    // part = length(p) - sz;
    // d = smax(d, -part, rr);

    // model.dist = d;

    // return model;


    // vec2 wh = vec2(.15, .15);
    // pR(p.xy, PI * .5);
    // model.dist = min(
    //     fCylinder(p, wh.x, wh.y),
    //     fTorus(p, wh.y, wh.x)
    // );

    // return model;


    p *= 2.;

    vec3 a = vec3(1,0,0);
    float w = 0.;

    w += wave(p, GDFVector13);
    w += wave(p, GDFVector14);
    w += wave(p, GDFVector15);
    w += wave(p, GDFVector16);
    w += wave(p, GDFVector17);
    w += wave(p, GDFVector18);

    // w += wave(p, GDFVector3);
    // w += wave(p, GDFVector4);
    // w += wave(p, GDFVector5);
    // w += wave(p, GDFVector6);
    // // w += wave(p, GDFVector17);
    // // w += wave(p, GDFVector18);    
    
    float r = w * .005 + .6;
    model.dist = length(p) - r;

    model.dist = mix(model.dist, length(p) - .6, .0);

    // model.dist = fBox(p, vec3(.5));

    model.dist /= 2.;

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
    // pR(p.xy, .5);
    // pR(p.yz, PI / 2.);
    // pR(p.yz, time * PI * 2. - .8);
    // pR(camUp.yz, time * PI * 2.);


    float d = 1e12;
    float part;

    Model model = newModel();
    if ( ! enableTransparency) return model;

    // pR(p.zx, -.5);
    // pR(p.xz, -time * PI);

    model = modelC(p);
    
    if (insideTransparency) model.dist *= -1.;

    model.material = waterMaterial;
    return model;
}

Model map( vec3 p ){
    Model model = backModel(p);
    p *= modelRotation();
    // Model model;
    model = opU(model, mainModel(p));
    // model = opU(model, backModel(p));
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

float makeLines(float x, float lines, float thick) {
    x += .5;
    float start = thick * .5;
    float end = 1. - start;
    float aa = .001;
    float str = mod(x, 1. / lines) * lines;
    str = smoothstep(start, start - aa, str) + smoothstep(end, end + aa, str);
    return str;
}

void shadeSurface(inout Hit hit){
    
    vec3 color = vec3(.04,.045,.05);
    
    if (hit.isBackground) {
        hit.color = color;
        hit.color = hit.ray.direction * .5 + .5;
    }

    hit.color = hit.model.material.albedo;
}


CastRay transparencyCastRay(Hit hit, float refractiveIndex) {
    float separation = 0.05;    
    vec3 rayDirection, rayOrigin;
    float startDistance;    
    rayDirection = refract(hit.ray.direction, hit.normal, refractiveIndex);
    if (rayDirection == vec3(0)) {
        rayDirection = reflect(hit.ray.direction, hit.normal);
    }
    startDistance = separation / abs(dot(rayDirection, hit.normal));
    rayOrigin = hit.pos + startDistance * rayDirection;
    return CastRay(rayOrigin, rayDirection);
}

bool isInsideTransparentModel(vec3 pos) {
    Model model = map(pos);
    return model.dist < 0. && model.material.transparency > 0.;
}

const float REFRACT_SAMPLES = 10.; // max trace distance

Hit renderTransparency(Hit hit) {

    float riMax = hit.model.material.refractiveIndex;
    float riMin = riMax * .7;
    float refractiveIndex;

    float wl;
    Hit hitOutside;
    vec3 color = vec3(0);

    CastRay castRay;

    for(float i = 0.; i < REFRACT_SAMPLES; i++){

        wl = i / REFRACT_SAMPLES;
        refractiveIndex = mix(riMin, riMax, wl);

        // First march ray through to the other side

        castRay = transparencyCastRay(hit, refractiveIndex);
        insideTransparency = true;
        enableTransparency = true;
        Hit hitInside = raymarch(castRay);
        float thickness = hitInside.ray.len;
        
        // Then march back out into the scene
        
        castRay = transparencyCastRay(hitInside, 1. / refractiveIndex);
        insideTransparency = false;
        enableTransparency = false;
        hitOutside = raymarch(castRay);
        enableTransparency = true;
        
        // Shade the final model
        
        shadeSurface(hitOutside);
// 
        hitOutside.color *= spectrum(wl) * 2.;
        color += hitOutside.color;

    }

    color /= REFRACT_SAMPLES;
    hitOutside.color = color;

    // float core = thickness;
    // hitOutside.color = mix(hitOutside.color, vec3(1), core);
    
    return hitOutside;
}


void mixTransparency(inout Hit hit, Hit hit2) {
    hit.color = mix(hit.color, hit2.color, hit.model.material.transparency);
}


vec3 render(Hit hit){
    
    shadeSurface(hit);
    
    if (hit.isBackground) {
        return vec3(0);
        return hit.color;
    }

    // return hit.normal * .5 + .5;
    
    if (hit.model.material.transparency > 0.) {
        Hit hit2 = renderTransparency(hit);
        if ( ! hit2.isBackground && hit2.model.material.transparency > 0.) {
            Hit hit3 = renderTransparency(hit2);
            if ( ! hit3.isBackground && hit3.model.material.transparency > 0.) {
                Hit hit4 = renderTransparency(hit3);
                mixTransparency(hit3, hit4);
            }
            mixTransparency(hit2, hit3);
        }
        mixTransparency(hit, hit2);
    }

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



CastRay newCastRay(Hit hit, vec3 rayDirection) {
    float separation = 0.01;
    vec3 rayOrigin;
    float startDistance;
    startDistance = separation / abs(dot(rayDirection, hit.normal));
    rayOrigin = hit.pos + startDistance * rayDirection;
    return CastRay(rayOrigin, rayDirection);
}

const float REFRACT_BOUNCES = 5.;
const float REFRACT_SAMPLES_S = 400.;
const float DISPERSION = 1. - .5;
const float MULT = 20.;
#define ALLOW_ESCAPE

vec3 shade(Hit hit) {
    vec3 color = vec3(0,1,0);

    if (hit.isBackground) {
        color = hit.ray.direction;
        color = mod(color, 1./5.) * 5.;
        color = vec3(0.);
    } else if (hit.model.material.transparency == 0.) {
        vec2 uv = hit.model.uv;
        // pR(uv, time * PI * 2.);
        pR(uv, PI*.25);
        // float rep = mix(6., 7., sin(time) * .5 + .5);
        float rep = 7.;
        uv += time * (1. / rep) * vec2(1., 1.);
        // uv.x += .25;
        
        float size = .1;
        uv = mod(uv - rep * .5, 1. / rep) * rep;
        uv -= .5;
        float d = smoothstep(size, size * .8, length(uv));
        color = vec3(d);

        // color = vec3(0);
        // color += makeLines(uv.x, 1., .1) * mix(spectrum(.1), vec3(1), 1.);
        // color += makeLines(uv.y, 1., .1) * mix(spectrum(.4), vec3(1), 1.);

        // color = clamp(vec3(hit.model.uv, 0.).xzy * .5 + .5, 0., 1.);
        // color = hit.normal* .5 + .5;
        // color = vec3(1);
    }

    return color;
}


Hit marchTransparent(Hit hit, float wl) {
    enableTransparency = true;
    insideTransparency = false;
    
    for (float i = 0.; i < REFRACT_BOUNCES; i++) {
        if (hit.isBackground || hit.model.material.transparency == 0.) {
            return hit;
        } else {
            float refractiveIndex = hit.model.material.refractiveIndex;
            // float v = 1.05;
            float riMin = refractiveIndex * DISPERSION;
            float riMax = refractiveIndex;
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

void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
    mousee = iMouse.xy;

    // mousee = vec2(.257,.45) * iResolution;

    vec2 p = (-iResolution.xy + 2.0*fragCoord.xy)/iResolution.y;
    vec2 m = mousee.xy / iResolution.xy;

    pR(p, PI * -.25);

    time = iGlobalTime;
    time /= 2.;
    // time = mod(time, 1.);

    doCamera();

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
    vec3 sampleColour;

    if (hit.isBackground || hit.model.material.transparency == 0.) {
        // color = shade(hit);
    } else {
        for(float r = 0.; r < REFRACT_SAMPLES_S; r++){
            wl = r / REFRACT_SAMPLES_S;
            // wl = time;

            Hit hit2 = marchTransparent(hit, wl);
            color += (shade(hit2) * spectrum(wl)) / REFRACT_SAMPLES_S * MULT;
            // color += sampleColour / REFRACT_SAMPLES_S;
        }
    }

    color = linearToScreen(color);
    fragColor = vec4(color,1.0);
}


/*

* Create intial ray
* START LOOP
* Get hit
* Transparent?
** Y IS inside transparent flag?
*** Y Create exit refract ray
*** N Create enter refract ray
** IS TIF?
*** N Set inside transparent flag
** IS last loop
*** Y Unset inside transparent flag, disable models
** LOOP


*/