precision highp float;

uniform vec2 iResolution;
uniform vec2 iOffset;
uniform float iGlobalTime;
uniform vec4 iMouse;
uniform sampler2D iChannel0;

uniform bool guiLevel1Enabled;
uniform bool guiLevel2Enabled;
uniform bool guiLevel3Enabled;

uniform float guiLevel1Spacing;
uniform float guiLevel2Spacing;
uniform float guiLevel3Spacing;

uniform float guiLevel1Offset;
uniform float guiLevel2Offset;
uniform float guiLevel3Offset;

uniform float guiThickness;
uniform bool guiNormals;


void mainImage(out vec4 a, in vec2 b);

void main() {
    mainImage(gl_FragColor, gl_FragCoord.xy + iOffset.xy);
}

#ifdef GL_ES
precision mediump float;
#endif


/* SHADERTOY FROM HERE */

#pragma glslify: distanceMeter = require(./distance-meter.glsl)

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
// Modelling
// --------------------------------------------------------


#define saturate(x) clamp(x, 0., 1.)

// Rotate around a coordinate axis (i.e. in a plane perpendicular to that axis) by angle <a>.
// Read like this: R(p.xz, a) rotates "x towards z".
// This is fast if <a> is a compile-time constant and slower (but still practical) if not.
void pR(inout vec2 p, float a) {
    p = cos(a)*p + sin(a)*vec2(p.y, -p.x);
}

// http://www.neilmendoza.com/glsl-rotation-about-an-arbitrary-axis/
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



float vmax(vec2 v) {
    return max(v.x, v.y);
}

float vmax(vec3 v) {
    return max(max(v.x, v.y), v.z);
}

// Box: correct distance to corners
float fBox(vec3 p, vec3 b) {
    vec3 d = abs(p) - b;
    return length(max(d, vec3(0))) + vmax(min(d, vec3(0)));
}

float fBox2(vec2 p, vec2 b) {
    vec2 d = abs(p) - b;
    return length(max(d, vec2(0))) + vmax(min(d, vec2(0)));
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


// Repeat space along one axis. Use like this to repeat along the x axis:
// <float cell = pMod1(p.x,5);> - using the return value is optional.
float pMod1(inout float p, float size) {
    float halfsize = size*0.5;
    float c = floor((p + halfsize)/size);
    p = mod(p + halfsize, size) - halfsize;
    return c;
}

// Cylindrical coordinates
vec3 cartToPolar(vec3 p) {
    p = p.zxy;
    float r = length(p.xy); // distance from center
    float z = p.z; // distance from the plane it lies on
    float a = atan(p.y, p.x); // angle around center
    // r = pow(r, .7);
    return vec3(a, z, r);
}


vec3 cartToPolar2(vec3 p) {
    float x = p.x; // distance from the plane it lies on
    float a = atan(p.y, p.z); // angle around center
    float r = length(p.zy); // distance from center
    return vec3(x, a, r);
}

vec3 polarToCart(vec3 p) {
    return vec3(
        p.x,
        sin(p.y * (PI * 2.)) * p.z,
        cos(p.y * (PI * 2.)) * p.z
    );
}


vec2 closestPointOnLine(vec2 line, vec2 point){
    line = normalize(line);
    float d = dot(point, line);
    return line * d;
}

float globalScale;
bool debug = false;



vec3 closestSpiralA(vec3 p, inout vec3 debugP, float lead, float radius) {

    // pR(p.yz, (p.x / lead) * PI * 2.);

    p = cartToPolar2(p);
    // p.y += PI * 2. * (p.x / lead);
    p.y *= radius;
    // float x = p.x;
    // p.x = 0.;

    debugP = polarToCart(vec3(p.xy, radius));

    vec2 line = vec2(lead, radius * PI * 2.);
    vec2 closest = closestPointOnLine(line, p.xy);

    // closest.y -= PI * 2. * (x / lead);
    // closest.x = x;

    closest.y /= radius * 2. * PI;
    vec3 closestCart = polarToCart(vec3(closest, radius));

    // closestCart.x = x;

    return closestCart;
}

vec3 opU(vec3 p, vec3 m1, vec3 m2) {
    if (length(p - m1) < length(p - m2)) {
        return m1;
    } else {
        return m2;
    }
}



vec3 closestSpiral(vec3 p, inout vec3 debugP, float lead, float radius) {

    float c = pMod1(p.x, lead);
    vec3 pp = p;

    vec3 closestCartA = closestSpiralA(p, debugP, lead, radius);

    p.x += lead;
    vec3 closestCartB = closestSpiralA(p, debugP, lead, radius);
    closestCartB.x -= lead;

    p = pp;
    p.x -= lead;
    vec3 closestCartC = closestSpiralA(p, debugP, lead, radius);
    closestCartC.x += lead;

    p = pp;

    vec3 closestCart = opU(p, closestCartA, opU(p, closestCartB, closestCartC));

    closestCart.x += lead * c;

    return closestCart;
}



vec3 closestSpiral2(vec3 p, float lead, float radius) {
    vec3 cp = vec3(0);
    vec3 pp = p;

    vec3 closestA = closestSpiral(p, cp, lead, radius);

    p.yz *= vec2(-1);
    vec3 closestB = closestSpiral(p, cp, lead, radius);
    // closestB.yz *= vec2(-1);
    // pR(closestB.yz, PI);

    return closestA;

    p = pp;
    if (length(p - closestA) < length(p - closestB)) {
        return closestA;
    }
    return closestB;
}

vec3 pModSpiral(inout vec3 p, float flip, float lead, float radius) {
    vec3 a = vec3(0);
    vec3 closest = closestSpiral(p, a, lead, radius);
    float helixAngle = atan((2. * PI * radius) / lead);
    // float leadAngle = atan(lead / (2. * PI * radius));
    vec3 normal = normalize(closest - vec3(closest.x,0,0));
    vec3 tangent = vec3(1,0,0) * rotationMatrix(normal, helixAngle);
    float x = (closest.x / lead) * radius * PI * 2.;
    float y = dot(p - closest, cross(tangent, normal));
    float z = dot(p - closest, normal);
    p = vec3(x, y, z);
    return vec3(0.);
}


vec3 pModSpiral3(inout vec3 p, float flip, float lead, float radius) {

    float x = p.x;
    // p.x = 0.;

    float helixAngle = atan((2. * PI * radius) / lead);
    float leadAngle = atan(lead / (2. * PI * radius));
    vec3 axis = vec3(1, 0, 0);

    vec2 line = vec2(2. * PI * radius, lead);
    vec2 point = vec2(atan(p.y, p.z), p.x);
    vec2 closest = closestPointOnLine(line, point);

    float angle = (p.x / lead) * PI * 2.;
    // angle = closest.x;

    vec3 normal = vec3(0, sin(angle), cos(angle));
    vec3 tangent = cross(axis, normal) * rotationMatrix(normal, leadAngle);

    // p.x = 0.;
    pR(p.yz, angle);
    // pR(p.yx, helixAngle);
    
    // 
    p.z -= radius;


    return vec3(0., 0., 0.);
}

vec3 pModSpiral2(inout vec3 p, float flip, float lead, float radius) {

    float x = p.x;
    // p.x = 0.;

    float helixAngle = atan((2. * PI * radius) / lead);
    float leadAngle = atan(lead / (2. * PI * radius));
    vec3 axis = vec3(1, 0, 0);
    float angle = (p.x / lead) * PI * 2.;
    vec3 normal = vec3(0, sin(angle), cos(angle));
    vec3 tangent = cross(axis, normal) * rotationMatrix(normal, leadAngle);

    // p.x = x;
    p.x = 0.;
    pR(p.yz, angle);
    pR(p.yx, helixAngle);
    
    // 
    p.z -= radius;
    // p.x = x;
    
    
    // p -= normal * radius;
    // p.x = 0.;
    // p *= rotationMatrix(normal, helixAngle);

    return vec3(0., 0., 0.);
}

vec3 pModSpiralDebug(inout vec3 p, float flip, float lead, float radius) {

    float x = p.x;
    // p.x = 0.;

    float helixAngle = atan((2. * PI * radius) / lead);
    float leadAngle = atan(lead / (2. * PI * radius));
    vec3 axis = vec3(1, 0, 0);
    float angle = (p.x / lead) * PI * 2.;
    vec3 normal = vec3(0, sin(angle), cos(angle));
    vec3 tangent = cross(axis, normal) * rotationMatrix(normal, leadAngle);

    // p.x = x;
    // p.x = 0.;
    pR(p.yx, helixAngle);
    // pR(p.yz, angle);
    // 
    p.z -= radius;
    
    
    // p -= normal * radius;
    // p.x = 0.;
    // p *= rotationMatrix(normal, helixAngle);

    return vec3(0., 0., 0.);
}

vec3 xpModSpiral(inout vec3 p, float flip, float spacing, float zoffset) {
    float scale = .25;
    globalScale *= scale;

    float a = atan(spacing / PI) * -1.;

    // p.x *= spacing;

    // pMod1(p.y, PI * .5);


    p /= scale;
    p = p.yxz;

    p = cartToPolar(p);


    // a = PI * -.25;
    
    // p.x *= 3.;
    // p.y /= spacing;
    pR(p.xy, a * flip);

    

    float repeat = cos(a) * spacing * 2.;
    float c = pMod1(p.y, repeat);
    // float halfsize = repeat * .5;
    // float c = floor((p.y + halfsize) / repeat);
    // p.y = mod(p.y + halfsize, repeat) - halfsize;

    // float len = sqrt(pow(spacing, 2.) - pow(repeat, 2.));
    float len = sqrt(pow(spacing, 2.) + pow(PI, 2.));
    // float len = sqrt(pow(spacing, 2.) - pow(repeat, 2.));
    // float len = sqrt(pow(repeat, 2.) + pow(PI, 2.));

    float offset = repeat / tan(PI * .5 - a);
    p.x -= (offset + len * 2.) * c * flip;
    // p.x += len;
    

    // p.z -= mix(2., 10., sin(p.x * .1 + time * 3.) * .5 + .5);
    p.z -= zoffset;

    // p.x += time;
    // p.x -= 13.1 * c - (spacing * c * .75);
    // p.x -= PI * scale * .5;
    // float len = 

    return vec3(0., 0., 0.);
}


// Need to find nearest thread angle/position
// for x/phi position 
// mod(x, lead)
// 


struct Model {
    float dist;
    vec3 albedo;
    int id;
};


float debugDisplay(vec3 p, vec3 sample, vec3 closest, vec3 cp) {
    float d = 1e12;
    d = min(d, length(p - sample) - .1);
    // d = min(d, length(p - cp) - .2);
    d = min(d, length(p - closest) - guiThickness * 1.5);
    d = min(d, fCapsule(p, sample, closest, .05));
    // d = min(d, fCapsule(p, cp, closest, .05));
    return d;
}

Model prototype2d(vec3 p, float lead, float radius) {
    vec3 color = vec3(0);
    float d = 1e12;

    // float size = lead * .5;
    // float halfsize = size*0.5;
    // float c = floor((p.x + halfsize)/size);
    // p.x = mod(p.x + halfsize,size) - halfsize;
    // pR(p.yz, PI * mod(c, 2.));

    // p.x = mod(p.x - lead * .25, lead * .5) - lead * .25;

    vec3 pp = p;

    p = cartToPolar2(p.zyx);
    p.z -= radius;
    p.y *= radius * PI * 2.;


    // d = fBox(p, vec3(lead * .5, (radius * PI * 2.) * .5, .01));

    vec2 line = vec2(lead, radius * PI * 2.);
    vec2 closest = closestPointOnLine(line, p.xy);

    color.rg = p.xy / vec2(lead, 1.);
    color.rg = mod(color.rg, .5);
    color.b = p.y + .5;
    color = vec3(0);

    float e = length(p.xy - closest);
    // color.r = smoothstep(0., .1, e);

    p = pp;
    closest.y /= radius * 2. * PI;
    vec3 closestCart = polarToCart(vec3(closest, radius));
    // vec3 closestCart = vec3(closest, 0.);

    float f = length(p - closestCart);
    color.g = smoothstep(0., .1, f);
    // color = closestCart;


    // closest.x *= radius;
    // vec3 spiral = polarToCart(vec3(closest, radius));
    // p = pp;
    d = min(d, length(p - closestCart) - guiThickness);

    // color = vec3(p.x / lead);

    return Model(d, color, 1);
}



Model map( vec3 p ){
    // pR(p.zy, time * 8.);
    mat3 m = modelRotation();

    float slice = dot(p, vec3(0,1,0));

    float lead = guiLevel1Spacing;
    float radius = guiLevel1Offset;

    // return prototype2d(p, lead, radius);
    vec3 pp = p;

    vec3 a = vec3(0);
    vec3 cps = closestSpiral(p, a, lead, radius);
    float sp = length(p - cps) - guiThickness;


    if (guiLevel1Enabled) {
        pModSpiral(p, 1., guiLevel1Spacing, guiLevel1Offset);
        if (guiLevel2Enabled) {
            pModSpiral(p, -1., guiLevel2Spacing, guiLevel2Offset);
            if (guiLevel3Enabled) {
                pModSpiral(p, 1., guiLevel3Spacing, guiLevel3Offset);
            }
        }
    }
    float d = length(p.yz) - guiThickness;

    vec3 color = vec3(
        smoothstep(0., .1, sin(p.x * 20.)),
        sin(p.x / 1.5),
        sin(p.x / 3.)
    );

    // p = pp;
    // pModSpiralDebug(p, 1., guiLevel1Spacing, guiLevel1Offset);
    // d = min(d, length(p.yz) - guiThickness);


    // d = min(d, sp);

    // d = sp;


    p = pp;

   
    float dim = lead * 2.;
    dim = mix(-dim, dim, time);
    vec3 sample = vec3(dim, 0, 0);
    // sample.y = sample.z = 0.;
    // sample.z = radius * -1.;
    vec3 cp = vec3(0);
    vec3 closest = closestSpiral(sample, cp, lead, radius);
    // d = min(d, debugDisplay(p, sample, closest, cp));

    // pR(sample.yz, PI);
    // closest = closestSpiral(sample, cp, lead, radius);
    // d = min(d, debugDisplay(p, sample, closest, cp));

    // pR(sample.yz, PI * -1.);
    // sample.x += lead * .25;
    // closest = closestSpiral(sample, cp, lead, radius);
    // pR(sample.yz, PI * .5);
    // closest.x -= lead * .25;
    // pR(closest.yz, PI * .5);
    // sample.x -= lead * .25;
    // pR(cp.yz, PI * .5);
    // cp.x -= lead * .25;
    // d = min(d, debugDisplay(p, sample, closest, cp));

    // pR(p.xy, PI * .5);
    // d = min(d, fCylinder(p, .05, lead / 2.));

    

    // d = max(d, slice);
    // d = slice;


    Model model = Model(d, color, 1);

    Model meter = Model(slice, color, 0);

    if (slice < d) {

    }

    return model;
}

Model mapDebug(vec3 p) {

    float d = abs(dot(p, vec3(0,1,0)) - time * 10. + 5.) - .001;
    Model model = map(p);

    return model;

    if (model.dist < d) {
        return model;
    }

    return Model(d, vec3(0), 0);
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
    camPos = vec3(0,0,-50.);
    camPos *= cameraRotation();
}



// --------------------------------------------------------
// Ray Marching
// Adapted from: https://www.shadertoy.com/view/Xl2XWt
// --------------------------------------------------------

const float MAX_TRACE_DISTANCE = 100.; // max trace distance
const float INTERSECTION_PRECISION = .00001; // precision of the intersection
const int NUM_OF_TRACE_STEPS = 1000;
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
        model = mapDebug(ray.origin + ray.direction * ray.len);
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
// --------------------------------------------------------

void shadeSurface(inout Hit hit){

    vec3 background = vec3(.1)* vec3(.5,0,1);
    background = vec3(.2,.8,1.) * .9;
    if (hit.isBackground) {
        hit.color = background;
        return;
    }

    if (hit.model.id == 0) {
        float dist = map(hit.pos).dist;
        hit.color = distanceMeter(dist * 2., hit.ray.len, hit.ray.direction, 50.);
        return;
    }

    pR(hit.normal.xz, 2.75);
    if (guiNormals) {
        hit.color = hit.normal * -.5 + .5;
    } else {
        hit.color = hit.model.albedo;
    }
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
    mousee = iMouse.xy;

    vec2 p = (-iResolution.xy + 2.0*fragCoord.xy)/iResolution.y;
    vec2 m = mousee.xy / iResolution.xy;

// debug = true;
    if (p.y < 0.) {
        debug = true;
    }

    time = iGlobalTime;

    doCamera();

    mat3 camMat = calcLookAtMatrix(camPos, camTar, camUp);
    float focalLength = 3.;
    vec3 rd = normalize(camMat * vec3(p, focalLength));
    Hit hit = raymarch(CastRay(camPos, rd));

    vec3 color = render(hit);
    color = linearToScreen(color);
    fragColor = vec4(color,1.0);
}
