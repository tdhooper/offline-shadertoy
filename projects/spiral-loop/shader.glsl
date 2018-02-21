precision highp float;

uniform vec2 iResolution;
uniform vec2 iOffset;
uniform float iGlobalTime;
uniform vec4 iMouse;
uniform sampler2D iChannel0;

uniform float guiLead;
uniform float guiInnerRatio;
uniform bool guiNormals;
uniform float guiDebug;
uniform float guiMix;
uniform float guiOffsetX;
uniform float guiOffsetY;
uniform float guiZoom;
uniform float guiRotateX;
uniform float guiRotateY;
uniform float guiRotateModel;
uniform float guiRotateModelX;
uniform bool guiFlip;
uniform float guiNormalX;
uniform float guiNormalY;
uniform float guiFocal;
uniform float guiZipOffset;
uniform float guiZipSize;
uniform float guiZipSpeed;


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
        mouse = vec2(guiRotateX, guiRotateY);

        // mouse = vec2(0.6621212121212121, 0.5879120879120879);

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


float range(float vmin, float vmax, float value) {
  return (value - vmin) / (vmax - vmin);
}

float rangec(float a, float b, float t) {
    return clamp(range(a, b, t), 0., 1.);
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

float fBox2(vec2 p, vec2 b, float round) {
    return fBox2(p, b * (1. - round)) - round * vmax(b);
}

float fBox2(inout float side, vec2 p, vec2 b) {
    vec2 d = abs(p) - b;
    // side = sign(d.x - d.y) + 1.;
    // side = max(0., sign(vmax(d)))
    if (d.x > d.y) {
        side = max(0., sign(p.x));
    } else {
        side = max(0., sign(p.y)) + 2.;
    }
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
        sin(p.y) * p.z,
        cos(p.y) * p.z
    );
}


vec2 closestPointOnLine(vec2 line, vec2 point){
    line = normalize(line);
    float d = dot(point, line);
    return line * d;
}

float globalScale;
bool debug = false;


vec3 closestSpiralB(vec3 p, float lead, float radius) {

    p = cartToPolar2(p);
    p.y *= radius;

    vec2 line = vec2(lead, radius * PI * 2.);
    vec2 closest = closestPointOnLine(line, p.xy);

    closest.y /= radius;
    vec3 closestCart = polarToCart(vec3(closest, radius));

    return closestCart;
}

vec3 closestSpiralA(vec3 p, float lead, float radius) {
    // float flip = max(0., sign(dot(p, vec3(0,0,-1))));
    // pR(p.yz, PI * flip);
    vec3 s1 = closestSpiralB(p, lead, radius);
    // pR(s1.yz, PI * -flip);
    return s1;
}

vec3 opU(vec3 p, vec3 m1, vec3 m2) {
    if (length(p - m1) < length(p - m2)) {
        return m1;
    } else {
        return m2;
    }
}

vec3 closestSpiral(vec3 p, float lead, float radius) {

    float c = pMod1(p.x, lead);
    vec3 pp = p;

    vec3 closestCartA = closestSpiralA(p, lead, radius);

    p.x += lead;
    vec3 closestCartB = closestSpiralA(p, lead, radius);
    closestCartB.x -= lead;

    p = pp;
    p.x -= lead;
    vec3 closestCartC = closestSpiralA(p, lead, radius);
    closestCartC.x += lead;

    p = pp;

    vec3 closestCart = opU(p, closestCartA, opU(p, closestCartB, closestCartC));

    closestCart.x += lead * c;

    return closestCart;
}

vec3 pModSpiral(inout vec3 p, float flip, float lead, float radius) {
    vec3 closest = closestSpiral(p, lead, radius);
    float helixAngle = atan((2. * PI * radius) / lead);
    vec3 normal = normalize(closest - vec3(closest.x,0,0));
    vec3 tangent = vec3(1,0,0) * rotationMatrix(normal, helixAngle);
    float x = (closest.x / lead) * radius * PI * 2.;
    float y = dot(p - closest, cross(tangent, normal));
    float z = dot(p - closest, normal);
    p = vec3(x, y, z);
    return vec3(0.);
}

float pModHelix(inout vec3 p, float lead, float innerRatio) {
    float radius = mix(.25, .5, innerRatio);
    pModSpiral(p, 1., lead, radius);
    float scale = mix(.5, 0., innerRatio);
    p /= scale;
    return 1. / scale;
}

float pModHelixUnwrap(inout vec3 p, float lead, float innerRatio, float t) {
    float radius = mix(.25, .5, innerRatio);
    float width = cos(asin(t));
    float adjust = (1. / width);
    float offset = ((.5 * adjust) - .5) * 7.;

    vec3 pp = p;
    pp.z -= radius;
    pR(pp.xy, PI * -.5);
    pp.x *= -1.;

    p.z += offset;
    radius += offset;
    pModSpiral(p, 1., lead, radius);

    p = mix(p, pp, rangec(.8, 1., t));

    float scale = mix(.5, 0., innerRatio);
    p /= scale;
    return 1. / scale;
}


struct Model {
    float dist;
    vec3 albedo;
    int id;
};

float level1(vec3 p) {

    float lead = guiLead;
    float innerRatio = guiInnerRatio;

    float offset = innerRatio * .25 + .25;
    p.z += offset;

    pR(p.xy, PI * .5);
    float scale = 1.;

    scale *= pModHelix(p, lead, innerRatio);
    p.x *= -1.;
    scale *= pModHelix(p, lead, innerRatio);
    p.x *= -1.;

    float d = length(p.yz) - .5;

    d /= scale;

    return d;
}

float level2(vec3 p) {
    float lead = guiLead;
    float innerRatio = guiInnerRatio;

    float offset = innerRatio * .25 + .25;

    float s = mix(.5, 0., innerRatio);
    offset *= 1. + s;

    pR(p.xy, PI * .5);
    float scale = 1.;

    pR(p.xy, PI * .59);
    scale = s;

    p *= scale;

    p.z += offset;

    scale *= pModHelix(p, lead, innerRatio);
    p.x *= -1.;
    scale *= pModHelix(p, lead, innerRatio);
    p.x *= -1.;
    scale *= pModHelix(p, lead, innerRatio);
    p.x *= -1.;

    float d = length(p.yz) - .5;

    d /= scale;

    return d;
}

Model opU(Model m1, Model m2) {
    if (m1.dist < m2.dist) {
        return m1;
    } else {
        return m2;
    }
}

/*

    t = 0 - 1

0    1 spiral (2nd hidden)
1    animate open 2nd sprial
2    zoom into so it looks like 1 spiral
3    blend into initial state     
    1 spiral

*/

float anim(float t, float index) {
    float overlap = .5;
    float steps = 2.;
    float all = mix(steps, 1., overlap);
    float width = 1. / (all - 1.);
    float each = width * (1.- overlap);
    float start = index * each - width * .5;
    float end = start + width;
    return range(start, end, t);
}



#ifndef HALF_PI
#define HALF_PI 1.5707963267948966
#endif

float sineIn(float t) {
  return sin((t - 1.0) * HALF_PI) + 1.0;
}

float unzip(vec3 p, float t, float invert) {
    // return t;
    // t = smoothstep(0., 1., t);
    float size = 2.2;
    float speed = .01;
    size = guiZipSize;
    speed = guiZipSpeed;

    // speed *= 1. + max(0., sign(-p.x * invert)) * .5;

    // t = pow(t, 1.25);
    // t = mix(sineIn(t), t, .5);

    t *= size * speed;

    if (sign(p.y) == sign(p.x) * invert) {
        float radius = mix(.25, .5, guiInnerRatio);
        float scale = mix(.5, 0., guiInnerRatio);
        float factor = radius / scale * PI * 2.;
        t -= (factor - .5);
    }

    return range(size, 0., abs(p.x) + size - t);
}


vec3 colA = vec3(.5,0,.75); // purple
vec3 colB = vec3(0,1,.25); // green


void addPipe(inout float d, inout vec3 color, vec3 p, float scale, float tt) {

    // if (p.y < 0.) {
    //     tt += .076;
    // }

    float t = clamp(0., 1., tt);

    // t = sineIn(t);
    // t = pow(t, 2.);
    // t = pow(smoothstep(0., 1., t), 2.);
    float boundry = 1.;
    float part;
    float separate = (
        rangec(0., boundry * .01, t) * .3 +
        rangec(boundry * .01, boundry, t) * .7
    );
    // separate = pow(separate, .5);
    float round = rangec(.0, 1., t);
    // separate = rangec(0., boundry, t);
    // round = 0.;

    float side= 0.;
    part = fBox2(side, p.yz, vec2(mix(guiLead * 2., .5, separate), .5));
    part = mix(part, length(p.yz) - .5, round);
    part /= scale;

    d = mix(d, part, smoothstep(.0, .01, t));

    vec3 col = mix(colB, colA, step(3., side));
    // col = mix(col, colA, round);

    float r = mix(.2, 1., rangec(.8, 1., t));
    float a = abs(atan(p.y, p.z) / (PI));
    float b = smoothstep(r, r+.01, a);

    // col = mix(colA, col, b);

    col = spectrum(.5 - t * .5 + .25);
    col = p.y > 0. ? colA : colB;

    // float a = abs(atan(p.y, p.z) / (PI));
    // float b = smoothstep(0., .5, a) - smoothstep(.5, 1., a);
    // vec3 col2 = mix(colA, colB, b);
    // col = mix(col, col2, round);

    col = mix(color, col, step(0., tt));
    color = mix(color, col, rangec(1., 0., tt));
}

float sss = 1. + 10. * guiDebug;

Model map(vec3 p) {
    float part, d, tt;
    float lead = guiLead;
    float innerRatio = guiInnerRatio;
    vec2 uv, uv2;

    p /= sss;

    vec3 pp = p;

    float t = mod(time, 1.);

    float t0 = smoothstep(0., 1./3., t);
    float t1 = smoothstep(1./3., 2./3., t);
    float t2 = smoothstep(2./3., 1., t);

    t1 = t0 = t2 = t;

    float s = mix(.5, 0., innerRatio);
    // s *= s;
    // s = 1.;

    float scaleA = 1.;
    float scaleB = s;

    scaleB = 1./pow(1./s, t1);

    pR(p.yz, guiRotateModelX * PI * 2.);
    pR(p.xy, PI * -.5 * t + guiRotateModel * PI * 2.);
    
    p *= scaleB;
    p.z += .5;

    if (guiFlip) {
        p.x *= -1.;
    }

    scaleB *= pModHelixUnwrap(p, lead, innerRatio, t0);
    p.x *= -1.;
    scaleB *= pModHelixUnwrap(p, lead, innerRatio, 0.);
    p.x *= -1.;

    d = length(p.yz) - .5;
    d /= scaleB;

    // return Model(d, vec3(.8), 1);

    scaleB *= pModHelix(p, lead, innerRatio);
    p.x *= -1.;

    vec3 color = vec3(
        smoothstep(0., .1, sin(p.x * 20.)),
        sin(p.x / 1.5),
        sin(p.x / 3.)
    );
    color = colA;

    color = vec3(1);

    // d = length(p.yz) - .5;
    // d /= scaleB;

    addPipe(d, color, p, scaleB, 1.);

    // 1

    scaleB *= pModHelix(p, lead, innerRatio);
    p.x *= -1.;

    float scb = scaleB;

    float offset = guiZipOffset / lead;

    vec3 cc = vec3(0);

    // cc += vec3(0,1,0) * mod(p.x / factor, 1.);

    tt = unzip(p - vec3(offset,0,0), anim(t, 0.), -1.);
    addPipe(d, color, p, scaleB, tt);

    

    // 2

    scaleB *= pModHelix(p, lead, innerRatio);
    p.x *= -1.;
    p.y *= -1.;

    // p.x -= 50.;

    // cc += vec3(0,0,1) * mod(p.x / factor, 1.);

    tt = unzip(p + vec3(offset,0,0), anim(t, 1.), 1.);
    addPipe(d, color, p, scaleB, tt);

    // color = cc;
    

    // uv = mix(uv, uv2, step(.5, unzip(p.x, anim(t, 1.))));
    // // // 3

    // scaleB *= pModHelix(p, lead, innerRatio);
    // p.x *= -1.;

    // part = length(p.yz) - .5;
    // part /= scaleB;
    // d = mix(d, part, unzip(p.x, anim(t, 2.)));

    // // 4

    // scaleB *= pModHelix(p, lead, innerRatio);
    // p.x *= -1.;

    // part = length(p.yz) - .5;
    // part /= scaleB;
    // d = mix(d, part, unzip(p.x, anim(t, 3.)));

    // color = vec3(1.);

    // color = vec3(mod(uv, 1.), 0.);

    color = vec3(.8);
    // color = colA;

    d *= sss;

    return Model(d, color, 1);
}

Model mapo( vec3 p ){

    float lead = guiLead;
    float innerRatio = guiInnerRatio;

    float offset = innerRatio * .25 + .25;
    p.z += offset;

    pR(p.xy, PI * .5);
    float scale = 1.;

    scale *= pModHelixUnwrap(p, lead, innerRatio, mod(time, 1.));
    p.x *= -1.;

    float d = length(p.yz) - .5;

    d /= scale;

    vec3 color = vec3(
        smoothstep(0., .1, sin(p.x * 20.)),
        sin(p.x / 1.5),
        sin(p.x / 3.)
    );

    return Model(d, color, 1);
}

Model mapDebug(vec3 p) {

    float d = abs(dot(p, vec3(0,0,1)) - .5 * 10. + 5.) - .001;
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
float camDist = guiZoom * sss;


void doCamera() {
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
const float INTERSECTION_PRECISION = .0001; // precision of the intersection
const int NUM_OF_TRACE_STEPS = 100;
const float FUDGE_FACTOR = .8; // Default is 1, reduce to fix overshoots

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
    vec3 eps = vec3( 0.0001, 0.0, 0.0 );
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

// LIGHTING

float softshadow( in vec3 ro, in vec3 rd, in float mint, in float tmax )
{
    float res = 1.0;
    float t = mint;
    for( int i=0; i<16; i++ )
    {
        float h = map( ro + rd*t ).dist;
        res = min( res, 8.0*h/t );
        t += clamp( h, 0.02, 0.10 );
        if( h<0.001 || t>tmax ) break;
    }
    return clamp( res, 0.0, 1.0 );

}


float calcAO( in vec3 pos, in vec3 nor )
{
    float occ = 0.0;
    float sca = 1.0;
    for( int i=0; i<5; i++ )
    {
        float hr = 0.01 + 0.12*float(i)/4.0;
        vec3 aopos =  nor * hr + pos;
        float dd = map( aopos ).dist;
        occ += -(dd-hr)*sca;
        sca *= 0.95;
    }
    return clamp( 1.0 - 3.0*occ, 0.0, 1.0 );    
}



vec3 doLighting(vec3 col, vec3 pos, vec3 nor, vec3 ref, vec3 rd) {

    // lighitng        
    float occ = calcAO( pos, nor );
    vec3  lig = normalize( vec3(-0.6, 0.7, 0.5) );
    float amb = clamp( 0.5+0.5*nor.y, 0.0, 1.0 );
    float dif = clamp( dot( nor, lig ), 0.0, 1.0 );
    float bac = clamp( dot( nor, normalize(vec3(-lig.x,0.0,-lig.z))), 0.0, 1.0 )*clamp( 1.0-pos.y,0.0,1.0);
    float dom = smoothstep( -0.1, 0.1, ref.y );
    float fre = pow( clamp(1.0+dot(nor,rd),0.0,1.0), 2.0 );
    float spe = pow(clamp( dot( ref, lig ), 0.0, 1.0 ),16.0);
    
    dif *= softshadow( pos, lig, 0.02, 2.5 );
    //dom *= softshadow( pos, ref, 0.02, 2.5 );

    vec3 lin = vec3(0.0);
    lin += 1.20*dif*vec3(.95,0.80,0.60);
    lin += 1.20*spe*vec3(1.00,0.85,0.55)*dif;
    lin += 0.80*amb*vec3(0.50,0.70,.80)*occ;
    lin += 0.30*dom*vec3(0.50,0.70,1.00)*occ;
    lin += 0.30*bac*vec3(0.25,0.25,0.25)*occ;
    lin += 0.20*fre*vec3(1.00,1.00,1.00)*occ;
    col = col*lin;

    return col;
}

void shadeSurface(inout Hit hit){

    vec3 background = vec3(.1)* vec3(.5,0,1);
    background = vec3(.2,.8,1.) * .9;
    background = vec3(.9);
    if (hit.isBackground) {
        hit.color = background;
        return;
    }

    if (hit.model.id == 0) {
        float dist = map(hit.pos).dist;
        hit.color = distanceMeter(dist * 10., hit.ray.len, hit.ray.direction, 10.);
        return;
    }

    // pR(hit.normal.xz, 2.75);
    hit.normal *= sphericalMatrix(guiNormalX * PI * 2., guiNormalY * PI * 2.);
    if (guiNormals) {
        hit.color = hit.normal * -.5 + .5;
    } else {
        vec3 ref = reflect(hit.ray.direction, hit.normal);
        vec3 albedo = hit.model.albedo;
        // hit.color = vec3(0);
        hit.color += albedo * (dot(vec3(0,1,0), hit.normal) * .5 + .5);
        // hit.color += albedo * (dot(vec3(1,0,0), hit.normal) * .5 + .5) * colB;
        // hit.color = doLighting(
        //     albedo,
        //     hit.pos,
        //     hit.normal,
        //     ref,
        //     hit.ray.direction
        // );
    }
    float fog = length(camPos - hit.pos);
    fog = smoothstep(camDist, camDist * 2.5, fog);
    // fog = 0.;
    hit.color = mix(hit.color, background, fog);
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

    
    color += plot(height, p, anim(x, 0.)) * hlCol(vec3(1,1,1), hl);
    color += plot(height, p, anim(x, 1.)) * hlCol(vec3(1,1,0), hl);
    color += plot(height, p, anim(x, 2.)) * hlCol(vec3(0,1,1), hl);    
    color += plot(height, p, anim(x, 3.)) * hlCol(vec3(1,0,1), hl);    
    // color += plot(height, p, x) * hlCol(vec3(0,1,1), hl);
}


void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
    mousee = iMouse.xy;

    vec2 p = (-iResolution.xy + 2.0*fragCoord.xy)/iResolution.y;
    vec2 m = mousee.xy / iResolution.xy;

    p.x -= guiOffsetX;
    p.y -= guiOffsetY;

    time = iGlobalTime;
    time *= .55;

    // vec3 c = vec3(1.);
    // renderPaths(c, fragCoord);
    // fragColor = vec4(c,1.0);
    // return;


// debug = true;
    if (p.x > (time - .5) * 3.) {
        debug = true;
    }


    doCamera();

    mat3 camMat = calcLookAtMatrix(camPos, camTar, camUp);
    float focalLength = pow(2., guiFocal);
    focalLength = 3.;
    vec3 rd = normalize(camMat * vec3(p, focalLength));
    Hit hit = raymarch(CastRay(camPos, rd));

    vec3 color = render(hit);
    color = linearToScreen(color);
    fragColor = vec4(color,1.0);
}
