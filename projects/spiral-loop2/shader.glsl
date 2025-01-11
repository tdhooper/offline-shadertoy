precision highp float;

uniform vec2 iResolution;
uniform vec2 iOffset;
uniform float iGlobalTime;
uniform vec4 iMouse;
uniform sampler2D iChannel0;

uniform mat4 cameraViewMatrix;
uniform vec3 cameraPosition;

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

float sineIn(float t) {
  return sin((t - 1.0) * HALF_PI) + 1.0;
}

float circularOut(float t) {
  return sqrt((2.0 - t) * t);
}

float circularIn(float t) {
  return 1.0 - sqrt(1.0 - t * t);
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

// Torus in the XZ-plane
float fTorus(vec3 p, float smallRadius, float largeRadius) {
    return length(vec2(length(p.xz) - largeRadius, p.y)) - smallRadius;
}


// Repeat space along one axis. Use like this to repeat along the x axis:
// <float cell = pMod1(p.x,5);> - using the return value is optional.
float pMod1(inout float p, float size) {
    float halfsize = size*0.5;
    float c = floor((p + halfsize)/size);
    p = mod(p + halfsize, size) - halfsize;
    return c;
}

// Repeat in two dimensions
vec2 pMod2(inout vec2 p, vec2 size) {
    vec2 c = floor((p + size*0.5)/size);
    p = mod(p + size*0.5,size) - size*0.5;
    return c;
}

vec3 cartToPolar(vec3 p) {
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

    p = cartToPolar(p);
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



float anim(float t, float index, float steps) {
    float overlap = .5;
    float all = mix(steps, 1., overlap);
    float width = 1. / (all - 1.);
    float each = width * (1.- overlap);
    float start = index * each - width * .5;
    float end = start + width;
    return range(start, end, t);
}

float unzip(vec3 p, float t, bool offset) {
    // return t * 2.;
    // t = smoothstep(0., 1., t);
    float size = 2.2;
    float speed = .01;
    size = guiZipSize;
    speed = guiZipSpeed;

    // t = pow(t, 1.25);
    // t = mix(sineIn(t), t, .5);

    t *= speed * 30.;

    if (sign(p.y) != sign(p.x) && offset) {
        float radius = mix(.25, .5, guiInnerRatio);
        float scale = mix(.5, 0., guiInnerRatio);
        float factor = radius / scale * PI * 2.;
        t -= (factor - .5);
    }

    return range(size, 0., abs(p.x) + size - t);
}


vec3 colA = vec3(.5,0,.75); // purple
vec3 colB = vec3(0,1,.25); // green


float hexagon(vec2 p) {
    vec2 q = vec2( p.x*2.0*0.5773503, p.y + p.x*0.5773503 );
    
    vec2 pi = floor(q);
    vec2 pf = fract(q);

    float v = mod(pi.x + pi.y, 3.0);

    float ca = step(1.0,v);
    float cb = step(2.0,v);
    vec2  ma = step(pf.xy,pf.yx);
    
    // distance to borders
    float e = dot( ma, 1.0-pf.yx + ca*(pf.x+pf.y-1.0) + cb*(pf.yx-2.0*pf.xy) );

    return e;
}

vec3 patternB(vec2 p) {
    p *= 5.9 * 1.5;
    // pMod2(p, vec2(1.));
    // float d = length(p) - .33;
    float d = hexagon(p);
    float fill = smoothstep(.05, .15, d);
    return vec3(fill);
}

float pattern(vec2 p, float t) {
    // return mix(colA, colB, step(.5, t));
    p = abs(p);
    // t = 0.;
    t = rangec(-.5, .5, t);
    // t = circularIn(t);
    float width = mix(.365, 1., 0.);
    float d = dot(p, vec2(0,1)) - width;
    float fill = smoothstep(.0, .01, d);
    return fill * (1.-t);
}

void addPipe(inout float d, vec3 p, float scale, float tt) {

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
}

void addColor(inout vec3 color, vec3 p, float tt, float tnext) {
    vec2 uv = vec2(p.x * .8, atan(p.y, p.z) / PI);
    vec3 col = vec3(mod(uv, 1.), 0.);
    float fill = pattern(uv, tnext);

    col = mix(color, colB, fill);
    color = mix(color, col, step(.0, tt));
}


float sss;


const int HELIX_ITERATIONS = 3;

Model map(vec3 p) {

    // time *= 1.01;

    pR(p.xy, PI * time * .5);
    //pR(p.yz, PI * time * 1.);

    // float dd = length(p) - .1;
    // dd = min(dd, length(p - vec3(.0,-.1,-.2)) - .2);
    // dd = min(dd, length(p - vec3(.1,.2,-.1)) - .02);

    // return Model(dd, vec3(0), 1);
    
    // float ww = length(p - cameraPosition) - .05;
    // ww = 1e23;
    // p = mod(p, .4) - .2;
    // float dd = length(p) - .1;
    // dd = min(dd, ww);
    // return Model(
    //     dd,
    //     vec3(0),
    //     1
    // );

    float part, d, t1, t2, t3, t4;
    float lead = guiLead;
    float innerRatio = guiInnerRatio;
    vec2 uv1, uv2, uv3;

    p /= sss;

    // d = length(p.yz) - .001;
    // d = min(d, length(p.zx) - .002);
    // d = min(d, length(p.xy) - .003);

    // d = min(d, length(p - vec3(.01)) - .003);

    // p += vec3(-.02,0,.03);
    // p *= sphericalMatrix(2.9 * PI * 2., 1.77 * 2.);
    // p.x -= .02;
    // pMod1(p.x, .03);
    // pR(p.xz, -.03);
    // d = min(d, fTorus(p.zxy, .005, .015));

    // d *= sss;
    // return Model(d, vec3(0,1,0), 1);


    vec3 pp = p;

    d = 1e12;

    float t = mod(time * .5, 1.);

    float s = mix(.5, 0., innerRatio);
    s *= s;
    // s = 1.;

    float scaleB = 1./pow(1./s, t);

    //pR(p.yz, guiRotateModelX * PI * 2.);
    //pR(p.xy, PI * -.5 * t + guiRotateModel * PI * 2.);
    
    p *= scaleB;
    p.z += .5;

    if (guiFlip) {
        p.x *= -1.;
    }

    scaleB *= pModHelixUnwrap(p, lead, innerRatio, mod(t*2., 1.));
    p.x *= -1.;
    scaleB *= pModHelix(p, lead, innerRatio);
    p.x *= -1.;
    scaleB *= pModHelix(p, lead, innerRatio);
    p.x *= -1.;

    d = min(d, length(p.yz) - .5);
    d /= scaleB;

    // return Model(d, vec3(.8), 1);

    float offset = guiZipOffset / lead;
    vec3 color = colA;

    float step = -1.;
    float reverse = 1.;
    float invert = 1.;

    for (int i = 0; i <= HELIX_ITERATIONS; i++) {
        scaleB *= pModHelix(p, lead, innerRatio);
        p.x *= -1.;
        t1 = unzip(p + vec3(offset,0,0) * invert, anim(t, step, float(HELIX_ITERATIONS)), true);
        // t2 = unzip((p * 13.7 + vec3(offset,0,0)), anim(t, 0.), false);
        addPipe(d, p, scaleB, t1);
        addColor(color, p, t1, t1);
        step += 1.;
        invert *= -1.;
    }

    // d -= color.r * .0005;

    // color = vec3(.8);

    d *= sss;

    return Model(d, color, 1);
}


Model mapDebug(vec3 p) {

    vec3 n = normalize(vec3(1,0,1));
    float d = abs(dot(p, n) - .5 * 10. + 5.) - .001;
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

const float MAX_TRACE_DISTANCE = 2.; // max trace distance
const float INTERSECTION_PRECISION = .0001; // precision of the intersection
const int NUM_OF_TRACE_STEPS = 1000;
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
    bool isBorder;
};

vec3 calcNormalA(vec3 pos){
    vec3 eps = vec3(.0001,0,0);
    vec3 nor = vec3(
        map(pos+eps.xyy).dist - map(pos-eps.xyy).dist,
        map(pos+eps.yxy).dist - map(pos-eps.yxy).dist,
        map(pos+eps.yyx).dist - map(pos-eps.yyx).dist );
    return normalize(nor);
}


const int NORMAL_STEPS_B = 3;

vec3 calcNormalB(vec3 pos){
    vec3 eps = vec3(.0001,0,0);
    vec3 nor = vec3(0);
    for(int i = 0; i < NORMAL_STEPS_B; i++){
        nor += (map(pos+eps).dist - map(pos-eps).dist) * eps;
        eps = eps.zxy;
    }
    return normalize(nor);
}


const int NORMAL_STEPS = 6;

vec3 calcNormal(vec3 pos){
    vec3 eps = vec3(mix(.000001, .00005, time),0,0);
    vec3 nor = vec3(0);
    float invert = 1.;
    for (int i = 0; i < NORMAL_STEPS; i++){
        nor += map(pos + eps * invert).dist * eps * invert;
        eps = eps.zxy;
        invert *= -1.;
    }
    return normalize(nor);
}


Hit raymarch(CastRay castRay){

    float currentDist = INTERSECTION_PRECISION * 2.0;
    float borderDist = INTERSECTION_PRECISION * 2.0;
    Model model;

    bool border = false;
    bool away = false;

    float lastDist = currentDist;

    Ray ray = Ray(castRay.origin, castRay.direction, 0.);

    for( int i=0; i< NUM_OF_TRACE_STEPS ; i++ ){
        if (currentDist < INTERSECTION_PRECISION || ray.len > MAX_TRACE_DISTANCE) {
            break;
        }
        model = mapDebug(ray.origin + ray.direction * ray.len);
        lastDist = currentDist;
        currentDist = model.dist;
        away = currentDist > lastDist;
        borderDist = currentDist * -1. + mix(.0005, .02, pow(ray.len, 1.)) * .15;
        if (borderDist > .0 && borderDist < currentDist && away) {
            currentDist = borderDist;
            border = true;
        } else {
            border = false;
        }
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

    return Hit(ray, model, pos, isBackground, normal, vec3(0), border);
}

float xray(CastRay castRay){

    float currentDist = INTERSECTION_PRECISION * 2.0;
    Model model;

    Ray ray = Ray(castRay.origin, castRay.direction, 0.);

    float col = 0.;
    float a;
    float stepSize = float(MAX_TRACE_DISTANCE) / float(NUM_OF_TRACE_STEPS);
    vec3 pos;
    float fog;

    for( int i=0; i< NUM_OF_TRACE_STEPS ; i++ ){
        
        pos = ray.origin + ray.direction * ray.len;
        model = mapDebug(pos);
        currentDist = model.dist;
        ray.len += stepSize;

        a = rangec(.005, 0., abs(currentDist));
        a = pow(a, 5.);
        a = (a * 50.) * stepSize;

        fog = length(camPos - pos);
        fog = smoothstep(camDist, camDist * 2.5, fog);

        col += a * (1. - fog);
    }

    
    // fog = 0.;
    // color = mix(color, background, fog);


    return col;
}


// --------------------------------------------------------
// Rendering
// --------------------------------------------------------

// LIGHTING

float softshadow( in vec3 ro, in vec3 rd, in float mint, in float tmax )
{
    float res = 1.0;
    float t = mint;
    float ph = 1e10; // big, such that y = 0 on the first iteration
    
    for( int i=0; i<32; i++ )
    {
        float h = map( ro + rd*t ).dist;

        // traditional technique
        if( false )
        {
            res = min( res, 10.0*h/t );
        }
        // improved technique
        else
        {
            // use this if you are getting artifact on the first iteration, or unroll the
            // first iteration out of the loop
            //float y = (i==0) ? 0.0 : h*h/(2.0*ph); 

            float y = h*h/(2.0*ph);
            float d = sqrt(h*h-y*y);
            res = min( res, 10.0*d/max(0.0,t-y) );
            ph = h;
        }
        
        t += h;
        
        if( res<0.0001 || t>tmax ) break;
        
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



vec3 doLighting2(vec3 col, vec3 pos, vec3 nor, vec3 ref, vec3 rd) {

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


vec3 doLighting(vec3 col, vec3 pos, vec3 nor, vec3 ref, vec3 rd) {

    vec3 up = normalize(vec3(1,.5,0));

    // lighitng        
    float occ = mix(calcAO( pos, nor ), 1., 0.);
    vec3  lig = normalize(vec3(0,.2,1));
    // lig *= sphericalMatrix(guiNormalX * PI * 2., guiNormalY * PI * 2.);
    float amb = clamp(dot(nor, up) * .5 + .5, 0., 1.);
    float dif = clamp( dot( nor, lig ), 0.0, 1.0 );
    float fre = pow( clamp(1.0+dot(nor,rd),0.0,1.0), 2.0 );
    vec3  hal = normalize( lig-rd );
    float spe = pow(clamp( dot( nor, hal ), 0.0, 1.0 ),16.0);
                    
    vec3 cA = vec3(.7,.3,.9);
    vec3 cB = vec3(.4,.9,.8);
    vec3 cC = vec3(.7,0,.7);

    col = mix(cA, cB, rangec(.0, 1., dot(-rd, nor))); // need better ramp
    col = mix(col, vec3(.8,.5,1), rangec(.5, 1., dif) * .5);
    col += cC * rangec(.5, 1., dif) * .1;

    // col = vec3(.5);
    // dif *= softshadow( pos, lig, 0.02, 2.5 ) * .9;

    // vec3 lin = vec3(0);
    // lin += .5 * dif;
    // lin += .1 * spe * dif;
    // lin += .2 * fre * occ;
    // lin += .5 * amb * occ;
    // lin += .4 * occ;
    col = col * mix(.5, 1., amb);

    // col = normalize(pos);
    // col = vec3(amb);
    return col;
}

void render(inout vec3 color, Hit hit){

    vec3 background = vec3(.1)* vec3(.5,0,1);
    // background = vec3(.2,.8,1.) * .9;
    // background = vec3(1);
    // background = vec3(.1);
    background = color;

    if (hit.isBackground) {
        color = background;
        return;
    }

    if (hit.isBorder) {
        color = vec3(background * .5);
    }

    if (hit.model.id == 0) {
        float dist = map(hit.pos).dist;
        color = distanceMeter(dist * 10., hit.ray.len, hit.ray.direction, 10.);
        return;
    }

    // pR(hit.normal.xz, 2.75);
    //hit.normal *= sphericalMatrix(guiNormalX * PI * 2., guiNormalY * PI * 2.);
    if ( ! hit.isBorder) {
        vec3 ref = reflect(hit.ray.direction, hit.normal);
        vec3 albedo = hit.model.albedo;
        // color = vec3(0);
        // color += albedo * (dot(vec3(0,1,0), hit.normal) * .5 + .5);
        // hit.color += albedo * (dot(vec3(1,0,0), hit.normal) * .5 + .5) * colB;
        // pR(hit.normal.xz, 1.);
        // pR(hit.normal.xy, -2.);
        pR(hit.normal.yz, -1.);
        color = doLighting(
            albedo,
            hit.pos,
            hit.normal,
            ref,
            hit.ray.direction
        );
        // color = hit.normal * .5 + .5;
    }

    float fog = length(camPos - hit.pos);
    fog = smoothstep(float(MAX_TRACE_DISTANCE) * .1, float(MAX_TRACE_DISTANCE), fog);
    // color = max(hit.pos, vec3(0)) * .5;
    // fog = 0.;
    fog = pow(fog, .5);
    color = mix(color, background, fog);
    // color = hit.normal * .5 + .5;
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

    
    color += plot(height, p, anim(x, 0., 4.)) * hlCol(vec3(1,1,1), hl);
    color += plot(height, p, anim(x, 1., 4.)) * hlCol(vec3(1,1,0), hl);
    color += plot(height, p, anim(x, 2., 4.)) * hlCol(vec3(0,1,1), hl);    
    color += plot(height, p, anim(x, 3., 4.)) * hlCol(vec3(1,0,1), hl);    
    // color += plot(height, p, x) * hlCol(vec3(0,1,1), hl);
}

// https://www.shadertoy.com/view/4djSRW
#define HASHSCALE3 vec3(.1031, .1030, .0973)
vec2 hash22(vec2 p)
{
    vec3 p3 = fract(vec3(p.xyx) * HASHSCALE3);
    p3 += dot(p3, p3.yzx+19.19);
    return fract((p3.xx+p3.yz)*p3.zy);

}

void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
    sss = 1. + 10. * guiDebug;

    mousee = iMouse.xy;

    vec2 p = (-iResolution.xy + 2.0*fragCoord.xy)/iResolution.y;
    vec2 m = mousee.xy / iResolution.xy;


    vec3 bgA = vec3(.6,.5,.8) * .55;
    vec3 bgB = vec3(.7,.9,1.) * .5;

    vec2 pp = hash22(p * 100.);
    pp = mix(p, pp, .25);
    vec3 color = mix(bgA, bgB, dot(pp, normalize(vec2(.2,-.6))) * .5);

    color *= 1.;
    // pR(color.xy, -.5);

    // color = vec3(0);

    // color = mix(vec3(.4,.3,.5) * .9, vec3(.6), -.2);

    // p.x -= guiOffsetX;
    // p.y -= guiOffsetY;

    time = iGlobalTime;
    // time *= .25;
    time = mod(time, 1.);

    // vec3 c = vec3(1.);
    // renderPaths(c, fragCoord);
    // fragColor = vec4(c,1.0);
    // return;


// debug = true;
    if (p.x > (time - .5) * 3.) {
        debug = true;
    }

    doCamera();

    camPos = cameraPosition;
    // camPos = vec3(0,0,1);
    camDist = length(camPos);

    // mat3 camMat = calcLookAtMatrix(camPos, camTar, camUp);
    mat4 camMat = cameraViewMatrix;
    float focalLength = guiFocal;
    // focalLength = 3.;
    // vec3 rd = normalize(camMat * vec3(p, focalLength));
    vec3 rd = normalize(
        (vec4(p, -focalLength, 1) * camMat).xyz
    );


    // float x = xray(CastRay(camPos, rd));
    // fragColor = vec4(vec3(x),1);
    // return;

    Hit hit = raymarch(CastRay(camPos, rd));

    render(color, hit);

    vec2 uv = fragCoord/iResolution.xy;
    float vig = pow(
        16. * uv.x * uv.y * (1. - uv.x) * (1. - uv.y),
        0.05
    );
    color *= vec3(.9, .95, 1.) * vig * 1.1;
    // color *= vec3(.9, .95, 1.);
    // if (p.x + p.y < 0.) {
    color = mix(color, vec3(pow(length(color * .6), 2.)), .1);
    color *= 1.05;
    
    pR(color.xy, -.5);
    color = pow(color, vec3(1.2,1.3,1.2));

    color = pow(color, vec3(.9));
    color *= 1.5;

    color = linearToScreen(color);
    fragColor = vec4(color,1.0);
}
