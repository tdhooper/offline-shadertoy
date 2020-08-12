// framebuffer tile: 3

precision highp float;

uniform float iTime;
uniform float iGlobalTime;

uniform vec2 iResolution;
uniform vec4 iMouse;

varying vec3 eye;
varying vec3 dir;

uniform vec3 debugPlanePosition;
uniform mat4 debugPlaneMatrix;


uniform mat4 cameraMatrix;
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

uniform float guiSpectrumOffset;
uniform float guiSpectrumScale;

uniform sampler2D iChannel0; // images/blue-noise.png filter: linear wrap: repeat
uniform vec2 iChannel0Size;

void mainImage(out vec4 a, in vec2 b);

void main() {
    mainImage(gl_FragColor, gl_FragCoord.xy);
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

#define LIGHT_MODE



// Michel-Lévy interference in linear sRGB, given path difference F expressed in micrometers.
// Note that for some F this function will return colours with some channels in the negative.
// This is because some interference colours are outside the linear sRGB gamut, and therefore
// must be represented with negative values for some channels.
// Blackle https://www.shadertoy.com/view/wtS3Dy
vec3 ML(float F) {
    return mat3(1.5,-.2,-.1,-.6,1.1,.2,-.1,.1,1.5)*(.5-.5*cos(2.*F*vec3(5.2,5.7,7.2)));
}






// IQ Spectrum

vec3 pal( in float t, in vec3 a, in vec3 b, in vec3 c, in vec3 d ) {
    return a + b*cos( 6.28318*(c*t+d) );
}

vec3 spectrum(float n) {
    vec3 a =  max(ML(n / guiSpectrumScale + guiSpectrumOffset), vec3(0));
    return a;
    vec3 b = pal( n, vec3(0.5,0.5,0.5),vec3(0.5,0.5,0.5),vec3(1.0,1.0,1.0),vec3(0.0,0.33,0.67) );
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

float fBox(vec2 p, vec2 b) {
    vec2 d = abs(p) - b;
    return length(max(d, vec2(0))) + vmax(min(d, vec2(0)));
}

float fBox(vec2 p, vec2 b, float round) {
    return fBox(p, b * (1. - round)) - round * vmax(b);
}

float fBox(inout float side, vec2 p, vec2 b) {
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

float unzip(vec3 p, float t, bool offset) {
    // return t;
    // t = smoothstep(0., 1., t);
    float size = 2.2;
    float speed = .01;
    size = guiZipSize;
    speed = guiZipSpeed;

    // t = pow(t, 1.25);
    // t = mix(sineIn(t), t, .5);

    t = pow(t, 1.5) * 2.;

    t *= size * speed;

    //if (sign(p.y) != sign(p.x) && offset) {
    //    float radius = mix(.25, .5, guiInnerRatio);
    //    float scale = mix(.5, 0., guiInnerRatio);
    //    float factor = radius / scale * PI * 2.;
    //    t -= (factor - .5);
    //}

    float radius = mix(.25, .5, guiInnerRatio);
    float lead = guiLead;

    //(closest.x / lead) * radius * PI * 2.;

    float off = (radius * PI * 2.) / lead * 2.;
    //off = 8.;
    float a = range(size, 0., abs(p.x - off) + size - t);
    float c = range(size, 0., abs(p.x) + size - t);
    //float b = range(size, 0., abs(p.x + off) + size - t);

    return max(a, c);
    //return c;
    //return max(c, max(a, b));
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

    float t = clamp(tt, 0., 1.);

    t -= .1;

    // t = sineIn(t);
    // t = pow(t, 2.);
    // t = pow(smoothstep(0., 1., t), 2.);
    float boundry = 1.;
    float part;
    float separate = (
        rangec(0., boundry * .01, t) * .2 +
        rangec(boundry * .01, boundry, t) * .8
    );
    //separate = .5;
    // separate = pow(separate, .5);
    //float round = rangec(.0, 1., t);
    float round = rangec(.5, 1., t + .1);
    // separate = rangec(0., boundry, t);
    //round = 0.;

    float side= 0.;
    part = fBox(side, p.yz, vec2(mix(guiLead * 2., .5, separate), .5));
    part = mix(part, length(p.yz) - .5, round);
    part /= scale;

    d = mix(d, part, smoothstep(.0, .01, t));
    //d = mix(d, part, step(0., tt));
}

void addColor(inout vec3 color, vec3 p, float tt, float tnext) {
    vec2 uv = vec2(p.x * .8, atan(p.y, p.z) / PI);
    vec3 col = vec3(mod(uv, 1.), 0.);
    float fill = pattern(uv, tnext);

    col = mix(color, colB, fill);
    color = mix(color, col, step(.0, tt));
}


float sss;


const int HELIX_ITERATIONS = 2;

Model mapHelix(vec3 p) {

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

    p.y -= 3.;
    p = p.xzy;
    pR(p.yz, PI/5.);

    sss = 60.;

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

    float flipColor = mod(floor(time), 2.);

    float t = mod(time, 1.);

    float s = mix(.5, 0., innerRatio);
    // s *= s;
    // s = 1.;

    float scaleB = 1./pow(1./s, t);

    pR(p.yz, guiRotateModelX * PI * 2.);
    pR(p.xy, PI * -.5 * t + guiRotateModel * PI * 2.);
    
    p *= scaleB;
    p.z += .5;

    if (guiFlip) {
        p.x *= -1.;
    }

    scaleB *= pModHelixUnwrap(p, lead, innerRatio, t);
    p.x *= -1.;
    scaleB *= pModHelixUnwrap(p, lead, innerRatio, 0.);
    p.x *= -1.;

    d = min(d, length(p.yz) - .5);
    d /= scaleB;

    // return Model(d, vec3(.8), 1);

    float offset = guiZipOffset / lead;
    vec3 color = vec3(0);

    float sstep = -1.;
    float reverse = 1.;
    float invert = 1.;


    vec3 pUv;

    for (int i = 0; i <= HELIX_ITERATIONS; i++) {
        pUv = p + vec3(lead / 2., 0, 0);
        pModHelix(pUv, lead, innerRatio);

        scaleB *= pModHelix(p, lead, innerRatio);
        p.x *= -1.;
        //p.z *= -1.;
        t1 = unzip(pUv, anim(t, sstep), true);
        //t1 = unzip(p + vec3(offset,0,0) * invert, anim(t, sstep), false);
        // t2 = unzip((p * 13.7 + vec3(offset,0,0)), anim(t, 0.), false);
        addPipe(d, p, scaleB, t1);
        //addColor(color, p, t1, t1);
        color = mix(color, vec3(mod(sstep + 10. + flipColor, 2.)), clamp(t1 * 10., 0., 1.));
        //if (i == 2) {
            //color = mix(color, vec3(1.), clamp(abs(pUv.x) / 10., 0., 1.));
        //    color = mix(color, vec3(1.), clamp(t1 * 10., 0., 1.));
        //}
        sstep += 1.;
        invert *= -1.;
    }

    // d -= color.r * .0005;

    // color = vec3(.8);

    d *= sss;

    return Model(d, color, 1);
}
















Model map(vec3 p) {
    return mapHelix(p);
}


//========================================================
// Lighting
//========================================================


float intersectPlane(vec3 rOrigin, vec3 rayDir, vec3 origin, vec3 normal, vec3 up, out vec2 uv) {
    float d = dot(normal, (origin - rOrigin)) / dot(rayDir, normal);
  	vec3 point = rOrigin + d * rayDir;
	vec3 tangent = cross(normal, up);
	vec3 bitangent = cross(normal, tangent);
    point -= origin;
    uv = vec2(dot(tangent, point), dot(bitangent, point));
    return max(sign(d), 0.);
}

mat3 envOrientation;

vec3 light(vec3 origin, vec3 rayDir) {
    origin = -(cameraMatrix * vec4(origin, 1)).xyz;
    rayDir = -(cameraMatrix * vec4(rayDir, 0)).xyz;

    origin *= envOrientation;
    rayDir *= envOrientation;

    vec2 uv;
    float hit = intersectPlane(origin, rayDir, vec3(5,-2,-8), normalize(vec3(1,-.5,-.1)), normalize(vec3(0,1,0)), uv);
    float l = smoothstep(.75, .0, fBox(uv, vec2(.4,1.2) * 6.));
	return vec3(l) * hit;
}

vec3 env(vec3 origin, vec3 rayDir) {
    
    origin = -(cameraMatrix * vec4(origin, 1)).xyz;
    rayDir = -(cameraMatrix * vec4(rayDir, 0)).xyz;

    origin *= envOrientation;
    rayDir *= envOrientation;

    float l = smoothstep(.0, 1.7, dot(rayDir, vec3(.5,-.3,1))) * .4;
    //l = smoothstep(-.5, 2.2, dot(rayDir, vec3(.5,-.3,1))) * .4;
    //l = smoothstep(.2, .5, dot(rayDir, vec3(.5,-.3,1))) * .4;
    //l = smoothstep(-1., 2., dot(rayDir, vec3(.5,-.3,1))) * .4;
   	return vec3(l) * vec3(1,1,1);
}



//========================================================
// Marching
//========================================================

// vec3 normal(in vec3 p){
//   vec3 v = vec3(.001, 0, 0);
//   vec3 n = vec3(
//       map(p + v.xyy).x,
//       map(p + v.yxy).x,
//       map(p + v.yyx).x
//   ) - map(p).x;
//   return normalize(n);
// }

// normal function, call de() in a for loop for faster compile times.
vec3 normal(vec3 p) {
    vec4 n = vec4(0);
    for (int i = 0 ; i < 4 ; i++) {
        vec4 s = vec4(p, 0);
        s[i] += 0.001;
        n[i] = map(s.xyz).dist;
    }
    return normalize(n.xyz-n.w);
}

struct Hit {
    Model model;
    vec3 p;
    float len;
    bool isBg;
};

Hit march(vec3 origin, vec3 rayDir, float maxDist) {
    vec3 p;
    float len = 0.;
    float dist = 0.;
    bool isBg = false;
    Model model;

    for (float i = 0.; i < 200.; i++) {
        len += dist * .9;
        p = origin + len * rayDir;
        model = map(p);
        dist = model.dist;
        if (dist < .0001) {
            break;
        }
        if (len >= maxDist) {
            isBg = true;
            len = maxDist;
            break;
        }
    }   

    return Hit(model, p, len, isBg);
}



mat3 sphericalMatrix(vec2 tp) {
    float theta = tp.x;
    float phi = tp.y;
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


// http://filmicworlds.com/blog/filmic-tonemapping-operators/
vec3 tonemap2(vec3 texColor) {
    texColor /= 2.;
   	texColor *= 16.;  // Hardcoded Exposure Adjustment
   	vec3 x = max(vec3(0),texColor-0.004);
   	return (x*(6.2*x+.5))/(x*(6.2*x+1.7)+0.06);
}

const float MAX_DISPERSE = 3.;
const float MAX_BOUNCE = 3.;


void mainImage( out vec4 fragColor, in vec2 fragCoord )
{

    time = iTime / 2.;
    //time= 0.;
    //time = fract(time + .4);
    
    #ifdef LIGHT_MODE
        envOrientation = sphericalMatrix(((vec2(81.5, 119) / vec2(187)) * 2. - 1.) * 2.);
    #else
        envOrientation = sphericalMatrix((vec2(0.7299465240641712,0.3048128342245989) * 2. - 1.) * 2.);
    #endif

    vec2 uv = (2. * fragCoord - iResolution.xy) / iResolution.y;

    Hit hit, firstHit;
    Model model;
    vec3 p, rayDir, origin, ref, raf, nor;
    float ior, offset, extinctionDist, maxDist, firstLen, bounceCount, wavelength;
    
    float focal = 3.8;

    vec3 bgCol = vec3(.22);

    maxDist = 15.; 
    origin = eye;
    rayDir = normalize(dir);

    vec3 col = vec3(0);

    hit = march(origin, rayDir, maxDist);
    model = hit.model;
    p = hit.p;
    
    if ( ! hit.isBg) {

        vec3 pos = p;
        vec3 rd = rayDir;
        vec3 nor = normal(p);
        vec3 lin = vec3(0);
        vec3 ref = reflect( rd, nor );
        float ks = 1.0;
        float occ = 1.;
        col = hit.model.albedo;

        wavelength = dot(rd, nor);
        vec3 speccol = spectrum(wavelength*1.5 + 1.5);
        //speccol = vec3(1);

        // sun
        {
            vec3  lig = normalize( vec3(-0.5, 0.4, -0.6));
            vec3  hal = normalize( lig-rd );
            float dif = clamp( dot( nor, lig ), 0.0, 1.0 );
        	      //dif *= calcSoftshadow( pos, lig, 0.02, 2.5 );
			float spe = pow( clamp( dot( nor, hal ), 0.0, 1.0 ),6.0);
                  spe *= dif;
                  spe *= 0.04+0.96*pow(clamp(1.0-dot(hal,lig),0.0,1.0),5.0);
            lin += col*2.20*dif*vec3(1)*speccol;
            lin +=     5.00*spe*vec3(1)*ks*speccol;
        }
        // sky
        {
            float dif = sqrt(clamp( 0.5+0.5*nor.y, 0.0, 1.0 ));
                  dif *= occ;
            float spe = mix(ref.y * .5 + .5, smoothstep( -0.2, 0.2, ref.y ), .3);
                  spe *= dif;
                  //spe *= calcSoftshadow( pos, ref, 0.02, 2.5 );
                  spe *= 0.04+0.96*pow(clamp(1.0+dot(nor,rd),0.0,1.0), 5.0 );
            lin += col*0.60*dif*vec3(.2);
            lin +=     2.00*spe*vec3(.2)*ks;
        }
        // back
        {
        	//float dif = clamp( dot( nor, normalize(vec3(0.5,0.0,0.6))), 0.0, 1.0 )*clamp( 1.0-pos.y,0.0,1.0);
            //      dif *= occ;
        	//lin += col*0.55*dif*vec3(0.25,0.25,0.25);
        }

        col = lin;
        //col += light(p, nor);
        //col += env(p, nor);
    }

    

    float t = hit.len;
    col = mix( col, vec3(0), 1.0-exp( -0.00001*t*t*t*t*t ) );

    col = pow( col, vec3(0.4545) );
    //col = tonemap2(col);

    fragColor = vec4(col, 1);
}



























