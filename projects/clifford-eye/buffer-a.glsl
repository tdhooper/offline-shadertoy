#version 300 es

// framexbuffer drawcount: 4

precision highp float;

uniform vec2 iResolution;
uniform mat4 cameraViewMatrix;
uniform sampler2D previousSample; // buffer-a.glsl filter: linear
uniform int drawIndex;
uniform int iFrame;
uniform float iTime;
uniform vec4 iMouse;

in vec3 eye;
in vec3 dir;
in float fov;
in float aspect;
in mat4 vView;

out vec4 fragColorOut;

void mainImage(out vec4 a, in vec2 b);

void main() {
    mainImage(fragColorOut, gl_FragCoord.xy);
}
#define DOF

// Dave_Hoskins https://www.shadertoy.com/view/4djSRW
vec2 hash22(vec2 p)
{
    p += 1.61803398875; // fix artifacts when reseeding
	vec3 p3 = fract(vec3(p.xyx) * vec3(.1031, .1030, .0973));
    p3 += dot(p3, p3.yzx+33.33);
    return fract((p3.xx+p3.yz)*p3.zy);
}

// Dave_Hoskins https://www.shadertoy.com/view/4djSRW
vec3 hash31(float p)
{
   vec3 p3 = fract(vec3(p) * vec3(.1031, .1030, .0973));
   p3 += dot(p3, p3.yzx+33.33);
   return fract((p3.xxy+p3.yzz)*p3.zyx); 
}

// Dave_Hoskins https://www.shadertoy.com/view/4djSRW
float hash12(vec2 p)
{
	vec3 p3  = fract(vec3(p.xyx) * .1031);
    p3 += dot(p3, p3.yzx + 33.33);
    return fract((p3.x + p3.y) * p3.z);
}

// iq https://www.shadertoy.com/view/tl23Rm
vec2 rndunit2(vec2 seed ) {
    vec2 h = seed * vec2(1,6.28318530718);
    float phi = h.y;
    float r = sqrt(h.x);
	return r*vec2(sin(phi),cos(phi));
}

// blackle https://suricrasia.online/demoscene/functions/
vec3 rndunit(float seed) {
  return normalize(tan(hash31(seed) * 2. - 1.));
}

vec2 rndunit2X(vec2 seed ) {
    vec2 u = rndunit2(seed);
    if (abs(u.x) > abs(u.y))
    {
        u.x = 0.;
    }
    else
    {
        u.y = 0.;
    }
    return u;
}

// Fork of "Clifford Torus Rotation" by tdhooper. https://shadertoy.com/view/wsfGDS
// 2019-10-13 19:56:51

/*

    Clifford Torus Rotation
    -----------------------

    Getting a good distance for this 4D stereographic projection was
    tricky, see the notes in 'Main SDF', or just toggle DEBUG below to
    see what's going on.

    Big thanks to Matthew Arcus (mla) for providing a better torus
    equation and improving the projection distance fix.

    See also:

    * Animation by Jason Hise https://www.youtube.com/watch?v=1_pzjvVixL0
    * Clifford Torus by mla https://www.shadertoy.com/view/3ss3z4
    * https://en.wikipedia.org/wiki/Clifford_torus
    * http://virtualmathmuseum.org/Surface/clifford_torus/clifford_torus.html

*/

//#define DEBUG

// --------------------------------------------------------
// HG_SDF
// https://www.shadertoy.com/view/Xs3GRB
// --------------------------------------------------------

#define PI 3.14159265359

void pR(inout vec2 p, float a) {
    p = cos(a)*p + sin(a)*vec2(p.y, -p.x);
}

void pR2(inout vec2 p, inout vec2 p2, float a) {
    pR(p, a);
    pR(p2, a);
}

vec2 pMod2(inout vec2 p, vec2 size) {
    vec2 c = floor((p + size*0.5)/size);
    p = mod(p + size*0.5,size) - size*0.5;
    return c;
}

float smax(float a, float b, float r) {
    vec2 u = max(vec2(r + a,r + b), vec2(0));
    return min(-r, max (a, b)) + length(u);
}

float smin2(float a, float b, float r) {
    vec2 u = max(vec2(r - a,r - b), vec2(0));
    return max(r, min (a, b)) - length(u);
}

float smax2(float a, float b, float r) {
    vec2 u = max(vec2(r + a,r + b), vec2(0));
    return min(-r, max (a, b)) + length(u);
}

float smin(float a, float b, float k){
    float f = clamp(0.5 + 0.5 * ((a - b) / k), 0., 1.);
    return (1. - f) * a + f  * b - f * (1. - f) * k;
}

float fTorus(vec3 p, float smallRadius, float largeRadius) {
    return length(vec2(length(p.xz) - largeRadius, p.y)) - smallRadius;
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
// Modelling Components
// --------------------------------------------------------

// Inverse stereographic projection of p,
// p4 lies onto the unit 3-sphere centered at 0.
// - mla https://www.shadertoy.com/view/lsGyzm
vec4 inverseStereographic(vec3 p, out float k) {
    k = 2.0/(1.0+dot(p,p));
    return vec4(k*p,k-1.0);
}

float fTorus(vec4 p4, out vec2 uv) {

    // Torus distance
    // We want the inside and outside to look the same, so use the
    // inverted outside for the inside.
    float d1 = length(p4.xy) / length(p4.zw) - 1.;
    float d2 = length(p4.zw) / length(p4.xy) - 1.;
    float d = d1 < 0. ? -d1 : d2;

    // Because of the projection, distances aren't lipschitz continuous,
    // so scale down the distance at the most warped point - the inside
    // edge of the torus such that it is 1:1 with the domain.
    d /= PI;

    // UV coordinates over the surface, from 0 - 1
    uv = (vec2(
        atan(p4.y, p4.x),
        atan(p4.z, p4.w)
    ) / PI) * .5 + .5;

    return d;
}

// Distances get warped by the stereographic projection, this applies
// some hacky adjustments which makes them lipschitz continuous.

// The numbers have been hand picked by comparing our 4D torus SDF to
// a usual 3D torus of the same size, see DEBUG.

// vec3 d
//   SDF to fix, this should be applied after the last step of
//   modelling on the torus.

// vec3 k
//   stereographic scale factor

float fixDistance(float d, float k) {
    float sn = sign(d);
    d = abs(d);
    d = d / k * 1.82;
    d += 1.;
    d = pow(d, .5);
    d -= 1.;
    d *= 5./3.;
    d *= sn;
    return d;
}

float spintime;
float time;
vec2 modelUv;
bool hitDebugTorus = false;

float warpSpace(inout vec3 p) {
    float k;
    vec4 p4 = inverseStereographic(p,k);

    // Rotate in 4D, turning the torus inside-out
    pR(p4.xw, spintime * -PI / 2.);

    vec2 uv;
    float d = fTorus(p4, uv);
    modelUv = uv;

    #ifdef DEBUG
       // d = fixDistance(d, k);
       // return d;
    #endif

    // Magic number that makes z distance the same scale as xy distances
    d /= 2.25;
    
    // Recreate domain to be wrapped around the torus surface
    // xy = surface / face, z = depth / distance
    p = vec3(uv, d);
    
    return k;
}

float vmax(vec3 v) {
    return max(max(v.x, v.y), v.z);
}

float fBox(vec3 p, vec3 b) {
    vec3 d = abs(p) - b;
    return length(max(d, vec3(0))) + vmax(min(d, vec3(0)));
}

//#define FOO

//========================================================
// Camera
//========================================================

//#define FREE_FLY;
//#define FREE_FLY_CAM0;
//#define FREE_FLY_CAM1
#define CAM 2

float camzoom;
vec3 calcCamPos(float camTime)
{
    #if CAM == 0
        float t = sin(camTime * PI * 4. + PI * 1.45) * .5 + .5;
        t = pow(t, 2.);
        return mix(vec3(-0.211, -0.631, 0.0), vec3(-0.473, -1.231, 0.0), t);
    #elif CAM == 1
        float t = sin(camTime * PI * 2. + PI * 1.5) * .5 + .5;
        t = pow(t, 3.);
        camzoom = t;
        //return vec3(-.0,-1.5,0) * t * 2.;
        return vec3(-0.2, -0.5, 0.0) * 8.;
    #elif CAM == 2
        return vec3(-0.211, -0.631, 0.0) * 1.5;
    #elif CAM == 3
        //float t = sin(camTime * PI * 4. + PI * 1.45) * .5 + .5;
        float t = sin(camTime * PI * 2. + PI * 1.5) * .5 + .5;
        t = pow(t, 3.);
        camzoom = t;
        //return vec3(-.0,-1.5,0) * t * 2.;
        return mix(vec3(-0.21, -0.65, 0.0) * .97, vec3(-0.2, -0.67, 0.0) * 2.3, t);
    #endif
}


//========================================================
// Modeling
//========================================================

struct Material {
    vec3 albedo;
    float specular;
    float roughness;
};

struct Model {
    float d;
    vec3 uvw;
    int id;
    float understep;
};

Material shadeModel(float rlen, Model model, inout vec3 nor) {
    if (model.id == 2)
    {
        return Material(vec3(.5,0,0), .02, .3);
    }
    if (model.id == 3)
    {
        return Material(model.uvw * .5, .9, .6);
    }
    return Material(model.uvw * .5, .9, .6);
}


Model map2(vec3 p) {
    
    float camd = 1e12;
    /*
#ifdef FREE_FLY
    int ci = 30;
    for (int i = 0; i < ci; i++)
    {
        vec3 camPos = calcCamPos(float(i) / float(ci - 1));
        camd = min(camd, length(p - camPos) - .01);
    }
    camd = min(camd, length(p - calcCamPos(time)) - .03);
#endif
*/
    //p.xy = mod(p.xy, 1.0);
    
    float db = length(p) - 2.;
    
    float df = abs(length(p - calcCamPos(time)) - .6);


    float k = 1.;

    //pR(p.xy, iTime * PI * -.25);
    //pR(p.yz, iTime * PI * -1.25);

    // The inside-out rotation puts the torus at a different
    // orientation, so rotate to point it at back in the same
    // direction
 	pR(p.zy, spintime * -PI / 2.);
 	//pR(p.zy, spintime * -PI / 4.);
 	//pR(p.xy, spintime * PI / 2.);

    #ifndef FOO
    k = warpSpace(p);
    #endif
    
    
    float bound = fBox(p-vec3(.5), vec3(.5));
        
    vec3 ppp = p;
    
    // Draw some repeated circles

    float n = 4.*4.;
    float repeat = 1. / n;

    repeat = sqrt(2.) / n;
    pR(p.xy, PI/4.);
    
    p.xy += repeat / 2.;


    //p.yx += iTime * -.1;
    
    float aa = spintime * PI * 1.;
    aa += PI * .25;
   // p.xy += vec2(cos(aa), sin(aa)) * .25;
   // p.xy += repeat / 2.;

    vec2 c = floor((p.xy + repeat*0.5)/repeat);
    c = mod(c + 3., 8.);
    //p.x += iTime * (mod(c.y, 2.) - .5) * .1;
    p.xy = mod(p.xy + repeat*0.5,repeat) - repeat*0.5;

    
    //p.z = mod(p.z + repeat/2., repeat) - repeat/2.;
    /*if (p.z > 0.) {
    	pR(p.xy, PI/2.);
    }
    p.z = abs(p.z);*/

        int id = 1;

    
    if (p.z < 0.) {
       p.x = -p.x;
    }

    float d;
    //p.z -= .05;
    float side = sign(p.z);
    p.z = abs(p.z);
    
    //float hs = 2.7/repeat;
    //d = smin(d, abs(p.z) - .005, .005);
    //d = abs(p.z)- .005;
    //d = smax(d, length(p.xy) - repeat * .3, .003);

    d = fBox(p, vec3(vec2(.3*repeat), 100.)) - .15 * repeat;
    d = mix(d, length(p.xy) - .45 * repeat, .5);
    d = smax(d, abs(p.z) - .005, .005);
    vec3 pp = p;
    p -= vec3(0,0,repeat * -.02);
    float d2 = length(p) - repeat * .3;
    if (side < 0.)
    {
        d2 = smax(d2, min(
            dot(p, normalize(vec3(1,1,.8))),
            dot(p, -normalize(vec3(1,1,-.8)))
        ), .001);
        d = smin(d, d2, repeat * .1);
        d2 = length(p) - repeat * .26;
        pR(p.xy, PI/4.);
        pR(p.xz, -.2);
        d2 = smax(d2, -smax(-d2-1.002, length(p.xy) - repeat * .06, .001), .001);
        d2 = smax(d2, -abs(length(p.xy) - repeat * .13), .001);
        if (d2 < d)
        {
            d = d2;
            id = 3;
        }
        //d = min(d, d2);

    }
    else
    {
        d2 = smax(d2, -abs(dot(p, normalize(vec3(1,1,.8)))), .005);
        d = smin(d, d2, repeat * .1);
    }
    p = pp;
    //d = smax(d, -(length(p.xy) - repeat * .05), .003);
    
    //d = min(d, max(p.z - .11, length(p.xy)) - .001);
p = ppp;

    if (c.x < 4.) {
       // d = 1e12;
        
    }


    //d = min(d, -(p.z - .13));
    //d = min(d, (p.z + .13));

    float rr = .04;
  //  p.xy -= iTime * -.25 + .5;
    //p.y += .5;
//    p.xy = mod(p.xy - .125, .25) + .125;
    p.xy = mod(p.xy, .25) + .125;
    
    //p.xy += .25;
    //p.xy = mod(p.xy, .5);
    //p.xy = mod(p.xy, 1.);
    
    // orb
    //d = min(d, smax(-p.z + .15 - rr, abs(p.y - .25) - rr, rr));
    
    
    //d = min(d, smax(p.z + .15 - rr, abs(p.x - .25) - rr, rr));

    //d = min(d, max((abs(p.x) - .5), (abs(p.y) - .5)));
        //d = min(d, (abs(p.y - .5)));
        //d = min(d, (abs(p.x - .5)));

///    d = min(d, max(-p.y + .01, -(p.z - .11)));
   // d = min(d, length(p - vec3(.6,-.04,.0)) - .1);

    //d = length(p.xy) - repeat * .3;
    //d = smax(d, abs(p.z) - .01, .005);
    //d = length(p) - repeat * .4;
    
   // pR(p.xy, PI/-4.);
    
    
    if (mod(c.y, 2.) == 0.) {
       // p.x = abs(p.x);
	   // p.x -= repeat/2.;
    }
    
    
    if (mod(c.y, 2.) == 0.) {
	    p.z *= -1.;
    }

    
    //float scale = 1.;
    //p /= scale;
    
    //p.z = abs(p.z);
    //d = smin(d, mHead(p * 15.)/15., .01);
        
    //d = mHead(p * 20.)/20.;
   // d = max(d, -p.z);
   // d = smin(d, max(fBox(p, vec3(.025)) - .005, abs(p.z)-.002), .005);
    
    //d = min(d, length(p.yz) - .001);
    
    //p.x = abs(p.x);
    //p.x -= repeat/2.;
    //d = min(d, length(p) - .005);
    
    //d = length(p) - .01;
    /*
    d = mHead(p * 20.)/20.;
    pR(p.xy, PI/2.);
    p.z *= -1.;
    d = smin(d, mHead(p * 20.)/20., .004);
    d = max(d, length(p) - .04);
*/
    
    //d = length(p) - .03;
    
    #ifdef FOO
    d = max(d, bound);
    #endif

    //d = length(p) - .01;

    //d *= scale;
    
    #ifndef FOO
    d = fixDistance(d, k) * 2.;
	#endif
    

    if (camd < d) {
        d = camd;
        id = 2;
    }

   // d = min(d, db);
    
    vec3 uvw = vec3(1,.9,.8);

    //uvw *= mix(1., .1, step(ppp.z, 0.));

    //uvw.x = k;

   // uvw = spectrum(c.x / 8.);
    //uvw = (c.x != 2. && c.y != 2.) ? vec3(1) : vec3(1,0,0);
    //uvw = ((c.x - c.y) < 0.) ? vec3(1) : vec3(1,0,0);
   // uvw = (c.x < 4.) ? vec3(1) : vec3(1,0,0);

    //d = min(d, max(d - .1, df));

    float understep = .25;
    return Model(d, uvw, id, understep);
}

Model map3(vec3 p) {

    float d = abs(fBox(p, vec3(5.)));
    //d = 1e12;
    vec3 uvw = fract(p);
    uvw = vec3(.5);

    float r = 2.;
    p = mod(p + r / 2., vec3(r)) - r / 2.;
    d = min(d, length(p) - r * .125);
    
    int id = 1;
    float understep = 1.;
    return Model(d, uvw, id, understep);
}

Model map(vec3 p) {
    return map2(p);
    return map3(p);
    float d = 1e12;
    int s = 10;
    float spintime_g = spintime;
    for (int i = 0; i < s; i++) {
        d = min(d, map2(p).d);
        spintime += 2. / float(s);
    }
    spintime = spintime_g;
    vec3 uvw = p;
    int id = 1;
    float understep = 1.;
    return Model(d, uvw, id, understep);
}

// --------------------------------------------------------
// Rendering - clifford
// --------------------------------------------------------


mat3 calcLookAtMatrix(vec3 ro, vec3 ta, vec3 up) {
    vec3 ww = normalize(ta - ro);
    vec3 uu = normalize(cross(ww,up));
    vec3 vv = normalize(cross(uu,ww));
    return mat3(uu, vv, ww);
}

#define TAU 6.2831853


//========================================================
// Rendering
//========================================================

// https://iquilezles.org/articles/normalsSDF
vec3 calcNormal( in vec3 pos )
{
    vec3 n = vec3(0.0);
    for( int i=0; i<4; i++ )
    {
        vec3 e = 0.05773*(2.0*vec3((((i+3)>>1)&1),((i>>1)&1),(i&1))-1.0);
        n += e*map(pos+0.001*e).d;
    }
    return normalize(n);
}

float boundRadius = 100.;

vec3 sunPos = normalize(vec3(1,-1,.0)) * 100.;
vec3 skyColor = vec3(0.50,0.70,1.00);
vec3 sunColor = vec3(8.10,6.00,4.20) * 3. * .1;

vec3 env(vec3 dir, bool includeSun) {
   vec3 col = mix(vec3(.5,.7,1) * .0, vec3(.5,.7,1) * 1., smoothstep(-.2, .2, dir.y));
   col *= vec3(1.1,.9,.6);
   return col * .5;
}

struct Hit {
    Model model;
    vec3 pos;
};

Hit march(vec3 origin, vec3 rayDirection, float maxDist, float understep, float boundRadius) {

    vec3 rayPosition;
    float rayLength, dist = 0.;
    Model model;

    for (int i = 0; i < 500; i++) {
        rayPosition = origin + rayDirection * rayLength;
        model = map(rayPosition);
        rayLength += model.d * understep * model.understep;

        if (model.d < .0002) break;

        if (rayLength > maxDist || length(rayPosition) > (boundRadius + .001)) {
            model.id = 0;
            break;
        }
    }
    return Hit(model, rayPosition);
}


// tracing/lighting setup from yx
// https://www.shadertoy.com/view/ts2cWm
vec3 ortho(vec3 a){
    vec3 b=cross(vec3(-1,-1,.5),a);
    // assume b is nonzero
    return (b);
}

// re-borrowed from yx from
// http://blog.hvidtfeldts.net/index.php/2015/01/path-tracing-3d-fractals/
vec3 getSampleBiased(vec3 dir, float power, vec2 seed) {
	dir = normalize(dir);
	vec3 o1 = normalize(ortho(dir));
	vec3 o2 = normalize(cross(dir, o1));
	vec2 r = seed;
	r.x=r.x*2.*PI;
	r.y=pow(r.y,1.0/(power+1.0));
	float oneminus = sqrt(1.0-r.y*r.y);
	return cos(r.x)*oneminus*o1+sin(r.x)*oneminus*o2+r.y*dir;
}

vec3 getConeSample(vec3 dir, float extent, vec2 seed) {
	dir = normalize(dir);
	vec3 o1 = normalize(ortho(dir));
	vec3 o2 = normalize(cross(dir, o1));
	vec2 r =  seed;
	r.x=r.x*2.*PI;
	r.y=1.0-r.y*extent;
	float oneminus = sqrt(1.0-r.y*r.y);
	return cos(r.x)*oneminus*o1+sin(r.x)*oneminus*o2+r.y*dir;
}

vec3 sampleDirect(Hit hit, vec3 nor, vec3 throughput, inout vec2 seed) {
    vec3 col = vec3(0);
    vec3 lightDir = (sunPos - hit.pos);
    vec3 lightSampleDir = getConeSample(lightDir, .0005, seed);
    seed = hash22(seed);
    float diffuse = dot(nor, lightSampleDir);
    vec3 shadowOrigin = hit.pos + nor * (.0002 / abs(dot(lightSampleDir, nor)));
    if (diffuse > 0.) {
        Hit sh = march(shadowOrigin, lightSampleDir, 1., 1., boundRadius);
        if (sh.model.id == 0) {
            col += throughput * sunColor * diffuse;
        }
    }
    return col;
}

float G1V(float dnv, float k){
    return 1.0/(dnv*(1.0-k)+k);
}

// noby https://www.shadertoy.com/view/lllBDM
float ggx(vec3 nor, vec3 rayDir, vec3 l, float rough, float f0){
    float alpha = rough*rough;
    vec3 h = normalize(-rayDir + l);
    float dnl = clamp(dot(nor,l), 0.0, 1.0);
    float dnv = clamp(dot(nor,rayDir), 0.0, 1.0);
    float dnh = clamp(dot(nor,h), 0.0, 1.0);
    float dlh = clamp(dot(l,h), 0.0, 1.0);
    float f, d, vis;
    float asqr = alpha*alpha;
    const float pi = 3.14159;
    float den = dnh*dnh*(asqr-1.0)+1.0;
    d = asqr/(pi * den * den);
    dlh = pow(1.0-dlh, 5.0);
    f = f0 + (1.0-f0)*dlh;
    float k = alpha/1.0;
    vis = G1V(dnl, k)*G1V(dnv, k);
    float spec = dnl * d * f * vis;
    return spec;
}

vec3 sphereLight(vec3 lightPos, float radius, vec3 pos, vec3 rayDir, vec3 nor) {
    vec3 L = (lightPos - pos);
    vec3 ref = reflect(rayDir, nor);
    vec3 centerToRay = dot(L, ref) * ref - L;
    vec3 closestPoint = L + centerToRay * clamp(radius / length(centerToRay), 0., 1.);
    return closestPoint;
}

vec3 sampleDirectSpec(Hit hit, vec3 rayDir, vec3 nor, float rough) {
    vec3 lpos = sphereLight(sunPos, 5., hit.pos, rayDir, nor);
    
    vec3 lightDir = normalize(lpos - hit.pos);
    vec3 h = normalize(rayDir + lightDir);
    float specular = pow(clamp(dot(h, nor), 0., 1.), 64.0);

    vec3 col = vec3(0);

    float fresnel = pow(max(0., 1. + dot(nor, rayDir)), 5.);
    specular = ggx(nor, rayDir, lightDir, rough, fresnel);

    vec3 shadowOrigin = hit.pos + nor * (.0002 / abs(dot(lightDir, nor)));
    if (specular > 0.) {
        Hit sh = march(shadowOrigin, lightDir, 1., 1., boundRadius);
        if (sh.model.id == 0) {
            col += sunColor * specular;
        }
    }
    return col;
}

const float sqrt3 = 1.7320508075688772;


vec3 sphereCam(in vec2 p){
    //float t = 1./(1. + dot(p, p)/3.);
    //float k = iTime / 4.;
    float k = 1./3.;
    float t = 1./(1. + dot(p, p) * k);
    return normalize(vec3(p*t, 2.*t - 1.));
}

vec3 getStereoDir(vec2 p) {
  //float k = iTime;
  float k = 1.33;
  p /= 2.;
  vec3 dir = vec3(
    p * 2. / k,
    dot(p, p) - 1. / k
  );
  return normalize(dir);
}

float sphIntersect( vec3 ro, vec3 rd, vec4 sph ) {
    vec3 oc = ro - sph.xyz;
    float b = dot( oc, rd );
    float c = dot( oc, oc ) - sph.w*sph.w;
    float h = b*b - c;
    if( h<0.0 ) return -1.0;
    h = sqrt( h );
    return -b + h;
}

// main path tracing loop, based on yx's
// https://www.shadertoy.com/view/ts2cWm
// with a bit of demofox's
// https://www.shadertoy.com/view/WsBBR3
vec4 draw(vec2 fragCoord, int frame) {

    vec2 p = (-iResolution.xy + 2.* fragCoord) / iResolution.y;
   
    vec2 seed = hash22(fragCoord + (float(frame)) * sqrt3);
    
    // jitter for antialiasing
    p += 2. * (seed - .5) / iResolution.xy;

    vec3 col = vec3(0);






    time = fract(iTime / 24.);
    //time = .5 / 8.;
    //time = 1.5 / 8.;

    //time = 0.;

    //time = mod(iTime / 3., 2.);
    spintime = time * 8.;
    #ifdef DEBUG
        time = iTime / 6.;
    #endif

    vec3 camPos = calcCamPos(time);
    vec3 camTar = vec3(.1,0,.1);
    vec3 camUp = vec3(-1,0,-1.5);

    float FOV = TAU/6.; // FOV - Field of view.

    //camPos = mix(vec3(-0.211, -0.631, 0.0), vec3(-0.473, -1.231, 0.0), .5);
    //FOV *= mix(2., .5, sin(time * PI * 4. + PI * 1.45) * .5 + .5);

    mat3 camMat = calcLookAtMatrix(camPos, camTar, camUp);

    float focalLength = 0.886;
    vec2 uv = p;

	//vec3 lookAt = u_camera_pos + u_camera_dir;  // "Look At" position.
    vec3 lookDir = vec3(1., 1.0, 0.0);
    
    //vec3 lookDir = vec3(1.0, 9.0, 0.0);
    //pR(lookDir.xz, u_time * .25);
    vec3 lookAt = camPos + lookDir;
    lookAt = vec3(0.0, 0.0, 0.0);

    // Using the above to produce the unit ray-direction vector.
    vec3 forward = normalize(lookAt - camPos);
    vec3 right = normalize(vec3(forward.z, 0, forward.x )); 
    vec3 up = cross(forward, right);


    // rd - Ray direction.
    //vec3 rd = normalize(uv.x*right + uv.y*up + forward/FOV );
    mat3 cam = mat3(right, up, forward);
    //vec3 rd = cam*normalize(vec3(uv, .5/FOV));
    //vec3 rd = cam*sphereCam(uv*PI*.7/FOV);

    //FOV *= mix(1., 2., camzoom);

    //FOV *= 2.;

    vec2 uv2 = uv * PI * .7/FOV;
    vec3 rd = cam * sphereCam(uv2);
    vec3 camF = cam * vec3(0,0,1);
    vec3 screenX = normalize(cam * sphereCam(uv2 + vec2(.1,0)) - rd);
    vec3 screenY = normalize(cam * sphereCam(uv2 + vec2(0,.1)) - rd);

    #if CAM == 0
        pR2(rd.xz, camF.xz, time * PI * -2.0f);
    #elif CAM == 1
        pR2(rd.yz, camF.yz, PI * -.05f);
        pR2(rd.xy, camF.xy, PI * .28f);
        pR2(rd.xz, camF.xz, PI * 0.166f);
    #elif CAM == 2
       //pR2(rd.xz, camF.xz, time * PI * 2.0f);
    #elif CAM == 3
       pR2(rd.xz, camF.xz, time * PI * 2.0f);
    #endif
    //pR(rd.yz, sin(time * PI * 4.0) * -1.);

    //pR(rd.xz, time * PI * -4.0f);
    //pR(rd.yz, -1.);
    //pR(rd.xz, PI * .25f);
    
    vec3 origin = camPos;
    vec3 rayDir = rd;

// can we set our focal point as the 

    // position on focal sphere
    float focalDistance = .6;
    vec3 focalSpherePosition = origin + focalDistance * rayDir;

    // try intersecting ray with larger sphere that's offset, similar to plane
    float u = 10000.;
    //focalSpherePosition = origin + rayDir * sphIntersect(origin, rayDir, vec4(origin - camF * u, u + focalDistance));

    // position on sensor sphere
    float sensorDistance = .05;
    vec3 fwdDof = rayDir;
    vec3 rayBasisX = normalize(cross(fwdDof, vec3(0,1,0)));
    vec3 rayBasisY = normalize(cross(rayBasisX, fwdDof));
    mat3 rayBasis = mat3(rayBasisX, rayBasisY, fwdDof);

    // we want to calculate the screen space frame for the pixel
    // would have thought using ray dir would work
 
    rayBasis = mat3(screenX, screenY, normalize(cross(screenX, screenY)));



    //origin += rayDir * sensorDistance;
    // try jitter wit sphere intead of circle 
    origin += rayBasis * vec3(rndunit2(seed), 0.) * .03;
    //origin += normalize(cross(rayDir, normalize(cross(camF, vec3(0,1,0))))) * rndunit2(seed).x * .6;
    //origin += screenY * rndunit2(seed).x * .6;
    //origin += cross(rayDir, normalize(cross(rayDir, normalize(cross(camF, vec3(0,1,0)))))) * rndunit2(seed).y * .3;
    
    //origin += rndunit(seed.x * 1000. + 1.) * .03;
    //origin -= .5;
    
    rayDir = normalize(focalSpherePosition - origin);
    
    //origin -= rayDir * sensorDistance;


    // ray closest distance from focal point
    // 

    //float bl = 1.-dot(rayDir, camF);
    //rayDir = normalize(rayDir + rndunit(seed.x * 1000. + 1.) * .1 * bl);

/*

    // calc closest point on ray to focal point
    vec3 focalpt = origin + camF * .6;
    //focalpt = vec3(0);
    vec3 closestpt = origin + rayDir * dot(rayDir, focalpt - origin);

    // jitter ray dir constantly
    rayDir = normalize(rayDir + rndunit(seed.x * 1000. + 1.) * .1);
    //rayDir = normalize(rayDir + rndunit(seed.x * 1000. + 1.) * .2 * distance(focalpt, closestpt));

    // offset origin such that ray passes through closest point 
    //origin = closestpt - rayDir * distance(origin, closestpt);
    vec3 originOffset = closestpt - rayDir * distance(origin, closestpt) - origin;
    //origin += originOffset * (1./(distance(focalpt, closestpt) + 1.));
    //origin += rndunit(seed.y * 1000. + 1.) * .01;
    origin += mix(originOffset, rndunit(seed.y * 1000. + 1.) * .01, distance(focalpt, closestpt) * 3.);
    // adjust this offset according to closest point distance
*/

    // 

    //rayDir = getStereoDir(uv * PI * .7/FOV);
    //pR(rayDir.xz, PI * .5);
    //pR(rayDir.xy, PI * -1.398);

    //if (p.x > 0.) {
    //    rayDir = rd;
    //}

#ifdef FREE_FLY
    origin = eye;
    rayDir = normalize(dir);
#endif

#ifdef FREE_FLY_CAM0
    origin = vec3(    -0.08242086321115494,
      0.14857783913612366,
      -1.7797837257385254);
    mat4 vview = mat4(
        -0.4924965798854828,
        0.868827760219574,
        -0.04798750579357147,
        0,
        0.862363874912262,
        0.4948953688144684,
        0.10635580122470856,
        0,
        0.11616011708974838,
        0.011008289642632008,
        -0.9929795861244202,
        0,
        0.03801973536610603,
        0.01767142489552498,
        -1.787046194076538,
        1
    );
    rayDir = normalize(vec3(uv * .72,-1.0) * mat3(vview));
#endif

#ifdef FREE_FLY_CAM1

    mat4 vview = mat4(
    -0.6581622362136841,
     0.1991388499736786,
     -0.7257429957389832,
     0,
     0.544807493686676,
     0.7914337515830994,
     -0.2770546078681946,
     0,
     0.5191762447357178,
     -0.5778564810752869,
     -0.6293509602546692,
     0,
     0.2808123826980591,
     -0.1893066018819809,
     -2.976214647293091,
     1
    );
    origin = vec3(-1.9383219480514526,
    -0.8278401494026184,
    -2.1293909549713135);

    rayDir = normalize(vec3(uv * .72,-1.0) * mat3(vview));
#endif
/*
    // TODO: Figure out accurate DOF for sphere camera
    #ifdef DOF

    // position on sensor plane
    vec3 cameraForward = -transpose(vView)[2].xyz;
    
    Hit dofHit = march(origin, cameraForward, 100., 1., 100.);
    float focalDistance = length(origin - dofHit.pos) - fov;

    origin = origin + rayDir / dot(rayDir, cameraForward) * fov;
    
    // position on focal plane
    //float focalDistance = length(origin);
    vec3 focalPlanePosition = origin + focalDistance * rayDir / dot(rayDir, cameraForward);
    origin = origin + vec3(rndunit2(seed), 0.) * mat3(vView) * .01;
    rayDir = normalize(focalPlanePosition - origin);

    #endif
*/





    Hit hit;
    vec3 nor, ref;
    Material material;
    vec3 throughput = vec3(1);
    vec3 bgCol = vec3(.2);
    bool doSpecular = true;

    //bgCol = sunColor * .3;
    bgCol = mix(sunColor * .2, vec3(.2), .5);


    int MAX_BOUNCE = 4;
    
    float firstHitDist;

    for (int bounce = 0; bounce < MAX_BOUNCE; bounce++) {

        hit = march(origin, rayDir, 100., 1., boundRadius);

        if (bounce == 0) {
        firstHitDist = distance(origin, hit.pos);
        }

        if (hit.model.id == 0)
        {
            if (bounce > 0) {
                col += env(rayDir, doSpecular) * throughput;
            } else {
                col = bgCol;
                //col = mix((sunColor * sunColor) * .2, mix(vec3(.2), env(vec3(0,.0,0), false), .5), .95) * .7;
                //col = sunColor * .3;
            } 
            break;
        }

        nor = calcNormal(hit.pos);

        {
            //col = vec3(dot(normalize(vec3(1,.5,-.5)), nor) * .5 + .5);
            //col = mix(col, vec3(1), .1);
            //col *= hit.model.uvw;
            //break;
        }

        material = shadeModel(distance(camPos, hit.pos), hit.model, nor);

        // calculate whether we are going to do a diffuse or specular reflection ray 
        seed = hash22(seed);
        doSpecular = hash12(seed) < material.specular;
        
        if (bounce == 0) { // fix fireflies from diffuse-bounce specular
            col += sampleDirectSpec(hit, rayDir, nor, material.roughness) * throughput * material.specular;
        }
        
        if ( ! doSpecular) {
            throughput *= material.albedo;
        }

        // Calculate diffuse ray direction
        seed = hash22(seed);
        vec3 diffuseRayDir = getSampleBiased(nor, 1., seed);

        if ( ! doSpecular)
        {
            seed = hash22(seed);
            col += sampleDirect(hit, nor, throughput, seed);
            rayDir = diffuseRayDir;
        }
        else
        {
            // Calculate specular ray direction
            vec3 specularRayDir = reflect(rayDir, nor);
            rayDir = normalize(mix(specularRayDir, diffuseRayDir, material.roughness * material.roughness));
        }

        // offset from sufrace https://www.shadertoy.com/view/lsXGzH
        origin = hit.pos + nor * (.0002 / abs(dot(rayDir, nor)));
    }

    col = mix(col, bgCol, 1.0 - exp(-(firstHitDist) * .15) );

    //col = vec3(firstHitDist * .1);
   // col = mix(col, vec3(0), 1.-clamp(distance(hit.pos, focalSpherePosition), 0., 1.));
    //col = mix(col, vec3(0), 1.-clamp(distance(hit.pos, closestpt), 0., 1.));
    //col = mix(col, vec3(0), (1./(distance(focalpt, closestpt) + 1.)));

    return vec4(col, 1);
}

void mainImage(out vec4 fragColor, in vec2 fragCoord) {
    vec4 col = draw(fragCoord, iFrame);
   
    if (drawIndex > 0) {
        vec4 lastCol = texture(previousSample, fragCoord.xy / iResolution.xy);
        col = mix(lastCol, col, 1. / float(drawIndex + 1));
    }
    
    fragColor = vec4(col.rgb, 1);
}
