// framebuffer drawcount: 1, tile: 1

precision highp float;

uniform vec2 iResolution;

uniform mat4 cameraMatrix;
uniform vec3 cameraPosition;

uniform sampler2D previousSample; // buffer-a.glsl filter: linear
uniform float drawIndex;
uniform int iFrame;
uniform float iTime;
uniform vec4 iMouse;

varying vec3 eye;
varying vec3 dir;
varying float fov;
varying float aspect;
varying mat4 vView;

#pragma glslify: inverse = require(glsl-inverse)

void mainImage(out vec4 a, in vec2 b);

void main() {
    mainImage(gl_FragColor, gl_FragCoord.xy);
}
#define ANIMATE
#define SSS
#define DOF
//#define PREVIEW

// Dave_Hoskins https://www.shadertoy.com/view/4djSRW
vec2 hash22(vec2 p)
{
    p += 1.61803398875; // fix artifacts when reseeding
	vec3 p3 = fract(vec3(p.xyx) * vec3(.1031, .1030, .0973));
    p3 += dot(p3, p3.yzx+33.33);
    return fract((p3.xx+p3.yz)*p3.zy);
}

// Dave_Hoskins https://www.shadertoy.com/view/4djSRW
float hash12(vec2 p)
{
	vec3 p3  = fract(vec3(p.xyx) * .1031);
    p3 += dot(p3, p3.yzx + 33.33);
    return fract((p3.x + p3.y) * p3.z);
}

vec3 hash31(float p)
{
   vec3 p3 = fract(vec3(p) * vec3(.1031, .1030, .0973));
   p3 += dot(p3, p3.yzx+33.33);
   return fract((p3.xxy+p3.yzz)*p3.zyx); 
}

// iq https://www.shadertoy.com/view/tl23Rm
vec2 rndunit2(vec2 seed ) {
    vec2 h = seed * vec2(1,6.28318530718);
    float phi = h.y;
    float r = sqrt(h.x);
	return r*vec2(sin(phi),cos(phi));
}

#define PI 3.14159265359

void pR(inout vec2 p, float a) {
    p = cos(a)*p + sin(a)*vec2(p.y, -p.x);
}

float smin(float a, float b, float k){
    float f = clamp(0.5 + 0.5 * ((a - b) / k), 0., 1.);
    return (1. - f) * a + f  * b - f * (1. - f) * k;
}

float smax(float a, float b, float k) {
    return -smin(-a, -b, k);
}

float vmax(vec2 v) {
	return max(v.x, v.y);
}

float vmin(vec2 v) {
	return min(v.x, v.y);
}

float vmax(vec3 v) {
	return max(max(v.x, v.y), v.z);
}

float vmin(vec3 v) {
	return min(min(v.x, v.y), v.z);
}

float fBox(vec2 p, vec2 b) {
	vec2 d = abs(p) - b;
	return length(max(d, vec2(0))) + vmax(min(d, vec2(0)));
}

float fBox(vec3 p, vec3 b) {
	vec3 d = abs(p) - b;
	return length(max(d, vec3(0))) + vmax(min(d, vec3(0)));
}



// Icosahedral domain mirroring
// knighty https://www.shadertoy.com/view/MsKGzw

#define PI 3.14159265359

vec3 facePlane;
vec3 uPlane;
vec3 vPlane;

int Type=5;
vec3 nc;
vec3 pab;
vec3 pbc;
vec3 pca;

void init() {
    float cospin=cos(PI/float(Type)), scospin=sqrt(0.75-cospin*cospin);
    nc=vec3(-0.5,-cospin,scospin);
    pbc=vec3(scospin,0.,0.5);
    pca=vec3(0.,scospin,cospin);
    pbc=normalize(pbc); pca=normalize(pca);
	pab=vec3(0,0,1);
    
    facePlane = pca;
    uPlane = cross(vec3(1,0,0), facePlane);
    vPlane = vec3(1,0,0);
}

vec3 fold(vec3 p) {
	for(int i=0;i<5 /*Type*/;i++){
		p.xy = abs(p.xy);
		p -= 2. * min(0., dot(p,nc)) * nc;
	}
    return p;
}

vec3 sfold(vec3 p, float s) {
	for(int i=0;i<5 /*Type*/;i++){
        p.xy = sqrt(p.xy * p.xy + s);
		p -= 2. * min(0., dot(p,nc)) * nc;
	}
    return p;
}


// Triangle tiling
// mattz https://www.shadertoy.com/view/4d2GzV

const float sqrt3 = 1.7320508075688772;
const float i3 = 0.5773502691896258;

const mat2 cart2tri = mat2(1, 0, i3, 2. * i3);
const mat2 tri2cart = mat2(1, 0, -.5, .5 * sqrt3);

vec2 pick3(vec2 a, vec2 b, vec2 c, float u) {
	float v = fract(u * 0.3333333333333);
	return mix(mix(a, b, step(0.3, v)), c, step(0.6, v));
}

vec4 pick3(vec4 a, vec4 b, vec4 c, float u) {
	float v = fract(u * 0.3333333333333);
	return mix(mix(a, b, step(0.3, v)), c, step(0.6, v));
}

vec4 closestHex(vec2 p, float separate) {
    p = cart2tri * p;
	vec2 pi = floor(p);
	vec2 pf = fract(p);
    /*
	vec2 nn = pick3(
        vec2(0, 0),
        vec2(1, 1),
        vec2(1, 0),
        pi.x + pi.y
    );
	vec2 hex = mix(nn.xy, nn.yx, step(pf.x, pf.y)) + pi;
    hex = tri2cart * hex;
    */

	vec4 nn = pick3(vec4(0.0, 0.0, 2.0,  1.0),
					vec4(1.0, 1.0, 0.0, -1.0),
					vec4(1.0, 0.0, 0.0,  1.0),
					pi.x + pi.y);
	
	vec4 ab = ( mix(nn, nn.yxwz, step(pf.x, pf.y)) +
			 vec4(pi, pi) );

    vec2 hexA = mix(ab.xy, ab.zw, separate * .5);
    vec2 hexB = ab.xy;
    hexA = tri2cart * hexA;
    hexB = tri2cart * hexB;

    return vec4(hexA, hexB);
}


// Geodesic tiling
// tdhooper https://www.shadertoy.com/view/llGXWc

vec3 intersection(vec3 n, vec3 planeNormal, float planeOffset) {
    float denominator = dot(planeNormal, n);
    float t = (dot(vec3(0), planeNormal) + planeOffset) / -denominator;
    return n * t;
}

vec2 icosahedronFaceCoordinates(vec3 p) {
    vec3 i = intersection(normalize(p), facePlane, -1.);
    return vec2(dot(i, uPlane), dot(i, vPlane));
}

vec3 faceToSphere(vec2 facePoint) {
	return normalize(facePlane + (uPlane * facePoint.x) + (vPlane * facePoint.y));
}

const float edgeLength = 1. / ((sqrt(3.) / 12.) * (3. + sqrt(5.)));
const float faceRadius = (1./6.) * sqrt(3.) * edgeLength;

vec3 geodesicTri(vec3 p, float subdivisions, float separate) {
	float uvScale = subdivisions / faceRadius;
    vec2 uv = icosahedronFaceCoordinates(p);
    uvScale /= 1.3333;
    vec4 closest = closestHex(uv * uvScale, separate) / uvScale; 
    float ridge = smoothstep(.23 * .5, .0, length(uv - closest.xy) * subdivisions);
    float inside = smoothstep(.2, .19, length(uv - closest.zw) * subdivisions);
    return vec3(ridge, max(0., inside - smoothstep(.25, 1., ridge)), 0.);
}


float time;

struct Material {
    vec3 albedo;
    float specular;
    float roughness;
    bool sss;
};

struct Model {
    float d;
    vec3 uvw;
    vec3 albedo;
    int id;
};

Material shadeModel(Model model, inout vec3 nor) {
    //return Material(model.albedo, .15, .3, true);

    vec3 skin = pow(vec3(0.890,0.769,0.710), vec3(2.2));
    float flush = smoothstep(-1.75, -.0, model.albedo.x);
    //skin += mix(vec3(-.6,.0,.15) * .5, vec3(.4,-.03,-.05), flush);
    //skin *= vec3(1.1,.8,.7);
    //skin = clamp(skin, vec3(0,0,0), vec3(1,1,1));
    bool sss = false;
    #ifdef SSS
    sss = true;
    #endif
    return Material(mix(skin, model.albedo, .8), .0, 1., sss);
}


// Spectrum palette, iq https://www.shadertoy.com/view/ll2GD3
vec3 pal( in float t, in vec3 a, in vec3 b, in vec3 c, in vec3 d ) {
    return .5 + .5 * cos( PI * 2.*mod(t + vec3(0,.33,.67), 1.) );
}
vec3 spectrum(float n) {
    return pal( n, vec3(0.5,0.5,0.5),vec3(0.5,0.5,0.5),vec3(1.0,1.0,1.0),vec3(0.0,0.33,0.67) );
}

// https://iquilezles.org/articles/functions/
float sabs( float x, float k )
{
    return sqrt(x*x+k);
}

// https://iquilezles.org/articles/smin/
vec4 smin( vec4 a, vec4 b, float k )
{
    float h = max( k-abs(a.x-b.x), 0.0 )/k;
    float m = h*h*0.5;
    float s = m*k*0.5;
    vec2 r = (a.x<b.x) ? vec2(a.x,m) : vec2(b.x,1.0-m);
    return vec4(r.x-s, mix( a.yzw, b.yzw, r.y ) );
}

const float PHI = 1.61803398875;

#define sqrt2i 0.7071067811865475
#define sqrt38 0.6123724356957945


Model map(vec3 p) {


    vec3 pp = p;
    p.y += .3;
    
    
    pR(p.xz, .3);
    pR(p.yz, .4);
    pR(p.xy, .3);

    vec3 col = normalize(p) * .5 + .5;
    float d;
    
    
    float s = .03;
    p /= s;
    
    d = 1e12;
    
    const int iter = 15;    
    float scl = 1.;
    vec4 dcol = vec4(1e20,0.0,0.0,0.0);



    float e = p.x * 1.;
    
    for (int i = 0; i < iter; i++) {


float offs = scl * 7.;
        
        

        float t = float(i) / max(1., float(iter - 1));

        //vec2 anim = sin(iTime * vec2(1., 2.) + (t * (vec2(2., 1.) + vec2(0., 1.)) * PI * 2.));
    
    
        vec3 dp = p;
        if (i > 0)
        {
            p.x = mix(p.x, min(p.x + offs, 0.), step(p.x, 0.));
        }

        float d2 = length(p * vec3(1,1,1)) - scl * 5.;
        p = dp;

        vec3 col2 = vec3(0);

        d2 += sin(time * PI * 6. + t * t * 10.) * .015;
        
        
        //float a = (atan(p.y, p.z) / PI) * .5 + .5;
        //float w = (dot(normalize(p), vec3(1,0,0)));
        //float ridge = sin(a * PI * 2. * 5. * e) * .5 + .5;
        //ridge *= (sin(w * 2. * e * PI - PI * .5) * .5 + .5) * smoothstep(1., .9, abs(w));

        #if 1
        //if (d2 < .5)
        if (d2 < 3.5 * scl)
        {
            float subd = ceil(22. * (scl));

            float f = 3. * subd;
            float k = sin(length(sin(pp * 5.)) * -10. + dot(p + sin(p * 6.) * 0., vec3(-.0,.5,0)) * 1. + time * PI * 2.);
            vec3 np = normalize(p);
            vec3 vv = sin(vec3(
                dot(np, vec3(1,0,0)),
                dot(np, vec3(0,1,0)),
                dot(np, vec3(0,0,1))
            ) * f + PI * .5);
            float v = vv.x * vv.y * vv.z;

            //v += k * .5;

            vec3 sp = normalize(sfold(p, .00005));

            //k = (step(0.0, sin(sp.x * 10. + iTime)) * 2. - 1.);

            v = .0;
            k = 0.;

            float separate = .2 + v * .2;
            separate += k * .15;
            separate += .2;
            
            vec3 gt = geodesicTri(sp, subd, separate);

           // t = 1.;

            float ridge = gt.x;
            float inside = gt.y;
            ridge *= sqrt(t);
          
            d2 -= v * .05;
            ridge -= v * .333;

            d2 -= (ridge * 2. - 1.) * .05 * (.8 + v * .2);
            

            float ridgestep = ridge;

            //ridgestep = 0.;

            float ts = float(i);
            if (i > 0)
            {
                ts += (p.x - offs) / offs / 1.25;
            }
            ts /= max(1., float(iter - 1));
            col2 = spectrum((ts * ts) * 1.3);
            
            //col2 = spectrum(ts * .2 + .4);
            col2 = mix(col2, mix(spectrum(1.3 + .175), vec3(1), .5) * 3., ridgestep);
            col2 *= ts * ts;
            col2 *= mix(.5, 1., ridge);
            //col2 = mix(col2, vec3(0), inside);

            //col2 = vec3(v * .5 + .5);
            //col2 = vec3(fract(ridge));

            //col2 = vec3(k);

        }


        #endif
        
        vec4 dcol2 = vec4(d2, col2);
    
        dcol = smin(dcol, dcol2, 4. * scl * .25);


        
        if (i > 0)
        {
        
        float t = float(i - 1) / max(1., float(iter - 1));
        float e = mix(4., 3., t)*.666;

        float at = 4.440;
        vec2 anim = sin(at * .5 - e * .5 + PI * -4. + vec2(0,.25) * PI) * (t * 1.);
        //anim *= 0.;
        anim += sin(at * (1. + PHI) - e * .5 + t * PI * 5. + vec2(0,.25) * PI) * .2 * (t * t * 5.);

        anim += sin(time * PI * 2. + t * PI * 15. + vec2(.5,.25) * PI) * .1 * (1. - t);

        anim *= t * t * 5.;
        
        float fi = float(i - 1);
        pR(p.xy, anim.x * .05 + PI * (.17 + mod(fi, 3.) * .2) * sign(mod(fi, 2.) - .5));
        pR(p.xz, anim.y * .05 + PI * (.17 + mod(fi, 3.) * .2) * sign(mod(fi, 2.) - .5));
        }

        p.x = sabs(p.x,0.005*scl);

        p.x -= offs;

/*
        float fi = float(i);
        pR(p.xy, anim.x * .05 + PI * (.17 + mod(fi, 3.) * .2) * sign(mod(fi, 2.) - .5));
        pR(p.xz, anim.y * .05 + PI * (.17 + mod(fi, 3.) * .2) * sign(mod(fi, 2.) - .5));
  */      
        //pR(p.xy, PI * (1.3 + rnd.x * .1));
        //pR(p.xz, PI * (1.3 + rnd.y * .1));



        scl *= mix(.85, .88, t);
        
       // if (i > 15)
        {
        //    scl *= .95;
        }
        
        //pR(p.xy, -.2);

    }
    
    //d = length(p) * pow(scale, -float(iter));
//d -= .2;

    d = dcol.x;
    
    col = dcol.yzw;
    
    //d += .6;
    //col = spectrum(dcol.y * -.03 + .3 + smoothstep(.14, .16, d - .2) * .05);
    //d -= length(sin(dcol.yzw * 15.)) * .2 + .1;

        d *= s;

    //d -= .1;

    //d = fBox(pp, vec3(.5));
    
    return Model(d, p, col, 1);
}


//========================================================
// Dust
//========================================================

vec3 sunPos = normalize(vec3(-.5,1.5,-.2)) * 100.;
//vec3 sunPos = normalize(vec3(-1.5,1.5,-.7)) * 100.;
vec3 skyColor = vec3(0.50,0.70,1.00);
vec3 sunColor = vec3(8.10,6.00,4.20) * 6.;


vec3 hash33(vec3 p3)
{
	p3 = fract(p3 * vec3(.1031, .1030, .0973));
    p3 += dot(p3, p3.yxz+33.33);
    return fract((p3.xxy + p3.yxx)*p3.zyx);
}
float hash13(vec3 p3)
{
	p3  = fract(p3 * .1031);
    p3 += dot(p3, p3.zyx + 31.32);
    return fract((p3.x + p3.y) * p3.z);
}

vec4 traceDust(vec3 ro, vec3 rd, float depth) {
    float scl = 20.;

    ro *= scl;

    vec3 p = floor(ro + rd * 5.) + .5;

	vec3 dRd = 1./abs(rd); // 1./max(abs(rd), vec3(.0001));
	vec3 srd = sign(rd);
    vec3 side = dRd*(srd*(p - ro) + .5);
    
    vec3 mask = vec3(0);
	
    float d = 0.;    
    
	for (int i = 0; i < 60; i++) {		
        vec3 coord = floor(p);

        vec3 c = coord + .5;

        if (distance(ro, coord) / scl >= depth) {
            break;
        }
        
        vec3 hc = coord;
        vec3 h = hash33(hc);
        float r = mix(.0, .03, pow(hash13(h), 3.));
        float w = .1;
        c += (h * 2. - 1.) * (.5 - r - w);

        vec3 n = vec3(.5,0,.25);
        float a = time * PI * 2. + dot(hc, n) * .5;
        c += vec3(cos(a), sin(-a) * .5, cos(a)) * w * sin((hc.z + PI * .9) * .2);

        mask = step(side, side.yzx)*(1. - step(side.zxy, side));
		side += mask*dRd;
		p += mask*srd;

        if (i > 0)
        {
            float dd = distance(c, ro + rd * dot(rd, c - ro));
            float dc = distance(ro, c) / scl;
            float fogFalloff = exp(-dc * .4);
            d += step(dd, r) * .25 * fogFalloff;
        }
    }
    
    return vec4(skyColor * skyColor, d);// * mix(sunColor * .02, skyColor, .75) * 2.;
}

//========================================================
// Rendering
//========================================================

vec3 calcNormal( in vec3 p ) // for function f(p)
{
    const float eps = 0.0001; // or some other value
    const vec2 h = vec2(eps,0);
    return normalize( vec3(map(p+h.xyy).d - map(p-h.xyy).d,
                           map(p+h.yxy).d - map(p-h.yxy).d,
                           map(p+h.yyx).d - map(p-h.yyx).d ) );
}






vec3 env(vec3 dir, bool includeSun) {
    vec3 col = mix(vec3(0), skyColor, smoothstep(-.2, .2, dir.y));
    return col;
}

struct Hit {
    Model model;
    vec3 pos;
    float rayLength;
};

Hit march(vec3 origin, vec3 rayDirection, float maxDist, float understep) {

    vec3 rayPosition = origin;
    float rayLength, dist = 0.;
    Model model;

    for (int i = 0; i < 200; i++) {
        rayPosition = origin + rayDirection * rayLength;
        model = map(rayPosition);
        rayLength += model.d * understep;

        float t = .0002;

        if (model.d < t) break;

        if (rayLength > maxDist) {
            model.id = 0;
            break;
        }
    }
    return Hit(model, rayPosition, rayLength);
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

// Walk on spheres subsurface scattering
// inspired by blackle https://www.shadertoy.com/view/wsfBDB
Hit walkOnSpheres(vec3 origin, vec3 normal, float startdepth, inout vec2 seed) {
    Model model;
    
    vec2 lastSeed = seed;
    seed = hash22(seed);
    normal = normalize(tan(vec3(seed.x, seed.y, lastSeed.x) * 2. - 1.));
    
    model = map(origin - normal * startdepth);
    origin -= normal * abs(model.d);
    
    for (int v = 0; v < 250; v++) {
        model = map(origin);

        if (abs(model.d) < .00002) break;
        
        vec2 lastSeed = seed;
        seed = hash22(seed);
        vec3 dir = normalize(tan(vec3(seed.x, seed.y, lastSeed.x) * 2. - 1.));
        
        origin += dir * abs(model.d);
    }
    return Hit(model, origin, 0.);
}

vec3 sampleDirect(Hit hit, vec3 nor, vec3 throughput, inout vec2 seed) {
    vec3 col = vec3(0);
    vec3 lightDir = (sunPos - hit.pos);
    vec3 lightSampleDir = getConeSample(lightDir, .0005, seed);
    seed = hash22(seed);
    float diffuse = dot(nor, lightSampleDir);
    vec3 shadowOrigin = hit.pos + nor * (.0002 / abs(dot(lightSampleDir, nor)));
    if (diffuse > 0.) {
        Hit sh = march(shadowOrigin, lightSampleDir, 1., 1.);
        if (sh.model.id == 0) {
            col += throughput * sunColor/10. * diffuse;
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

vec3 sampleDirectSpec(Hit hit, vec3 rayDir, vec3 nor, float rough, inout vec2 seed) {
    vec3 lpos = sphereLight(sunPos, 5., hit.pos, rayDir, nor);
    
    vec3 lightDir = normalize(lpos - hit.pos);
    vec3 h = normalize(rayDir + lightDir);
    float specular = pow(clamp(dot(h, nor), 0., 1.), 64.0);

    vec3 col = vec3(0);

    float fresnel = pow(max(0., 1. + dot(nor, rayDir)), 5.);
    specular = ggx(nor, rayDir, lightDir, rough, fresnel);

    vec3 shadowOrigin = hit.pos + nor * (.0002 / abs(dot(lightDir, nor)));
    if (specular > 0.) {
        Hit sh = march(shadowOrigin, lightDir, 1., 1.);
        if (sh.model.id == 0) {
            col += sunColor * specular * .1;
        }
    }
    return col;
}

vec3 traceGeo(vec3 origin, vec3 rayDir, vec2 seed, out float depth) {
    vec3 col = vec3(0);

    Hit hit;
    vec3 nor, ref;
    Material material;
    vec3 throughput = vec3(1);
    vec3 bgCol = vec3(.00,.04,.2) * .15;
    bool doSpecular = true;
    float pathLength = 0.;

    #ifndef PREVIEW
        const int MAX_BOUNCE = 3;
    #else
        const int MAX_BOUNCE = 1;
    #endif

    for (int bounce = 0; bounce < MAX_BOUNCE; bounce++) {
   
        hit = march(origin, rayDir, 6., 1.);

        if (bounce == 0) {
            depth = hit.rayLength;
        }

        if (hit.model.id == 0)
        {
            if (bounce > 0 && ! doSpecular) {
                col += env(rayDir, doSpecular) * throughput;
            } else {
                col = bgCol;
            }
            depth = 1e12;
            break;
        }

        nor = calcNormal(hit.pos);
        material = shadeModel(hit.model, nor);

        // calculate whether we are going to do a diffuse or specular reflection ray 
        seed = hash22(seed);
        doSpecular = hash12(seed) < material.specular;
        
        bool doSSS = material.sss && bounce < 1 && ! doSpecular;
        if (doSSS) {
            seed = hash22(seed);
            doSSS = hash12(seed) < .8;
        }
        
        if ( ! doSpecular) {
            // update the colorMultiplier

            float fogAmount = 1.0 - exp( -hit.rayLength * .4 );
            throughput *= mix(material.albedo, bgCol, fogAmount);
        }

        if (doSSS) {
            origin = hit.pos;
            
            seed = hash22(seed);
            hit = walkOnSpheres(origin, nor, .075, seed);
            nor = calcNormal(hit.pos);

            float extinctionDist = distance(origin, hit.pos) * 10.;
            vec3 extinctionCol = material.albedo;
            extinctionCol = mix(mix(extinctionCol, vec3(0,0,1), .5), vec3(1,0,0), clamp(extinctionDist - 1., 0., 1.));
            vec3 extinction = (1. - extinctionCol);
            extinction = 1. / (1. + (extinction * extinctionDist));	
            extinction = clamp(extinction, vec3(0), vec3(1));
            throughput *= extinction;
        }

        // Calculate diffuse ray direction
        seed = hash22(seed);
        vec3 diffuseRayDir = getSampleBiased(nor, 1., seed);

        if ( ! doSpecular)
        {
            seed = hash22(seed);
            col += sampleDirect(hit, nor, throughput, seed);
            rayDir = diffuseRayDir;

            #ifdef PREVIEW
                col += env(rayDir, doSpecular) * throughput;
            #endif
        }
        else
        {
            if (bounce == 0) { // fix fireflies from diffuse-bounce specular
                seed = hash22(seed);
                col += sampleDirectSpec(hit, rayDir, nor, material.roughness, seed) * throughput;
            }
            
            // Calculate specular ray direction
            vec3 specularRayDir = reflect(rayDir, nor);
            rayDir = normalize(mix(specularRayDir, diffuseRayDir, material.roughness * material.roughness));
        }

        // offset from sufrace https://www.shadertoy.com/view/lsXGzH
        origin = hit.pos + nor * (.0002 / abs(dot(rayDir, nor)));
    }

    return col;
}

mat3 rotX(float a) {
    return mat3(1,0,0, 0,cos(a),-sin(a), 0,sin(a),cos(a));
}

mat3 rotY(float a) {
    return mat3(cos(a),0,sin(a), 0,1,0, -sin(a),0,cos(a));
}

mat3 rotZ(float a) {
    return mat3(cos(a),-sin(a),0, sin(a),cos(a),0, 0,0,1);
}


//const float sqrt3 = 1.7320508075688772;

// main path tracing loop, based on yx's
// https://www.shadertoy.com/view/ts2cWm
// with a bit of demofox's
// https://www.shadertoy.com/view/WsBBR3
vec4 draw(vec2 fragCoord, int frame) {

    vec2 p = (-iResolution.xy + 2.* fragCoord) / iResolution.y;
    
    p *= .85;

    vec2 seed = hash22(fragCoord + (float(frame)) * sqrt3);
    
    // jitter for antialiasing
    p += 2. * (seed - .5) / iResolution.xy;


    float focalLength = 6.;
    vec3 camPos = vec3(0,0,.4) * focalLength * 1.;
    vec3 camTar = vec3(0);
    
    vec3 ww = normalize(camTar - camPos);
    vec3 uu = normalize(cross(vec3(0,1,0),ww));
    vec3 vv = normalize(cross(ww,uu));
    mat3 camMat = mat3(-uu, vv, ww);

    vec3 rayDir, origin;


    //if (fract(p.y * 20.) > .5)
    if (false)
    {
        rayDir = normalize(camMat * vec3(p.xy, focalLength));
        origin = camPos;
    } else {
        camMat = inverse(mat3(vView));
        origin = eye;

        //origin += camMat * vec3(sin(time * PI * 2.), cos(time * PI * 2.), 0) * .001;
        //camMat *= rotX(sin(time * PI * 4.) * -.001) * rotY(cos(time * PI * 2.) * -.0005);


        rayDir = normalize(camMat * vec3(p.x * fov, p.y * fov, -1.));
        focalLength = (1. / fov);
    }


    #ifdef DOF
    float fpd = .277 * focalLength;
    vec3 fp = origin + rayDir * fpd;
    vec2 off = rndunit2(seed);
    origin = origin + camMat * vec3(off, 0.) * .15;
    rayDir = normalize(fp - origin);
    #endif

    vec3 col = vec3(0);
    
    float depth = 1e12;

    col = traceGeo(origin, rayDir, seed, depth);    

    #ifndef PREVIEW
    vec4 dust = traceDust(origin, rayDir, depth);

    col = mix(col, dust.rgb, dust.a);
    #endif

    //col = vec3(1) * depth * .1;

    #ifdef DOF
    float lo = length(off);
    col = mix(col, col * mix(vec3(1,1,0), vec3(0,0,1), lo * lo) * 2., .666);
    //col *= spectrum(lo * lo) * 1.5;
    #endif

    return vec4(col, 1);
}

void mainImage(out vec4 fragColor, in vec2 fragCoord) {

    init();

    time = 1.3;
    time = fract(iTime);
    //time = 3.730;

    vec4 col = draw(fragCoord, iFrame);

    if (drawIndex > 0.) {
        vec4 lastCol = texture2D(previousSample, fragCoord.xy / iResolution.xy);
        col = mix(lastCol, col, 1. / (drawIndex + 1.));
    }
    
    fragColor = vec4(col.rgb, 1);
}