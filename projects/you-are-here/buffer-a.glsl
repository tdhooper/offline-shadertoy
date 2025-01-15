#version 300 es

// framebuffer drawcount: 1

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

out vec4 fragColor;

void mainImage(out vec4 a, in vec2 b);

void main() {
    mainImage(fragColor, gl_FragCoord.xy);
}

//#define DOF


//========================================================
// Tools
//========================================================

#define PI 3.1415926

// HG_SDF

void pR(inout vec2 p, float a) {
    p = cos(a)*p + sin(a)*vec2(p.y, -p.x);
}
float vmax2(vec2 v) {
	return max(v.x, v.y);
}
float vmax(vec3 v) {
	return max(max(v.x, v.y), v.z);
}
float vmin2(vec2 v) {
	return min(v.x, v.y);
}
float sum(vec3 v) {
    return v.x + v.y + v.z;
}
float pReflect(inout vec3 p, vec3 planeNormal, float offset) {
	float t = dot(p, planeNormal)+offset;
	if (t < 0.) {
		p = p - (2.*t)*planeNormal;
	}
	return sign(t);
}
float fBox(vec3 p, vec3 b) {
	vec3 d = abs(p) - b;
	return length(max(d, vec3(0))) + vmax(min(d, vec3(0)));
}
float fBox2(vec2 p, vec2 b) {
	vec2 d = abs(p) - b;
	return length(max(d, vec2(0))) + vmax2(min(d, vec2(0)));
}
float fHalfCapsule(vec2 p, float r) {
    p.y = max(p.y, 0.);
    return length(p) - r;
}

float smax(float a, float b, float r) {
    vec2 u = max(vec2(r + a,r + b), vec2(0));
    return min(-r, max (a, b)) + length(u);
}
float smin(float a, float b, float k){
    float f = clamp(0.5 + 0.5 * ((a - b) / k), 0., 1.);
    return (1. - f) * a + f  * b - f * (1. - f) * k;
}

// Rotate on axis, blackle https://suricrasia.online/demoscene/functions/
vec3 erot(vec3 p, vec3 ax, float ro) {
  return mix(dot(ax,p)*ax, p, cos(ro))+sin(ro)*cross(ax,p);
}


float unlerp(float vmin, float vmax, float value) {
    return clamp((value - vmin) / (vmax - vmin), 0., 1.);
}

float smoothbump(float a, float b, float x) {
    float c = mix(a, b, .5);
    return smoothstep(a, c, x) - smoothstep(c, b, x);
}



//========================================================
// Noise
//========================================================

// https://www.shadertoy.com/view/4djSRW
vec2 hash22(vec2 p)
{
    p += 1.61803398875; // fix artifacts when reseeding
	vec3 p3 = fract(vec3(p.xyx) * vec3(.1031, .1030, .0973));
    p3 += dot(p3, p3.yzx+33.33);
    return fract((p3.xx+p3.yz)*p3.zy);
}


// https://www.shadertoy.com/view/4djSRW
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

// https://suricrasia.online/demoscene/functions/
vec3 rndunit(float seed) {
  return normalize(tan(hash31(seed)));
}




//========================================================
// Background
//========================================================

const float sqrt3 = 1.7320508075688772;
const float i3 = 0.5773502691896258;

const mat2 cart2hex = mat2(1, 0, i3, 2. * i3);
const mat2 hex2cart = mat2(1, 0, -.5, .5 * sqrt3);

void swirl(inout vec2 p, float scale, float radius, float power, vec2 seed) {

    p += seed;
    
    p *= scale;

    mat2 sc = mat2(1,0,0,.785);
    mat2 sci = inverse(sc);
    
    vec2 h = p * sc * cart2hex;
    vec2 hf = floor(h);
    float d = 1e12;
    float c = 0.;
    float dc;
    
    for (int j = -2; j <= 2; j++)
    for (int i = -2; i <= 2; i++) {
        vec2 cellId = hf + vec2(i,j);
        float rnd = hash12(cellId);
        vec2 hp = cellId * hex2cart * sci;
    	dc = length(p - hp);
        if (dc < d) {
        	d = dc;
            c = mix(.1, 1., rnd);            
        }

        p -= hp;
        pR(p, smoothstep(radius, 0., length(p)) * power * (rnd * 2. - 1.));
        p += hp;
    }
    
    p /= scale;

    p -= seed;
}

vec4 cloudDist(vec2 p, vec2 s, float r, float blur)
{
    p /= s;
    blur = mix(0., blur, smoothstep(r*2., -r*2., p.y));
    float d = (length(p) - r + blur / 2.) / blur;
    d = smoothstep(1., 0., d);
    r += blur / 2.;
    p /= r;
    vec3 n = vec3(p, max(0., sqrt(1. - dot(p, p))));
    return vec4(n, d);
}


vec4 cloud(vec2 p, vec2 s, float r, float blur) {
    vec4 res = cloudDist(p, s, r, blur);
    vec3 ld = normalize(vec3(.5,1,.5));
    float l = dot(res.xyz, ld);
    l = l * .5 + .5;
    l = clamp(l, 0., 1.);
    vec3 col = vec3(l);
    float a = res.w;
    return vec4(col, a);

}

vec2 warp(vec2 p, vec2 seed) {
    p += seed;
    swirl(p, 4., .3, 8., vec2(0));
    swirl(p, 2., .3, 6., vec2(.9));
    swirl(p, 2., .5, 6., vec2(.5));
    swirl(p, .76, .75, 4., vec2(1. / 4.));
    swirl(p, 1.72, .75, 3., vec2(1. * .1));
    //swirl(p, 4., .75, 6., vec2(.5 + 1. / 6.));
    swirl(p, 6., 1., 5., vec2(.5 + 1. / 6.));
    p -= seed;
    return p;
}

// return distance, and cell id
vec2 voronoi( in vec2 x )
{
    vec2 n = floor( x );
    vec2 f = fract( x );

	vec3 m = vec3( 8.0 );
    for( int j=-1; j<=1; j++ )
    for( int i=-1; i<=1; i++ )
    {
        vec2  g = vec2( float(i), float(j) );
        vec2  o = hash22( n + g );
	    vec2  r = g - f + o;
        float size = hash12( n + g );
		float d = length(r) - mix(-.05, .05, size);
        if( d<m.x )
            m = vec3( d, o );
    }

    return vec2( m.x, m.y+m.z );
}

void scatterclouds(vec2 p, inout vec4 col) {
    float smallestDist = 1e12;
    vec2 closestPoint = vec2(0);
    vec2 cellId;
    for (int x = -1; x <= 1; x++) {
        for (int y = -1; y <= 1; y++) {
            vec2 cell = round(p) + vec2(x,y);
            vec2 offset = hash22(cell) * 2. - 1.;
            vec2 point = cell + offset * .5;
            float r = hash12(cell);
            vec4 cl = cloud(p - point, vec2(.2), mix(.5, 2., r), 5.);
            col.rgb = mix(col.rgb, cl.rgb, cl.w);
            col.a = max(col.a, cl.a);
		}
    }
}

vec3 skyTex(vec2 p)
{   
    p = warp(p, vec2(0));
    
    vec2 p2 = p;
    vec2 p3 = p;
    
    swirl(p2, .2, .75, 3., vec2(0.));
    swirl(p3, .2, .75, 3., vec2(10.));
    
    vec4 col = vec4(.5,.5,.5,0);

    vec2 sc = vec2(18.,6.);
    scatterclouds(p2 / sc, col);
    scatterclouds(p3 / sc, col);

    return col.rgb;
}





//========================================================
// Modelling
//========================================================

struct Material {
    vec3 albedo;
    float specular;
    float roughness;
};

struct Model {
    float d;
    vec3 uvw;
    vec3 albedo;
    int id;
};

Material shadeModel(Model model, inout vec3 nor) {
    int id = model.id;
    vec3 p = model.uvw;

    // floorboards
    if (id == 2) {
        vec3 col = mix(model.albedo * vec3(.5,.3,.2), model.albedo + .1, .9);
        vec2 size = vec2(.1,.02);        
        vec2 c = floor((p.zx + size / 2.) / size);
        p.z += mod(c.y, 2.) * size.x * .5;
        c = floor((p.zx + size / 2.) / size);
        p.zx = (fract(p.zx / size + .5) - .5);
        p.zx *= size;
        c *= size;
        float d = fBox2(p.zx, size / 2.);
        float r = hash12((c+120.) * 80.);
        float r2 = hash12((c+1.) * 60.);
        col = mix(col / 2., col * vec3(1.05,.95,1.05), r2);
        col *= mix(.1, 1., smoothstep(.0, -.0006, d));
        col *= mix(.5, 1., r);
        vec3 ax = rndunit(r * 5.);
        //nor = erot(nor, ax, (r * 2. - 1.) * .01);
        p /= 2.;
        p += length(sin(sin(p * 100. + r * 10.) * 10.)) * .0005;
        p.xz += size * (hash22(c * 20.) * 2. - 1.);
        p.z /= 10.;
        p = erot(p, ax, (r * 2. - 1.) * 2.2);
        p += sin((p + r * 100.) * 200.) * .04;
        d = length(p);
        d = sin(d * 1500.) * .5 + .5;
        col *= mix(.5, 1., d);
        return Material(col, .8, mix(.4, .2, d));
    }

    return Material(model.albedo, 0., 0.);
}

vec3 sofasz = vec3(.2, .1, .1) / 2.;
    
Model fSofa(vec3 p) {
    
    vec3 ppp = p;

    p += sum(sin(erot(p, vec3(1), 1.) * 6000.)) * .000015;
    
    float fade, d2, d3, d4, ar, armang, ar0, ar1, arw, footh, psx, vary, baseh, cs, cr, axisx, axisz, crw, seam, buttpatch, br, scl;
    vec2 armtopp;
    vec3 isofasz, pp, armsz, pc, col, backsz, cushionsz;

    armsz = vec3(.012,.026,sofasz.z);
    footh = .0075;
    isofasz = sofasz - vec3(armsz.x * 2., 0, 0);

    psx = sign(p.x);
    vary = max(psx, 0.);
    pp = p;

    // arms
    p.x = abs(p.x);
    p.x -= sofasz.x - armsz.x;
    p.y -= footh - sofasz.y + armsz.y;
    scl = gmTransform(p);
    d2 = fHalfCapsule(p.xy - vec2(0, armsz.y), armsz.x);
    armtopp = p.xy - vec2(.004, armsz.y + .005);
    d2 = smin(d2, length(armtopp) - armsz.x - .002, .01);
    d3 = d2 + armsz.x * .4;
    ar = .007;
    armang = atan(armtopp.y, armtopp.x);
    ar0 = sin(sin(sin(armang) * 10. - vary*2.) * 5. + length(armtopp) * 500. + p.z * 100.);
    ar1 = sin(sin(sin((p.y + 1.) * 65.) * 10.) * 5. + p.x * 300. + p.z * 100.);
    arw = mix(ar0, ar1, smoothstep(.01, -.01, p.y - armsz.y / 3.));
    arw *= smoothstep(0., armsz.z, abs(p.z));
    fade = mix(.33, 0., arw) * smoothstep(.01, .0, length(vec2(d2, 2. * abs(abs(p.z) - armsz.z + .0035)))) * smoothstep(-.03, .06, p.y);// mix(1., 0., arw) // * mix(.5, 1., smoothstep(.005, 0., abs(abs(p.z) - armsz.z + .005)));
    d2 = smax(d2 + arw * .0001, abs(p.z) - armsz.z, ar + arw * .0005);
    d3 = max(d3, -p.z);
    if (d3 > 0.) {
        fade = max(fade, smoothbump(.5, 3., mod(armang - .5, PI * 2.)) * smoothbump(-armsz.z / 2., armsz.z * 1.5, p.z)) *  mix(1., .5, arw);
    }
    ar = .00725;
    d4 = smax(d3, abs(p.z) - armsz.z - .003, ar);
    d2 = smax(d2, -d4, .001);
    ar += sin(sin(sin((p.y + 1.) * 80.) * 10.) * 5. + p.x * 300.) * .0003;
    d4 = smax(d3, abs(p.z) - armsz.z - .003, ar);
    d2 = min(d2, d4);
    d2 = smax(d2, -p.y - armsz.y, .003);
    d2 *= scl;
	

    // base
    p = pp;
    baseh = .012;
    p.y -= footh - sofasz.y + baseh;
    scl = gmTransform(p);

    br = .002;
    br += sin(sin(sin(p.x * 55. + 2.) * 10.) * 5. + p.y * 100.) * .00025;
	
    d3 = fBox(p, vec3(isofasz.x + .0068, baseh, isofasz.z) - br) - br;

    d2 = min(d2, d3);
    p.x = abs(p.x);
    p.xy -= vec2(sofasz.x / 2.5, 0.);
    p.y += sin(p.x * 250.) * .0005;
    p.xy *= vec2(.4, 1);
    fade = max(fade, pow(smoothstep(.025, .0, length(p.xy)), 3.)/2.);

	/*
    // back
    p = pp;
    backsz = vec3(isofasz.x + .001, armsz.y + .005, .005);
    p.y -= footh - sofasz.y + baseh + backsz.y;
    p.z -= -sofasz.z + backsz.z;
    d3 = fBox(p.zyx, backsz.zyx - .006) - .006;
    d2 = min(d2, d3);
	*/
    // cushion
    p = pp;
    p.x = abs(p.x);
    cushionsz = vec3(isofasz.x / 2. + .001, .01, sofasz.z - .018);
    p.y -= footh - sofasz.y + baseh * 2. - .001;
    p.y -= cushionsz.y * 2.;
    p.x -= isofasz.x / 2.;
    p.z -= sofasz.z - cushionsz.z * 2. + .005;
    vary += pReflect(p, normalize(vec3(0,-.66,1)), 0.);
    p.y += cushionsz.y;
    p.z -= cushionsz.z - .004;
    scl = gmTransform(p);

    cs = mix(.95, 1.01, length(sin(sin((p + vary * 240.) * 30.) * 3.) * .5 + .5));
    cs = 1. + sum(sin((p + vary * 2. + 1.) * 100.) * vec3(1,0,1)) * .02;
    cr = .008;
    axisx = max(vmin2(p.xz), vmin2(-p.xz));
    axisz = min(vmin2(p.xz), vmin2(-p.xz));
		
    crw = sin(sin(sin((axisx + mix(.4, .6, vary)) * (48. + vary)) * 10.) * 5. + p.y * 300. + p.z * 300.);
    crw *= smoothstep(0., .0005, abs(dot(abs(p.xz), cushionsz.zx * vec2(1,-1))));
    crw *= unlerp(.5, 1., vmax2(abs(p.xz) / cushionsz.xz));
    cr += crw * .0003;

    cs += smoothstep(cushionsz.x * 1.2, 0., length(p.xz)) * .4;
    pc = p;
    pc.x += sin(pc.z * 150. + vary * 3.) * .02;
    pR(pc.xz, -vary * .2 + 1.6);
    buttpatch = sum(sin((sin(2. + vec3(pc.x, pc.y, 0) * 122. * (1. + abs(vary - 1.) * .2)) + vary * 30.) * 8.));
    
    buttpatch *= smoothstep(cushionsz.z, 0., length(p.xz));
    cs += buttpatch * .03;

    d3 = (fBox(p / cs, cushionsz - cr - crw * .0001) - cr) * cs* .9;
    seam = abs(p.y) - cushionsz.y * .75;
    d3 = smax(d3, -abs(seam), .001);
    d3 *= scl;

    if (d3 < d2) {
        d2 = d3;
        fade = 0.;
        fade += smoothbump(.0, .0015, seam) * mix(.5, .25, crw);
        fade += smoothbump(.0, .00075, -seam) * mix(.5, .25, crw);
        fade = max(fade, smoothstep(cushionsz.x * 1.1, 0., length(p.xz)));
        if (psx < 0.) {
            fade /= 3.;
        } else {
            fade *= 1.2;
            fade += .1;
        }
    }
    

    col = pow(vec3(0.55,0.29,0.23), vec3(2.2));
    col = mix(col, vec3(1.,.1,.0) * .05 * 6., .6);
    col.r -= .05;
    col.g += .006;
    col.b += .006;
    col = mix(col, mix(col * 1.75, vec3(.2), .04), fade);

    int id = 15;

    p = ppp;
    p.y += sofasz.y;
   
    p.y -= .0015;
    p.xz = abs(p.xz);
    p.xz -= sofasz.xz * vec2(.93,.68);
    scl = gmTransform(p);
    d3 = max(fBox(p, vec3(0,.01,0)) - .005, -p.y);  
    d3 *= scl;
    if (d3 < d2) {
        d2 = d3;
        col = vec3(.01);
        id = 3;
    }

    return Model(d2, p, col, 15);
}


Model map(vec3 p) {
    // p.x = -p.x;

    vec3 p2 = p;
    float scl2 = gmTransform(p2);
    Model m = fSofa(p2);
    m.d *= scl2;
    
    p.y += sofasz.y;
    float d = fBox(p, vec3(.14,.003,.14));
        
    vec3 uvw = p;
    Model m2 = Model(d, uvw, vec3(0.714,0.43,0.19), 2);
    if (m2.d < m.d) m = m2;    
    
    return m;
}

float GIZMO_MAP(vec3 p) {
    return map(p).d;
}

//========================================================
// Rendering
//========================================================

// https://iquilezles.org/articles/normalsSDF
vec3 calcNormal( in vec3 p )
{
    const float h = 0.0001;
    #define ZERO (min(iFrame,0)) // non-constant zero
    vec3 n = vec3(0.0);
    for( int i=ZERO; i<4; i++ )
    {
        vec3 e = 0.5773*(2.0*vec3((((i+3)>>1)&1),((i>>1)&1),(i&1))-1.0);
        n += e*map(p+e*h).d;
    }
    return normalize(n);
}

vec3 sunPos = normalize(vec3(-5,4,.5)) * 100.;
vec3 skyColor = vec3(0.50,0.70,1.00);

vec3 env(vec3 dir) {
    vec3 col = mix(vec3(.5,.7,1) * .05, vec3(.5,.7,1) * 1., smoothstep(-.4, .4, dir.y));
    vec2 pc = vec2(atan(dir.z, dir.x), dir.y) * 30. - 28.96 * 10.;
    vec3 cl = skyTex(pc);
    col *= cl;
    col += pow(cl, vec3(15.)) * 2.;
    return col;
}

struct Hit {
    Model model;
    vec3 pos;
};

Hit march(vec3 origin, vec3 rayDirection, float maxDist, float understep) {

    vec3 rayPosition;
    float rayLength, dist = 0.;
    Model model;

    for (int i = 0; i < 200; i++) {
        rayPosition = origin + rayDirection * rayLength;
        model = map(rayPosition);
        rayLength += model.d * understep;

        if (abs(model.d) / rayLength < .0002) break;

        if (rayLength > maxDist) {
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
vec3 getSampleBiased(vec3  dir, float power, vec2 seed) {
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

// iq https://www.shadertoy.com/view/tl23Rm
vec2 rndunit2(vec2 seed ) {
    vec2 h = seed * vec2(1,6.28318530718);
    float phi = h.y;
    float r = sqrt(h.x);
	return r*vec2(sin(phi),cos(phi));
}


void getCamera(out vec3 origin, out vec3 rayDir, vec2 seed) {
    origin = eye;
    rayDir = normalize(dir);

    #ifdef DOF

    // position on sensor plane
    vec3 cameraForward = -transpose(vView)[2].xyz;
    Hit dofHit = march(origin, cameraForward, 100., 1.);
    float focalDistance = length(origin - dofHit.pos) - fov;

    float focalPlaneOffset = dot(rayDir, cameraForward) / fov;
    origin = origin + rayDir / focalPlaneOffset;

    // position on focal plane
    vec3 focalPlanePosition = origin + focalDistance * rayDir / dot(rayDir, cameraForward);
    origin = origin + vec3(rndunit2(seed), 0.) * mat3(vView) * .001;

    rayDir = normalize(focalPlanePosition - origin);

    // jitter for antialiasing
    origin += vec3(2. * (seed - .5) / iResolution.y, 0) * mat3(vView) * focalDistance * fov;

    origin -= rayDir / focalPlaneOffset;

    #else

    // position on sensor plane
    vec3 cameraForward = -transpose(vView)[2].xyz;
    float focalPlaneOffset = dot(rayDir, cameraForward);
    vec3 p = origin + rayDir / focalPlaneOffset;

    // jitter for antialiasing
    p += vec3(2. * (seed - .5) / iResolution.y, 0) * mat3(vView) * fov;
    
    rayDir = normalize(p - origin);

    #endif
}

// main path tracing loop, based on yx's
// https://www.shadertoy.com/view/ts2cWm
// with a bit of demofox's
// https://www.shadertoy.com/view/WsBBR3
vec4 draw(vec2 fragCoord, int frame) {

    vec2 p = (-iResolution.xy + 2.* fragCoord) / iResolution.y;
    //p /= 2.;

    vec2 seed = hash22(fragCoord + (float(frame)) * sqrt3);
    
    // jitter for antialiasing
    p += 2. * (seed - .5) / iResolution.xy;

    vec3 col = vec3(0);

    float focalLength = 6.;
    vec3 camPos = vec3(.04, .01, .1) * focalLength * 1.2;
    if (iMouse.x > 0.) {
        vec2 im = (iMouse.xy / iResolution.xy) - .5;
        pR(camPos.yz, -im.y * 1.5);
        pR(camPos.xz, -im.x * PI * 1.5);
    }

    vec3 camTar = vec3(0);
    
    vec3 ww = normalize(camTar - camPos);
    vec3 uu = normalize(cross(vec3(0,1,0),ww));
    vec3 vv = normalize(cross(ww,uu));
    mat3 camMat = mat3(-uu, vv, ww);
    
    vec3 rayDir = normalize(camMat * vec3(p.xy, focalLength));
    vec3 origin = camPos;

    getCamera(origin, rayDir, seed);

    Hit hit = march(origin, rayDir, 5., .9);

    vec3 nor, ref;
    Material material;
    vec3 accum = vec3(1);
    vec3 bgCol = skyColor;

    const int MAX_BOUNCE = 6;

    for (int bounce = 0; bounce < MAX_BOUNCE; bounce++) {
   
        if (hit.model.id == 0) {
            col += env(rayDir) * accum;
            break;
        }

        nor = calcNormal(hit.pos);
        material = shadeModel(hit.model, nor);

        // calculate whether we are going to do a diffuse or specular reflection ray 
        bool doSpecular = hash12(seed) < material.specular;

        // update the colorMultiplier
       	accum *= material.albedo;

        // Calculate diffuse ray direction
        seed = hash22(seed);
        vec3 diffuseRayDir = getSampleBiased(nor, 1., seed);

        if ( ! doSpecular) {
            
            // calculate direct lighting
            vec3 directLight = vec3(0);
            seed = hash22(seed);

            vec3 lightDir = (sunPos - hit.pos);
            vec3 lightSampleDir = getConeSample(lightDir, .005, seed);
            float diffuse = dot(nor, lightSampleDir);
            vec3 shadowOrigin = hit.pos + nor * (.0002 / abs(dot(lightSampleDir, nor)));
            if (diffuse > 0.) {
                Hit sh = march(shadowOrigin, lightSampleDir, 1., 1.);
                if (sh.model.id == 0) {
                    col += accum * vec3(8.10,6.00,4.20)/10. * diffuse;
                }
            }

            rayDir = diffuseRayDir;
        } else {
            // Calculate specular ray direction
            vec3 specularRayDir = reflect(rayDir, nor);
            rayDir = normalize(mix(specularRayDir, diffuseRayDir, material.roughness * material.roughness));
        }

        // offset from sufrace https://www.shadertoy.com/view/lsXGzH
        origin = hit.pos + nor * (.0002 / abs(dot(rayDir, nor)));
        seed = hash22(seed);
        hit = march(origin, rayDir, 1., 1.);
    }


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
