// framebuffer drawcount: 1

#extension GL_OES_standard_derivatives : enable

precision highp float;

uniform vec2 iResolution;
uniform mat4 cameraMatrix;
uniform sampler2D previousSample; // buffer-a.frag filter: linear
uniform float drawIndex;
uniform int iFrame;
uniform float iTime;
uniform vec4 iMouse;

uniform sampler2D iChannel0; // /images/blue-noise.png filter: linear wrap: repeat
uniform sampler2D revisionTex; // /images/revision-logo.png filter: linear
uniform sampler2D linenTex; // /images/linen.jpg filter: linear wrap: mirror
uniform vec2 iChannel0Size;
uniform vec2 revisionTexSize;

uniform float guiSkyX;
uniform float guiSkyY;

varying vec3 eye;
varying vec3 dir;
varying float fov;
varying float aspect;
varying mat4 vView;


void mainImage(out vec4 a, in vec2 b);

void main() {
    mainImage(gl_FragColor, gl_FragCoord.xy);
}

//#define ROOM_ONLY
//#define FREE_FLY
//#define HIDE_ROOM;


float time;
float timeRaw;

#define PI 3.1415926

#pragma glslify: inverse = require(glsl-inverse)

float round(float v) {
    return floor(v + .5);
}

vec2 round(vec2 v) {
    return floor(v + .5);
}

//========================================================
// Tools
//========================================================

#define PHI 1.618033988749895

#define saturate(x) clamp(x, 0., 1.)

// HG_SDF
float dot2( in vec2 v ) { 
    return dot(v,v);
}
void pR(inout vec2 p, float a) {
    p = cos(a)*p + sin(a)*vec2(p.y, -p.x);
}
void pR45(inout vec2 p) {
	p = (p + vec2(p.y, -p.x))*sqrt(0.5);
}
float vmax(vec3 v) {
	return max(max(v.x, v.y), v.z);
}
float vmax(vec2 v) {
	return max(v.x, v.y);
}
float vmin(vec2 v) {
	return min(v.x, v.y);
}
float vmin(vec3 v) {
	return min(min(v.x, v.y), v.z);
}
float vsin(vec3 v) {
    v = sin(v);
    return v.x + v.y + v.z;
}
float vsin(vec2 v) {
    v = sin(v);
    return v.x + v.y;
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
float fBox(vec2 p, vec2 b) {
	vec2 d = abs(p) - b;
	return length(max(d, vec2(0))) + vmax(min(d, vec2(0)));
}
float fBox(vec3 p, vec3 b, out vec3 face) {
	vec3 d = abs(p) - b;
    face = step(d.yzx, d.xyz)*step(d.zxy, d.xyz)*sign(p);
    return length(max(d, vec3(0))) + vmax(min(d, vec3(0)));
}
float fBoxHalf(vec2 p, vec2 b) {
	vec2 d = p;
    d.y = abs(d.y);
    d -= b;
	return length(max(d, vec2(0))) + vmax(min(d, vec2(0)));
}
float fCorner(vec2 p, vec2 b) {
	vec2 d = p;
    d -= b;
	return length(max(d, vec2(0))) + vmax(min(d, vec2(0)));
}
float fCorner(vec2 p) {
	return length(max(p, vec2(0))) + vmax(min(p, vec2(0)));
}
float fMailbox(vec2 p, vec2 b) {
    bool top = p.y > 0.;
    p = abs(p);
    float o;
    if (top) {
        p.y -= b.y;
        o = -b.x;
    } else {
        p -= b;
        o = vmax(min(p, 0.));
    }
    return length(max(p, 0.)) + o;
}
float fMailbox(vec3 p, vec3 b) {
    bool top = p.y > 0.;
    p = abs(p) - b;
    float o = vmax(min(p, 0.));
    if (top) {
        p.xz += b.xz;
        p.y = max(p.y, 0.);
        p.xy = vec2(length(p.xy), p.z) - b.xz;
        p.z = 0.;
        o = min(vmax(p.xy), 0.);
    }
    return length(max(p, 0.)) + o;
}
float fHalfCapsule(vec2 p, float r) {
    p.y = max(p.y, 0.);
    return length(p) - r;
}
float sdCappedCylinder( vec3 p, float h, float r )
{
  vec2 d = abs(vec2(length(p.xz),p.y)) - vec2(h,r);
  return min(max(d.x,d.y),0.0) + length(max(d,0.0));
}
float smax(float a, float b, float r) {
    vec2 u = max(vec2(r + a,r + b), vec2(0));
    return min(-r, max (a, b)) + length(u);
}
float smin(float a, float b, float k){
    float f = clamp(0.5 + 0.5 * ((a - b) / k), 0., 1.);
    return (1. - f) * a + f  * b - f * (1. - f) * k;
}
float smin2(float a, float b, float k) {
    float h = max(0., k-abs(b-a))/k;
    return min(a,b)-h*h*h*k/6.;
}
float smin3(float a, float b, float r) {
    vec2 u = max(vec2(r - a,r - b), vec2(0));
    return max(r, min (a, b)) - length(u);
}
float smax2(float a, float b, float k) {
    return -smin3(-a, -b, k);
}


float fCornerBevel(vec2 p, vec2 c) {
    vec2 n = normalize(c.yx);   
    vec2 pc = p + vec2(c.x, 0);
    float e = dot(pc, n);
    float i = max(vmax(min(p, vec2(0))), min(e, 0.));
    float t = dot(pc, c * vec2(1,-1)) / dot2(c);
    vec2 v = vec2(0);
    if (t < 0.) {
        v = max(p + vec2(c.x,0), 0.);
    }
    if (t > 1.) {
        v = max(p + vec2(0,c.y), 0.);
    }
    e = max(length(v), e);
    return e + i;
}

// https://www.shadertoy.com/view/MsVGWG
float sdUnterprim(vec3 p, vec4 s, vec3 r, vec2 ba, float sz2) {
    vec3 d = abs(p) - s.xyz;
    float q = length(max(d.xy, 0.0)) + min(0.0,max(d.x,d.y)) - r.x;
    // hole support: without this line, all results are convex
    q = abs(q) - s.w;
    vec2 pa = vec2(q, p.z - s.z);
    vec2 diag = pa - vec2(r.z,sz2) * clamp(dot(pa,ba), 0.0, 1.0);
    vec2 h0 = vec2(max(q - r.z,0.0),p.z + s.z);
    vec2 h1 = vec2(max(q,0.0),p.z - s.z);   
    return sqrt(min(dot(diag,diag),min(dot(h0,h0),dot(h1,h1))))
        * sign(max(dot(pa,vec2(-ba.y, ba.x)), d.z)) - r.y;
}

// s: width, height, depth, thickness
// r: xy corner radius, z corner radius, bottom radius offset
float sdUberprim(vec3 p, vec4 s, vec3 r) {
    // these operations can be precomputed
    s.xy -= r.x;
    r.x -= s.w;
    s.w -= r.y;
    s.z -= r.y;
    vec2 ba = vec2(r.z, -2.0*s.z);
    return sdUnterprim(p, s, r, ba/dot(ba,ba), ba.y);
}

// generic ellipsoid - approximated distance: https://www.shadertoy.com/view/tdS3DG
float sdEllipsoid( in vec3 p, in vec3 r ) 
{
    float k0 = length(p/r);
    float k1 = length(p/(r*r));
    return k0*(k0-1.0)/k1;
}

// Rotate on axis, blackle https://suricrasia.online/demoscene/functions/
vec3 erot(vec3 p, vec3 ax, float ro) {
  return mix(dot(ax,p)*ax, p, cos(ro))+sin(ro)*cross(ax,p);
}

// https://www.shadertoy.com/view/tdXBWX
float fSin(vec2 p, vec2 scale) {
    p.x = asin(sin(p.x * scale.x) * .9) / scale.x;
    float d1 = dot(p, normalize(scale));
    float d2 = 1. / scale.y - p.y;
    float d3 = p.y + 1. / scale.y;
    float sm = 1. / sqrt(scale.x * scale.y);
    return -smin2(d2, -smin2(d1, d3, sm), sm);
}

float cmax(float a, float b, float r) {
	float m = max(a, b);
	if (r <= 0.) return m;
	if (((-a < r) && (-b < r)) || (m < 0.)) {
		return max(m, (a + r + b)*sqrt(0.5));
	} else {
		return m;
	}
}

float cmin(float a, float b, float r) {
    return -cmax(-a, -b, r);
}

float pMod1(inout float p, float size) {
	float halfsize = size*0.5;
	float c = floor((p + halfsize)/size);
	p = mod(p + halfsize, size) - halfsize;
	return c;
}

float stairmin(float a, float b, float r, float n) {
    float d = min(a, b);
    vec2 p = vec2(a, b);
    pR45(p);
    p = p.yx - vec2((r-r/n)*0.5*sqrt(2.));
    p.x += 0.5*sqrt(2.)*r/n;
    float x = r*sqrt(2.)/n;
    pMod1(p.x, x);
    d = min(d, p.y);
    pR45(p);
    return min(d, vmax(p -vec2(0.5*r/n)));
}

float stairmax(float a, float b, float r, float n) {
	return -stairmin(-a, -b, r, n);
}


float unlerp(float vmin, float vmax, float value) {
    return clamp((value - vmin) / (vmax - vmin), 0., 1.);
}

float sinbump(float a, float b, float x) {
    x = unlerp(a, b, x);
    return sin(x * PI * 2. - PI / 2.) * .5 + .5;
}

float smoothbump(float a, float b, float x) {
    float c = mix(a, b, .5);
    return smoothstep(a, c, x) - smoothstep(c, b, x);
}

float gain(float x, float P) {
    if (x > 0.5)
        return 1.0 - 0.5*pow(2.0-2.0*x, P);
    else
        return 0.5*pow(2.0*x, P);
}

float gain(float x, float P, float S) {
    return mix(x, gain(x, P), S);
}

float gain2(float x, float P, float S) {
    float s = sign(.5 - x);
    return gain(x + .5 * s, P, S) - .5 * s;
}

float gainB(float x, float P, float g) {
    x = clamp(unlerp(g/2., 1. - g/2., x), 0., 1.);
    return gain(x, P);
}

float gain2B(float x, float P, float g) {
    float s = sign(.5 - x);
    return gainB(x + .5 * s, P, g) - .5 * s;
}




// mla https://www.shadertoy.com/view/lsGyzm
vec4 inverseStereographic(vec3 p, out float k) {
  k = 2.0/(1.0+dot(p,p));
  return vec4(k*p,k-1.0);
}

vec3 stereographic(vec4 p4) {
  float k = 1.0/(1.0+p4.w);
  return k*p4.xyz;
}


//========================================================
// Noise
//========================================================

// https://www.shadertoy.com/view/4djSRW
vec2 hash22_original(vec2 p)
{
	vec3 p3 = fract(vec3(p.xyx) * vec3(.1031, .1030, .0973));
    p3 += dot(p3, p3.yzx+33.33);
    return fract((p3.xx+p3.yz)*p3.zy);
}

vec2 hash22(vec2 p)
{
    p += 1.61803398875; // fix artifacts when reseeding
    return hash22_original(p);
}

vec2 hash22b(vec2 p)
{
    return hash22_original(p);
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

float rnd(ivec2 uv) {
    return texture2D(iChannel0, vec2(uv) / iChannel0Size.xy, -10000.).r;
}

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
    
    for (int j = -1; j <= 1; j++)
    for (int i = -1; i <= 1; i++) {
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

vec4 shadeCloud(vec4 res) {
    vec3 ld = normalize(vec3(.5,1,.5));
    float l = dot(res.xyz, ld);
    l = l * .5 + .5;
    l = clamp(l, 0., 1.);
    vec3 col = vec3(l);
    float a = res.w;
    return vec4(col, a);
}

vec4 cloud(vec2 p, vec2 s, float r, float blur) {
    vec4 res = cloudDist(p, s, r, blur);
    vec4 col = shadeCloud(res);
    return col;
}

vec2 warp(vec2 p, vec2 seed) {
    p += seed;
    swirl(p, 4., .75, 2., vec2(.5));
    swirl(p, .66, .75, 2., vec2(1. / 4.));
    swirl(p, 1.72, .75, 3., vec2(1. * .1));
    //swirl(p, 4., .75, 6., vec2(.5 + 1. / 6.));
    swirl(p, 5.7, .75, 10., vec2(.5 + 1. / 6.));
    p -= seed;
    return p;
}

vec4 opU(vec4 a, vec4 b) {
    vec4 res = mix(a, b, b.w);
    res.w = max(a.w, b.w);
    return res;
}

vec4 voronoiCenters(vec2 p, float t) {
    float smallestDist = 1e12;
    vec2 closestPoint = vec2(0);
    vec2 cellId;
    for (int x = -1; x <= 1; x++) {
        for (int y = -1; y <= 1; y++) {
            vec2 cell = round(p) + vec2(x,y);
            vec2 offset = hash22b(cell) * 2. - 1.;
            //offset = sin( iTime + 6.2831*offset );
            vec2 point = cell + offset * t;
            float dist = distance(p, point);
            if (dist < smallestDist) {
                smallestDist = dist;
                closestPoint = point;
                cellId = cell;
            }
		}
    }
    return vec4(closestPoint, cellId);
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
        vec2  o = hash22b( n + g );
      //vec2  r = g - f + o;
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
            vec2 offset = hash22b(cell) * 2. - 1.;
            vec2 point = cell + offset * .5;
            float r = hash12(cell);
            vec4 cl = cloud(p - point, vec2(.2), mix(.5, 2., r), 5.);
            col.rgb = mix(col.rgb, cl.rgb, cl.w);
            col.a = max(col.a, cl.a);
		}
    }
}

vec3 skyTex(vec2 p, bool dark, float seed)
{   
    vec2 v = voronoi(p + 14.3);

    p = warp(p, vec2(0));
    
    vec2 p2 = p;
    vec2 p3 = p;
    
    swirl(p2, .2, .75, 3., vec2(0.) - seed);
    swirl(p3, .2, .75, 3., vec2(10.) - seed);
    
    p2 /= 6.;
    p2.x /= 3.;

    p3 /= 6.;
    p3.x /= 3.;

    //vec4 col = vec4(.596,.596,.68,0.);
    vec4 col = vec4(.5,.5,.5,0);

    scatterclouds(p2, col);
    scatterclouds(p3, col);

if (dark) {
    float star = smoothstep(.01, -.01, v.x) * .5;
    col += star * pow(1. - col.a, 5.);
    col = min(col, vec4(1));
}
    col.a = 1.;   
    //col.rgb = pow(col.rgb, vec3(1./2.2));

//    col += p.y * .02;
//    col -= .1;
//    col = clamp(col, vec4(.1), vec4(3));

    return col.rgb;
}





void fBlocks(inout vec2 p, vec2 size, float offset, out vec2 c) {
    c = floor((p + size / 2.) / size);
    p.x += mod(c.y, 2.) * size.x * offset;
    c = floor((p + size / 2.) / size);
    p = (fract(p / size + .5) - .5);
    p *= size;
    c *= size;
}


//========================================================
// Modelling
//========================================================

// 0 1 2 3 4
#define ANIM 4

bool isFirstRay;

bool lightingPass;

struct Meta {
    vec3 p;
    vec3 albedo;
    int id;
};

struct Material {
    vec3 albedo;
    float specular;
    float roughness;
};

struct Model {
    float d;
    Meta meta;
};

Model opU(Model a, Model b) {
    if (a.d < b.d) return a;
    return b;
}

float mincol(float a, float b, inout Meta metaa, Meta metab) {
    if (a < b) return a;
    metaa = metab;
    return b;
}

float maxcol(float a, float b, inout Meta metaa, Meta metab) {
    if (a > b) return a;
    metaa = metab;
    return b;
}

void pGr(inout vec2 p, vec2 a, vec2 b) {
    vec2 n = normalize(b - a);
    float g = atan(n.y / n.x);
    pR(p.xy, g);
}

vec3 sunColor = vec3(8.10,6.00,4.20)/10.;

vec3 lightpurple = pow(vec3(0.890,0.851,0.831), vec3(2.2));
vec3 purple = pow(vec3(0.749,0.800,0.812), vec3(2.2));
vec3 darkpurple = pow(vec3(0.573,0.518,0.486), vec3(2.2));
vec3 pink = pow(vec3(0.533,0.302,0.247), vec3(2.2));
vec3 lightyellow = pow(vec3(1.000,0.925,0.722), vec3(2.2));
vec3 lampshadeCol = vec3(1.,.1,.0) * .05;
vec3 bulbCol = vec3(1,.75,.5) * .25;
vec3 woodcol = vec3(0.714,0.43,0.19);
vec3 whitecol = vec3(.5);
vec3 wallcol = vec3(179,179,195)/255./2.;
vec3 darkgrey = vec3(.09,.105,.11)*.8;
vec3 featurewallcol = vec3(.05,.26,.32);


Material woodMat(vec3 p, inout vec3 nor, vec3 col, vec2 size, float offset, float variance, float uneven) {
    float spec = .1;
    float rough = .5;
    vec2 c;
    fBlocks(p.zx, size, offset, c);
    float d = fBox(p.zx, size / 2.);
    float r = hash12((c+20.) * 80.);
    float r2 = hash12((c+1.) * 60.);
    col = mix(col, col * vec3(1.05,.95,1.05), mix(.5, r2, variance));
    col *= mix(.1, 1., smoothstep(.0, -.0003, d));
    col *= mix(.5, 1., mix(.5, r, variance));
    vec3 ax = rndunit(r * 10.);
    nor = erot(nor, ax, (r * 2. - 1.) * .1 * uneven);
    p += length(sin(sin(p * 100. + r * 10.) * 10.)) * .0005;
    p.xz += size * (hash22(c * 20.) * 2. - 1.);
    p.z /= 10.;
    p = erot(p, ax, (r * 2. - 1.) * 2.2);
    p += sin((p + r * 100.) * 200.) * .04;
    d = length(p);
    d = sin(d * 1500.) * .5 + .5;
    //d = step(d, .5);
    col *= mix(.5, 1., d);
    rough = mix(.6, .4, d);
    //rough = 1.;
    //spec = mix(.0, .2, d);
    return Material(col, spec, rough);
}

Material shadeModel(Model model, inout vec3 nor) {
    int id = model.meta.id;
    vec3 p = model.meta.p;

    Material mat;

    vec3 col = model.meta.albedo;
    float spec = 0.;
    float rough = 0.;

    if (id == 101) {
        float logo = texture2D(revisionTex, p.zy).r;
        col = mix(vec3(1.4), vec3(.2,1.4,.8), logo);
    }

    if (id == 23) {
        spec = 1.;
    }

    // floorboards
    if (id == 204) {
        p.y = 0.;
        mat = woodMat(p, nor, woodcol + .1, vec2(.1,.02), .5, 1., 1.);
        return mat;
    }

    // table
    if (id == 141) {
        p.y = 0.;
        mat = woodMat(p * 2.5, nor, woodcol * vec3(.5,.3,.2), vec2(.03,.03), 0., .5, 0.);
        return mat;
    }

    // door, mirror
    if (id == 13 || id == 24) {
        p.x = 0.;
        mat = woodMat(p.zxy * 2. + 20., nor, woodcol * vec3(.5,.3,.2), vec2(1.,.03), 0., .5, 0.);
        return mat;
        //col = fract(p.xyz * 100.);
    }

    // picture
    if (id == 22) {
        p.x = 0.;
        mat = woodMat(p.zxy, nor, woodcol + .1, vec2(.1,.02), .5, 1., 1.);
        return mat;
        //col = fract(p.xyz * 100.);
    }

    // picture image
    if (id == 221) {
        p = p.zyx;
        p *= 20.;
        col = mix(vec3(.596,.596,.68) * .5, vec3(.596,.596,.68) * 1.5, smoothstep(-.5, .5, p.y));
        //col = mix(col, vec3(.5,1,.5), .5);
        p *= 10.;
        vec3 sk = skyTex(p.xy - vec2(-2,9), true, 10.);
        col *= pow(sk, vec3(2.));
        col += pow(sk, vec3(10.));
        spec = .1;
    }

    // bay cap
    if (id == 203) {
        mat = woodMat(p.zxy/2. - 12., nor, mix(woodcol * vec3(.5,.3,.2), woodcol + .1, .3), vec2(2,.03), .0, 1., 1.);
        mat.specular = 0.;
        return mat;
    }


/*
    // sofa
    if (id == 15) {
        vec2 s = vec2(5,.1)*.75;
        float a = texture2D(iChannel0, p.xy * s).r;
        float b = texture2D(iChannel0, p.xy * s.yx).g;
        col = pink * pow(a + b, 2.2);
        //nor = normalize(nor + vec3(a,0,b) * 1.5);
     //   col = pow(max((nor.zyx * vec3(1,1,-1)) * .5 + .5, vec3(0)), vec3(1.8));
    }
*/
    return Material(col, spec, rough);
}

float fLampshade(vec3 p, float h, float r1, float r2, out bool side) {

    /*
    float d = sdUberprim(p.xzy, vec4(r2,r2,h,.00005), vec3(r2,0,r1 - r2), side);
    vec2 pc = vec2(length(p.xz) - mix(r2, r1, step(p.y, 0.)), abs(p.y) - h);
    d = min(d, length(pc) - .0003);
    return d;
    */

    vec2 pc = vec2(length(p.xz) - r1, p.y);
    float d = fBox(pc, vec2(.0, h)) - .0001;
    pc.y = abs(pc.y) - h;
    d = min(d, length(pc) - .0003);
    side = pc.x * step(pc.y, -.0003) < 0.;
    return d;
}

void fSofa(vec3 p, vec3 s, inout float d, inout Meta meta) {
    
    float fade = 0.;
    vec3 uvw = p;
    float d2, d3, d4;
    p.xy += s.xy;
    p.x -= .01;
    p = p.zyx;
    vec3 sofasz = vec3(.2, .1, .1) / 2.;
    vec3 armsz = vec3(.012,.026,sofasz.z);
    float footh = .0075;
    vec3 isofasz = sofasz - vec3(armsz.x * 2., 0, 0);
    p.zy -= sofasz.zy;
    float psx = sign(p.x);
    float vary =  max(psx, 0.);
    vec3 psofa = p;
    // d2 = fBox(p, sofasz);
    // d2 = max(d2, -fBox(p - sofasz * vec3(.5,1.25,0), sofasz * vec3(1,1,.6)));
    // d = mincol(d, d2, meta, Meta(p, pink, 15));

    // arms
    p.x = abs(p.x);
    p.x -= sofasz.x - armsz.x;
    p.y -= footh - sofasz.y + armsz.y;
    d2 = fHalfCapsule(p.xy - vec2(0, armsz.y), armsz.x);
    vec2 armtopp = p.xy - vec2(.004, armsz.y + .005);
    d2 = smin(d2, length(armtopp) - armsz.x - .002, .01);
    d3 = d2 + armsz.x * .4;
    float ar = .007;
    float armang = atan(armtopp.y, armtopp.x);
    float ar0 = sin(sin(sin(armang) * 10. - vary*2.) * 5. + length(armtopp) * 500. + p.z * 100.);
    float ar1 = sin(sin(sin((p.y + 1.) * 65.) * 10.) * 5. + p.x * 300. + p.z * 100.);
    float arw = mix(ar0, ar1, smoothstep(.01, -.01, p.y - armsz.y / 3.));
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
    d2 = smax(d2, -p.y - armsz.y, .001);

    // base
    p = psofa;
    float baseh = .012;
    p.y -= footh - sofasz.y + baseh;
    float br = .002;
    br += sin(sin(sin(p.x * 55. + 2.) * 10.) * 5. + p.y * 100.) * .00025;
    d3 = fBox(p, vec3(isofasz.x + .0068, baseh, isofasz.z) - br) - br;
    d2 = min(d2, d3);
    p.x = abs(p.x);
    p.xy -= vec2(sofasz.x / 2.5, 0.);
    p.y += sin(p.x * 250.) * .0005;
    p.xy *= vec2(.4, 1);
    fade = max(fade, pow(smoothstep(.025, .0, length(p.xy)), 3.)/2.);
    // if (d3 < d2) {
    //     uvw = p;
    //     d2 = d3;
    // }

    // back
    p = psofa;
    vec3 backsz = vec3(isofasz.x + .001, armsz.y + .005, .005);
    p.y -= footh - sofasz.y + baseh + backsz.y;
    p.z -= -sofasz.z + backsz.z;
    d3 = fMailbox(p.zyx, backsz.zyx - .006) - .006;
    d2 = min(d2, d3);

    // cushion
    p = psofa;
    p.x = abs(p.x);
    vec3 cushionsz = vec3(isofasz.x / 2. + .001, .01, sofasz.z - .018);
    p.y -= footh - sofasz.y + baseh * 2. - .001;
    p.y -= cushionsz.y * 2.;
    p.x -= isofasz.x / 2.;
    p.z -= sofasz.z - cushionsz.z * 2. + .005;
    vary += pReflect(p, normalize(vec3(0,-.66,1)), 0.);
    p.y += cushionsz.y;
    p.z -= cushionsz.z - .004;
    //float rr = mix(.007, .005, length(sin(sin((p.xz + max(psx, 0.)) * 50.) * 35.) * .5 + .5));
    float cs = mix(.95, 1.01, length(sin(sin((p + vary * 240.) * 30.) * 3.) * .5 + .5));
    cs = 1. + vsin((p.xz + vary * 2. + 1.) * 100.) * .02;
    float cr = .008;
    float axisx = max(vmin(p.xz), vmin(-p.xz));
    float axisz = min(vmin(p.xz), vmin(-p.xz));
    float crw = sin(sin(sin((axisx + mix(.4, .6, vary)) * (48. + vary)) * 10.) * 5. + p.y * 300. + p.z * 300.);
    crw *= smoothstep(0., .0005, abs(dot(abs(p.xz), cushionsz.zx * vec2(1,-1))));
    crw *= unlerp(.5, 1., vmax(abs(p.xz) / cushionsz.xz));
    cr += crw * .0003;
    //p.y -= smoothstep(cushionsz.x * 1.2, 0., length(p.xz)) * .005;
    cs += smoothstep(cushionsz.x * 1.2, 0., length(p.xz)) * .4;
    vec3 pc = p;
    pc.x += sin(pc.z * 150. + vary * 3.) * .02;
    pR(pc.xz, -vary * .2 + 1.6);
    float buttpatch = vsin((sin(2. + pc.xy * 122. * (1. + abs(vary - 1.) * .2)) + vary * 30.) * 8.);
    buttpatch *= smoothstep(cushionsz.z, 0., length(p.xz));
    cs += buttpatch * .03;
    //p.xz -= normalize(p.xz) * .0075;
    //cs -= .15;
    //p.y += (sin((p.x + ) * 100.) * sin(p.z * 100.)) * .005;
    d3 = (fBox(p / cs, cushionsz - cr - crw * .0001) - cr) * cs* .9;
    float seam = abs(p.y) - cushionsz.y * .75;
    d3 = smax(d3, -abs(seam), .0016);
    //d3 = mix(d3, sdEllipsoid(p, cushionsz), .5);
    if (d3 < d2) {
        d2 = d3;
        fade = 0.;
        fade += smoothbump(.0, .002, seam) * mix(.5, .25, crw);
        fade += smoothbump(.0, .001, -seam) * mix(.5, .25, crw);
        fade = max(fade, smoothstep(cushionsz.x * 1.1, 0., length(p.xz)));
    }

    vec3 col = pow(vec3(0.55,0.29,0.23), vec3(2.2));
    col = mix(col, lampshadeCol * 6., .6);
    col = mix(col, mix(col * 1.75, vec3(1.), .0125), fade);
    //col = vec3(fade);

    d = mincol(d, d2, meta, Meta(uvw, col, 15));
}

Model fRoom(vec3 p, vec3 s, vec3 baysz) {
    #if ANIM != 0
        p.z = -p.z;
    #endif

    float d = 1e12;
    float d2, d3, d4, bound;
    p.x = -p.x;
    vec3 pp = p;    
    vec3 col = purple;
    vec2 spec = vec2(0);
    vec3 p4;
    vec2 pc;
    vec3 uvw;
    Meta meta = Meta(p, vec3(.5), 2);

    //fSofa(p, s, d, meta);
    //return Model(d, meta);
   
    vec3 p2 = pp - vec3(0,0,.05);
    vec3 p3 = pp + vec3(0,0,.11);
   
    vec3 doorsz = (vec3(35, 1981, 762) * .0001) / 2.;
    vec3 doorpos = vec3(s.x, doorsz.y - s.y, -.11);

    bound = -p.x + s.x * .55;
    if (bound > .002) {
        d = min(d, bound);
    } else {
        // tv unit
        p = p2;
        p.x = -p.x;
        p.xy += s.xy;
        p.z = abs(p.z);
        p.x -= .01;
        vec3 tvusz = vec3(.06, .06, .13) / 2.;
        float tvut = .004;
        float tvur = .001;
        p.xy -= tvusz.xy;
        d2 = sdUberprim(p.zyx + vec3(0, tvusz.y, 0), vec4(tvusz.zyx * vec3(1,2,1), tvut), vec3(vec2(tvur), .0));
        float drawt = .002;
        vec3 tvuboxsz = vec3(tvusz.x - drawt, tvusz.y * .5, tvusz.z - tvut * 2.);
        p.y -= tvut - tvusz.y + tvuboxsz.y;
        d2 = min(d2, fBox(p, tvuboxsz - tvur) - tvur);
        vec3 drawsz = vec3(drawt, tvuboxsz.y-tvut, tvusz.z*.5) - vec3(0, 0, tvut);
        d2 = min(d2, fBox(p - vec3(tvuboxsz.z/2.,tvut,drawsz.z), drawsz - tvur) - tvur);
        //d2 = fBox(p, tvusz);
        d = mincol(d, d2, meta, Meta(p, darkgrey, 10));
        
        // tv
        p = p2;
        p.x = -p.x;
        p.xy += s.xy;
        vec3 tvsz = vec3(8.6, 71.1, 122.8) / 1000. / 2.;
        p.xy -= tvsz.xy;
        p.y -= tvusz.y * 2. + .005;
        p.x -= tvusz.x;
        d = mincol(d, fBox(p, tvsz), meta, Meta(p, vec3(.01), 11));
        d2 = fBox(p - vec3(.0041,0,0), tvsz - .004);
        if (d2 < d) {
            meta.p = (p / tvusz.z) + .5;
            meta.id = 101;
            d = d2;
        }

        // shelf
        //p = pp;
        //p.x -= s.x;
        //vec3 shelfsz = vec3(.03,.003,.1) / 2.;
        //p.x += shelfsz.x;
        //p.zy -= vec2(.1,-.02);
        //d2 = fBox(p, shelfsz);
        //d = mincol(d, d2, meta, Meta(p, woodcol, 12));

        // door
        p = pp - doorpos;
        uvw = p;
        d2 = fBox(p, doorsz);
        p = abs(p);
        p.y -= .004;
        vec3 panelsz = doorsz / 2.4 - .005;
        panelsz.x = .001;
        p.z -= panelsz.z + (doorsz.z - panelsz.z * 2.) / 3.;
        p.y -= panelsz.y + (doorsz.y - panelsz.y * 2.) / 3.;
        float dr = .0005;
        d2 = cmax(d2 + dr, -fBox(p.zy, panelsz.zy), dr) - dr;
        p.x -= panelsz.x + .0005;// + doorsz.x - sqrt(dr*dr*2.);
        p.x = -p.x;
        d2 = min(d2, sdUberprim(p.zyx, vec4(panelsz.zyx,10), vec3(0,0,-.005)));
        //d2 = d3;
        d = mincol(d, d2, meta, Meta(uvw, woodcol, 13));  

        // handle
        p = pp - doorpos;
        p.x += .007;
        p.y -= .004;
        p.z -= doorsz.z * .75;
        d2 = length(p) - .004;
        d = mincol(d, d2, meta, Meta(p, vec3(1,.5,.3), 23));  

    }
    
    // table
    p = p2 - vec3(-.01,0,.0);
    pR(p.xz, .1);
    p.y += s.y;
    vec3 tablesz = vec3(.075,.05,.075) / 2.;
    p.y -= tablesz.y;
    bound = fBox(p, tablesz + .001);
    if (bound > .002) {
        d = min(d, bound);
    } else {
        float ttop = .0015;
        d2 = fBox(p - vec3(0,tablesz.y - ttop,0), vec3(tablesz.x, ttop, tablesz.z) + .0002) - .0002;
        d = mincol(d, d2, meta, Meta(p, woodcol, 141));
        p.xz = abs(p.xz);
        float tleg = .0035;
        d2 = sdUberprim(p.xzy - vec3(0,0,tablesz.y-ttop-tleg*1.5), vec4(vec3(tablesz.xz - .003, tleg * 1.5), tleg/3.), vec3(.0002,.0002,0));
        p.xz -= tablesz.xz - tleg - .002;
        d2 = min(d2, fBox(p, vec3(tleg, tablesz.y, tleg) + .0002) - .0002);
        d = mincol(d, d2, meta, Meta(p, whitecol, 14));
    }
    
    // sofa
    p = p2;
    fSofa(p, s, d, meta);

    p = pp;
    p.y -= s.y;
    bound = max(-p.y - .09, length(p.xz) - .05);
    if (bound > .002) {
        d = min(d, bound);
    } else {
        // ceiling rose
        p = pp;
        p.y -= s.y;
        pc = vec2(length(p.xz), -p.y);
        pR(pc, -.2);
        float rx = sin((pc.x + 4.9) * 157.8) + 2.5;
        d2 = (pc.y - sin(rx * 5.2) * .0015 - .01) * .8;
        vec2 pc2 = vec2(rx, -pc.y + .008);
        d2 = smax(d2, pc.x - .042, .0005);
        d2 = max(d2, p.y);
        d = mincol(d, d2,  meta, Meta(p, whitecol, 16));

        // light
        p = pp;
        p.y -= s.y;
        float lightoffset = .065;
        p.y += lightoffset;
        bool side;
        d2 = fLampshade(p, .02, .04, .03, side);
        vec3 mainlightcol = side ? whitecol : lampshadeCol * 20.;
        d = mincol(d, d2, meta, Meta(p, mainlightcol, 17));

        // cable and bulb
        d2 = length(p) - .013;
        p.y = max(max(p.y - lightoffset, -p.y), .0);
        d2 = min(d2, length(p) - .0005);
        d = mincol(d, d2, meta, Meta(p, whitecol, 18));
    }

    // skirting
    p = pp;
    vec2 sksz = vec2(.002, .01) / 2.;
    d2 = fBox(p.xz, s.xz);
    d3 = fBox(p - doorpos + vec3(0,doorsz.y,0), doorsz * vec3(10,2,1) + sksz.y * 2.) + sksz.y * 2.;
    d3 = min(d3, p.y + s.y);
    d4 = fBox(vec2(-d2, d3) - sksz, sksz) - .0005;
    d = mincol(d, d4, meta, Meta(p, whitecol, 19));

    // cornice
    p = pp;
    vec2 corsz = (vec2(.0155, .0121)/2.) * 1.5;
    d2 = fBox(p.xz, s.xz);
    d3 = p.y - s.y;
    pc = vec2(-d2, d3) / 1.5;
    d4 = dot(pc, normalize(vec2(1,-1.3))) - .01;
    d4 = smax(d4, -(length(pc - vec2(.011,-.01)) - .008), .0005);
    //d4 = smin(d4, length((pc - vec2(.003,-.0025)) * vec2(.7,1)) - .005, .002);
    d4 *= 1.5;
    d4 = smax(d4, fBox(vec2(d2, d3) + corsz, corsz), .0002);
    d = mincol(d, d4, meta, Meta(p, whitecol, 20));

    // rail
    p = pp;
    vec2 railsz = vec2(.002,.002) / 2.;
    d2 = fBox(p.xz, s.xz);
    d3 = p.y - .1;
    pc = vec2(-d2 + .001, d3);
    pR(pc, -.1);
    d4 = fBox(pc, railsz) - .001;
    d4 = stairmin(d4, length(pc - railsz * vec2(1.75, 2.)) - .0014, .0014, 2.);
    d4 = max(d4, -(length((pc - railsz * vec2(0,1) - vec2(.0008,.0022)) * vec2(1.,.9)) - .0004));
    d4 = max(d4, -fBox(p - s * vec3(0,0,1), baysz) - .02);
    d = mincol(d, d4, meta, Meta(p, whitecol, 21));
 
    // picture
    p = p2;
    p.z -= .04;
    p.x += s.x;
    vec3 picsz = vec3(.005,.08,.11) / 2.;
    vec3 picimgsz = picsz * .8;
    d2 = fBox(p - picsz * vec3(1,0,0), picsz);
    d3 = fBox(p - picsz * vec3(1.75,0,0), picimgsz);
    if (d2 < d) {
        uvw = p;
        if (fBox(p.zy, picimgsz.zy) < -.0002) {
            meta = Meta(uvw, woodcol, 221);
        } else {
            pc = abs(p.zy) / picsz.zy;
            if (pc.x < pc.y) {
                uvw = p.xzy;
            }
            meta = Meta(uvw, woodcol, 22);
        }
        d2 = max(d2, -d3);
        d = d2;
        
    }

    // mirror
    p = p2;
    p.z += .08;
    p.x += s.x;
    //p.z -= .15;
    d2 = length(p.yz) - .06 / 2.;
    d2 = max(d2, abs(p.x) - .01 / 2.);
    d3 = length(p.yz) - .04 / 2.;
    d2 = max(d2, -d3);
    d = mincol(d, d2, meta, Meta(p, woodcol, 24));
    d3 = length(p - vec3(-.022,0,0)) - .033;
    d3 = max(d3, -p.x);
    d = mincol(d, d3, meta, Meta(p, vec3(1,.5,.3), 23));


    // lamp
    p = p3;
    p.x += .3 / 2.;

    // lampshade
    bool side;
    d2 = fLampshade(p, .02, .025, .025, side);
    if (d2 < d) {
        d = d2;
        col = side ? bulbCol * .5 : lampshadeCol;
        col *= isFirstRay ? 80. : 1.;
        meta = Meta(p, col, 7);
    } 

    // lamp bulb
    d2 = length(p) - .01;
    if (d2 < d) {
        d = d2;
        col = bulbCol;
        col *= isFirstRay ? 80. : 1.;// * vec3(8.10,6.00,4.20);
        meta = Meta(p, col, 8);
    }

    // lamp stand
    d2 = length(p.xz) - .007 / 2.;
    d2 = max(d2, p.y);   
    p.y += s.y;
    d3 = length(p.xz) - .05 / 2.;
    d3 = max(d3, p.y - .01 / 2.);
    d2 = min(d2, d3);
    p = p3;
    d2 = max(d2, -p.y - s.y);
    d = mincol(d, d2, meta, Meta(p, woodcol, 25));
    
    return Model(d, meta);
}

float fBricks(vec2 p, out vec2 c, out vec2 uv, out float hide) {
    vec2 size = vec2(.087,.045);
    c = floor((p + size / 2.) / size);
    p.x += mod(c.y, 2.) * size.x / 2.;
    c = floor((p + size / 2.) / size);
    p = (fract(p / size + .5) - .5);
    uv = p;
    p *= size;
    float r = rnd(ivec2(c)) * 2. - 1.;
    pR(p, .1 * r);
    float gap = .001;
    float d = fBox(p, size / 2. - gap);
    hide = -fBox(p, size / 2. + gap * 4.);
    c *= size;
    return d;
}

float fTile(vec3 p, vec3 s) {
    p.z = -p.z;
    //return fBox(p, s);
    float overlap = s.x * .2;
    float d = 1e12;
    float r = s.x / 4.;
    float bx = fBox(p + vec3(r,0,0), s);
    float cut = dot(p.xz - vec2(s.x - r, 0), normalize(vec2(1,2)));
    p.x = abs(p.x - s.x / 4.) - s.x / 2.;
    float cir = length(p.zx) - r;
    d = min(d, cir);
    d = max(d, p.z - s.z);
    d = min(d, bx);
    d = max(d, -cir - s.z * 2.);
    d = max(d, abs(p.y) - s.y);
    d = max(d, cut);
    return d;
}

float fTiles(vec3 p, vec3 area, vec2 limit, out vec3 col) {
    col = pink;
    //return fBox(p, area);
    p += area;
    vec2 size = area.xy / (limit / 2. + .5);
    p.xy -= size;
    vec2 co = floor((p.xy + size / 2.) / size);

    float d = 1e12;

    for (float x = 0.; x < 2.; x++)
    for (float y = 0.; y < 2.; y++)
    {
        vec2 c = co + vec2(x, y);
        c = clamp(c, vec2(0), limit);
        vec3 pc = p - vec3((c - .5) * size, 0);
        pR(pc.yz, .05);
        d = min(d, fTile(pc, vec3(size + vec2(0,.005), area.z) / 2.));
    }

    col = pink;

    return d;
}

vec2 face(vec2 p) {
     vec2 a = abs(p);
     return step(a.yx, a.xy)*step(a.xy, a.xy)*sign(p);
}

vec3 lampPos = vec3(.3/2., 0, -.11) * 3.25;

bool inside;
bool lastinside;
bool initialsample;
bool startedinside;
int passedwindow1;
int passedwindow2;

Model scene(vec3 p) {

    //float ld = length(p - lampPos) - .1;

    #if ANIM != 0
        p.z = -p.z;
    #endif
    
    //p.z = -p.z;

    float d = 1e12;
    float d2;

    float wall = .05;
    vec3 roomsz = vec3(.2,.2,.2);
    vec3 mainsz = roomsz + wall;
    vec3 baysz = vec3(mainsz.xy * vec2(.7, .7), .08) * vec3(1,.8,1);
    Meta meta;

    d = 1e12;

    float sc = 3.2;
    p /= sc;

/*
    float hi = fTile(p, vec3(.1,.15,.005)) * sc;
    hi = min(hi, fTile(p - vec3(.2,0,0), vec3(.1,.15,.005)) * sc);
    meta = Meta(p, pink, 202);
    return Model(hi, meta);
*/


    #ifdef ROOM_ONLY
        Model rm = fRoom(p, roomsz, baysz);
        rm.d *= sc;
        return rm;
    #endif
    
    //p.x = -p.x;

    p.z = -p.z;
    vec3 pp = p;
    meta = Meta(p, vec3(.5), 1);

    
    // core
    vec2 peak = vec2(-.1, .08);
    p.y -= peak.y;
    d = min(d, fBox(p, mainsz + vec3(0, peak.y, 0)));

    p = pp;
    vec3 roomFace;
    float roomBound = fBox(p, roomsz, roomFace);

    if ( roomBound > 0.) {

        // bricks
        p = pp;
        float bf = fBox(p, mainsz + vec3(0, 3., 0));
        vec2 c, uv;
        float hide;
        vec2 fc = face(p.xz);
        p = abs(p.x) < abs(p.z) - (mainsz.z - mainsz.x) ? p : p.zyx;
        float bricks = fBricks(p.xy, c, uv, hide);
        float rnd3 = rnd(ivec2(c * 100. + 185. + fc * 7.)) * 2. - 1.;
        float hh = (c.y + mainsz.y * 1.15) + (-abs(c.x) + mainsz.x / 2.) * .75;
        hh = mix(hh, rnd3, .3);
        if (hh < .0) {
            bricks = lightingPass ? 1e12 : hide;
        }
        float rnd1 = rnd(ivec2(c * 100. + 50. + fc * 10.));
        bf = max(bf, -bf - .02);
        bf = bf - rnd1 * .0025 - .0025;
        float rnd2 = rnd(ivec2(c * 100. + 30. + fc * 10.)) * PI * 6.;
        bf -= dot(uv, vec2(sin(rnd2), cos(rnd2))) * .01 * rnd1;
        float bd = cmax(bf, bricks, .003);
        d = mincol(d, bd, meta, Meta(p, lightpurple, 201));
        //d = bd;
        //return Model(d, col, id);

        float tilecap = fBox(p.xz, mainsz.xz + .008);

        // roof
        p = pp;
        p.y -= peak.y;
        p.y -= mainsz.y + peak.y;
        p.z -= peak.x;

        //float tilestop = length(p.zy - vec2(0., -.005)) - .02;
        float tilestop = sdCappedCylinder((p - vec3(0., -.005, 0)).yxz, .0175, mainsz.x * 1.07 - .0025) - .0025;

        vec3 ps = sign(p);
        p.z = abs(p.z);
        pGr(p.zy, vec2(0), vec2(-(mainsz.z - peak.x) / 2., peak.y));
        tilestop = max(tilestop, -p.y - .003);

        d = max(d, p.y + .005);
        tilecap = max(max(tilecap, p.y + .005), -p.y - .05);

        d = mincol(d, tilecap, meta, Meta(p, whitecol, 2011));

        //float tiles = fBricks(p.xz, c, uv, hide);
        vec3 tilecol;
        vec3 tilearea = vec3(mainsz.x * 1.13, .203, .005);
        vec2 tilelimit = vec2(4);
        if (ps.z < 0.) {
            tilearea.y = .093;
            tilelimit.y = 1.;
        }
        p.z -= tilearea.y;
        float tiles = fTiles(p.xzy - vec3(.017,0,0), tilearea, tilelimit, tilecol);
        tilestop = max(tilestop, -tiles+.0015);
        p.x = abs(p.x) - mainsz.x * 1.07;
        tiles = max(tiles, p.x);
        tiles = min(tiles, tilestop);
        //tiles = min(tiles, fBox(p + vec3(0,.005,0), vec3(.01, .0075, tilearea.y + .003)));


        d = mincol(d, tiles, meta, Meta(p, tilecol, 202));


    }
    
    // main
    p = pp;
    //p.z += baysz.z / 2.;
    float main = fBox(p, mainsz);

    // window
    p = pp;
    p.z += roomsz.z;
    d = max(d, -fBox(p - vec3(0,.0,0), vec3(.2,.175,.3)/2.));
    p.x = abs(p.x);
    p.z -= roomsz.z * 2.;
    d = maxcol(d, -fBox(p, baysz), meta, Meta(p, wallcol, 207));
    //d = max(d, -fBox(p - vec3(0,0,0), vec3(.08,.12,.3)));
    //d = max(d, -fBox(p - vec3(.12,0,0), vec3(.035,.12,.3)));

    // bay
    float bayinner = baysz.x * .45;
    float framethick = .01;
    float capheightTop = .016;
    float capheight = p.y > 0. ? capheightTop : .008;

    p = pp;
    p.z -= mainsz.z;
    p.x = abs(p.x);

    float bayshape = p.z - baysz.z;
	//bayshape = fBoxHalf(p.zy, baysz.zy);
    //bayshape = fBox(p, baysz);
    bayshape = max(bayshape, dot(p.xz - vec2(baysz.x, 0), normalize(vec2(baysz.z, baysz.x - bayinner))));
//    float bayshapeInner = abs(p.y) - baysz.y + capheight + .001;//max(abs(p.y) + baysz.y/2., bayshape + framethick);

  //  int bayid = bayshapeInner > 0. ? 203 : 2033;

    // bay caps
    float baycaps = smax(abs(abs(p.y) - baysz.y) - capheight, bayshape - .01, .0);
    baycaps = max(baycaps, -p.z - wall + .001);
    baycaps = min(baycaps, fBox(p + vec3(0,baysz.y - capheight / 2.,.045), vec3(baysz.x, capheight / 2., .01)));
    //if (p.y > 0.) caps = caps = max(-p.z - wall);
    //d = min(d, bayshape);


    //d2 = max(bayshape - .012, -fCorner(-p.zy + vec2(.01, baysz.y + capheightTop + .005)));
    //d2 = max(d2, fBox(p - vec3(0,baysz.y + capheightTop + .01,0), vec3(baysz.x + .02,.02,1)));
    //d = mincol(d, d2, meta, Meta(p, vec3(.05), 209));



    float frame = max(max(bayshape, -bayshape - .02), fBox(p, baysz));
    p.xz -= vec2(bayinner, baysz.z);
    vec3 fn = normalize(mix(vec3(1,0,0), normalize(vec3(baysz.x - bayinner, 0, -baysz.z)), .5));
    float bs = pReflect(p, -fn, 0.);
    vec2 framesz = vec2(bayinner - framethick, baysz.y - framethick * 2.);
    if (bs < 0.) framesz.x = length(vec2(baysz.x - bayinner, baysz.z)) / 2. - framethick * 1.25;
    p.x += framethick + framesz.x;
    float fr = .01;
    vec2 pc = vec2(fBox(p.xy, framesz + fr) + fr, frame);
    pc.x += .008;
    pc.y = -pc.y;
    float fd = fCornerBevel(-pc + vec2(0,.0033), vec2(.005, .003));
    fd = min(fd, max(fCornerBevel(-pc + vec2(.01,.0), vec2(.002, .002)), pc.y - .0027));
    frame = max(frame, fd);

    //int frameid = pc.x < 0.0099 ? 2033 : 203;
    int frameid = 2033;

    d = mincol(d, frame, meta, Meta(p, whitecol, frameid));
    
    
    

    // p = pp;
    // p.z -= mainsz.z - baysz.z;
    // float bay = fBox(p, baysz * vec3(1,1,2));
    // p.z -= baysz.z;
    // p.x = abs(p.x);
    // p.x -= baysz.x;
    // pR(p.xz, -.7);
    // bay = max(bay, p.z);
    // //main = min(main, bay);
    // //d = min(d, main);
    // d = mincol(d, bay, meta, Meta(p, pink, 203));
    // d = bay;
    

    pp.z *= -1.;
    p = pp;

    // Subtract room

    d2 = -main - wall;
    inside = d2 > 0.;

    if (initialsample) {
        lastinside = inside;
        startedinside = inside;
    } else {
        if (lastinside != inside) {
            lastinside = inside;
            if (p.z < 0.) {
                passedwindow1 += 1;
            } else {
                passedwindow2 += 1;
            }
        }
    }
    
    if (d2 > d) {
        meta = Meta(p, wallcol, 207);
        if (roomFace == vec3(0,-1,0)) {
            meta = Meta(p - vec3(0, mainsz.y, 0), woodcol, 204);
        }
        if (roomFace == vec3(-1,0,0)) meta = Meta(p, featurewallcol, 205);
        if (p.y > 0.1) meta = Meta(p, whitecol, 206);
        d = d2;
    }

    d = mincol(d, baycaps, meta, Meta(p.yxz, whitecol, 203));

    Model m = Model(d, meta);
    
    // Add room
    #ifndef HIDE_ROOM
    if (fBox(p, roomsz) < 0.) {
        m = opU(m, fRoom(p, roomsz, baysz));
    }
    #endif
    
    m.d *= sc;

    return m;
}



float gWarp;
float gSpin;
float gSpinBg;
float gAxblend;
float gDaxblend;

void warpspin(float time, bool isDark, inout vec3 p, inout vec3 dir) {
    float tf = fract(time);
    float tf1 = unlerp(0., .5, tf);
    float tf2 = unlerp(.5, 1., tf);
    #if 0
        warp = gain2(tf, 3., .5);
        float spin = mix(fract(time), gain(unlerp(.025, .825, fract(time)), 1.75), 1.) * 2.;
        axblend = sinbump(.15, 1., tf);
        vec3 ax = normalize(vec3(-1., axblend * 2., 0));
        p = erot(p, ax, -spin * PI * 2.);
    #elif 0
        warp = gain2(tf, 3., .5);
        float spin = mix(fract(time), gain(unlerp(.025, .825, fract(time)), 1.75), 1.) * 2.;
        vec3 ax = normalize(vec3(-1.,-sinbump(0., 1., tf) * -2.,-.0));
        p = erot(p, ax, spin * PI * -2.);
        _spin = spin;
    #elif 1
        gWarp = gain2(tf1, 2.3, 1.) + gain2(tf2, 2.3, 1.);
        gSpin = mix(fract(time), gain(unlerp(.0, 1., fract(time)), 1.75), .7);
        gAxblend = smoothstep(.0, .66, tf) - smoothstep(.66, 1., tf);
        vec3 ax = normalize(mix(vec3(-1,0,0), vec3(-.5,2,0), gAxblend));
        p = erot(p, ax, 4. * gSpin * PI * -2.);

        gSpinBg = gSpin;

        vec3 dax = vec3(-1,0,0);

        if (isDark) {
            if (tf < .5) {
                gSpinBg -= .128;
            } else {
                gSpinBg -= .7;
                dax = normalize(vec3(-1,1,0));
            }
        } else {
            if (tf > .7) {
                gSpinBg -= 1.;
            } else if (tf > .3) {
                gSpinBg -= .5;
                dax = normalize(vec3(-.5,1,0));
            } 
        }

        gSpinBg /= 2.;

        dir = erot(dir, dax, gSpinBg * PI * -2.);
    #elif 0
        warp = gain2(tf1, 2., .8) + gain2(tf2, 2., .8);
        float spin = mix(fract(time), gain(unlerp(.0, 1., fract(time)), 2.), .3);
        float spin2 = gain(tf, 8., 1.);
        spin = mix(spin, spin2, .2);
        axblend = smoothstep(.0, .66, tf) - smoothstep(.66, 1., tf);
        vec3 ax = normalize(mix(vec3(-1,0,0), vec3(-.5,2,0), axblend));
        //vec3 ax = normalize(mix(vec3(-1,0,0), vec3(-1,2,-1.), axblend));
        p = erot(p, ax, 3. * spin * PI * -2.);
        _spin = spin;
    #elif 0
        warp = gain2(tf1, 2., .8) + gain2(tf2, 2., .8);
        float spin = gain(tf, 2., .6);
        float spin2 = gain(tf, 10., 1.);
        spin = mix(spin, spin2, .05);
        axblend = smoothstep(.0, .66, tf) - smoothstep(.66, 1., tf);
        vec3 ax = normalize(mix(vec3(-1,0,0), vec3(-.5,2,0), axblend));
        //vec3 ax = normalize(mix(vec3(-1,0,0), vec3(-1,2,-1.), axblend));
        p = erot(p, ax, 4. * spin * PI * -2.);
        _spin = spin;
    #elif 0
        //warp = gain2(tf, 2.5, .75);
        warp = mix(tf1, gain2B(tf1, 3., .0), .9);
        warp += mix(tf2, gain2B(tf2, 3., .0), .9);
        //float spin = mix(fract(time), gain(unlerp(.025, 1. - .05, fract(time)), 1.85), 1.) * 2.;
        float spin = gain(tf, 2., .5) * 4.;
        //float axblend = smoothstep(.0, .5, tf) - smoothstep(.5, 1., tf);
        axblend = sinbump(.0, 1., tf);
        //vec3 ax = normalize(mix(vec3(-1,.25,0), normalize(vec3(-.0,1,0)), axblend));
        //ax = normalize(mix(ax, vec3(1,0,0), smoothstep(.55, 1., tf)));



        //p = erot(p, ax, spin * PI * -2.);

        // p = erot(p, vec3(0,0,1), smoothstep(.0, 1., tf) * PI * -2.);
        // p = erot(p, vec3(1,0,0), smoothstep(0., .5, tf) * PI * 2. * .5);
        // p = erot(p, vec3(1,0,0), smoothstep(.5, 1., tf) * PI * 2. * .5);
        // p = erot(p, normalize(vec3(-.0,1,0)), smoothstep(.0, 1., tf) * PI * 2.);

        //p = erot(p, vec3(0,1,0), gain(tf, 2., 1.) * -2. * PI * 2.);
        //p = erot(p, normalize(vec3(1,1,0)), gain(unlerp(.1, 1., tf), 2., 1.) * PI * 2.);
        //p = erot(p, vec3(1,0,0), gain(unlerp(0., .3, tf), 2., 1.) * PI * 2.);

        tf = fract(time + .1);

        //spin = (gain(tf1, 1.5, .75) + gain(tf2, 1.5, .75)) / 2.;
        spin = gain(unlerp(.0, 1., tf), 2., .5);
        //axblend = gain(unlerp(.1, .4, tf), 2., 1.);// - gain(unlerp(.6, .9, tf), 2., 1.);
        //axblend = gain(unlerp(.1, .8, tf), 2., 1.) - gain(unlerp(.9, 1., tf), 1., 1.);

        axblend = gain(unlerp(.4, .6, tf), 2., 1.) - gain(unlerp(.8, 1., tf), 2., 1.);
        axblend = 1.;
        vec3 ax = mix(vec3(0,1,0), normalize(vec3(-.25,-1,-.5)), axblend);
        
        //p = erot(p, vec3(0,1,0), smoothstep(.15, 1., tf1) * PI * 2.);
        p = erot(p, normalize(ax), spin * 3. * PI * 2.);

        p = erot(p, vec3(1,0,0), gain(unlerp(0., .5, tf), 2., 1.) * 1. * PI * 2.);


    #endif
}

vec3 warpedP;
vec3 camPos;

Model sceneWarped(vec3 p) {

    float dbg = vmin(abs(p.yz));
    dbg = 1e12;

    float k;
    vec4 p4 = inverseStereographic(p, k);
    
    vec3 _, _2;
    warpspin(time, false, _, _2);
    
    pR(p4.zw, -gWarp * PI * 2.);
    p = stereographic(p4);
    warpedP = p;

    Model model = scene(p);
    float fix = length(p.xyz) * k;
    model.d = min(model.d, model.d / fix);

    if (model.d > .7) model.d = .7 - 1. / model.d / 5.;
    
    if (dbg < model.d) {
        model.meta.albedo = vec3(fract(model.d * 100.), max(0., sign(model.d)), 0);
        model.d = dbg;
    }
       
    return model;
}


bool firstMarch;

Model map(vec3 p) {
    return scene(p);
}

Model mapWarped(vec3 p) {
    vec3 _;
    float camSphere = length(p - camPos) - 6.;
    warpspin(time, false, p, _);
    Model model = sceneWarped(p);
    //model.d = max(model.d, -camSphere);
    return model;
}








//========================================================
// Rendering
//========================================================

vec3 calcNormal(vec3 p )
{
    const float h = 0.00001;      // replace by an appropriate value
    vec3 n = vec3(0.0);
    for( int i=0; i<4; i++ )
    {
        vec3 v = vec3(
            int(mod(float(i + 3), 4.)) / 2, // 1 0 0 1
            i / 2, // 0 0 1 1
            int(mod(float(i), 2.)) // 0 1 0 1
        );
        vec3 e = 0.5773 * (2. * v- 1.);
        n += e * map(p + e * h).d;
    }
    return normalize(n);
}


vec3 sunPos = normalize(vec3(-5,5,7)) * 100.;
vec3 skyColor = vec3(0.50,0.70,1.00);

vec3 env(vec3 dir, bool dark) {
#if 1

//dark = true;

    vec3 col = mix(vec3(.5,.7,1) * .05, vec3(.5,.7,1) * 1., smoothstep(-.5, .5, dir.y));
    col = mix(col, vec3(.596,.596,.68), .1);

    vec2 pc = vec2(atan(dir.z, dir.x), dir.y) * 30.;

    float seed = time - .2;

    if (dark) {
        float blend = dir.y;
        if (time > .5) {
            blend += .2;
            pc -= vec2(20);
        } else {
            pc += vec2( 51, -36.8);
        }
        col = mix(vec3(0.002,0.008,.02) * 3., vec3(0.002,0.008,.02) * .2, smoothstep(-.4, .0, blend));
        col = mix(vec3(0.03,0.07,.07), col, smoothstep(-.5, -.2, blend));
    } else {
        if (time > .2 && time < .7) {
            pc += vec2(-17);
        } else {
            seed = fract(timeRaw + .1) - .1;
            pc += vec2(119.3, 8.7);
        }
    }

    // vec2(-10.8, 8.7)
    // 51 -36.8
    // -82.3, 54.2
    //vec2( -83.3, 53.)
    vec3 cl = skyTex(pc, dark, seed * 10.);
    col *= cl;
    if (dark) {
        col += pow(cl, vec3(15.)) * .3;
    } else {
        col += pow(cl, vec3(15.)) * 2.;
    }
    //col += .1;

//    col += dir.y;
    
  //  col = pow(col, vec3(2.));


    return col;
#else 
    vec3 col = vec3(0);
    //col += max(dir.y, 0.) * skyColor;
    col = mix(vec3(.5,.7,1) * .1, vec3(.5,.7,1) * .5, clamp(unlerp(-.4, .1, dir.y), 0., 1.));
    #if 1
        //col += smoothstep(.5, 1., dot(dir, normalize(sunPos))) * sunColor;// * 5.;
    #else
        //col += smoothstep(.97, .99, dot(dir, normalize(sunPos))) * sunColor * 20.;
    #endif
    return col;
#endif
}


struct Hit {
    Model firstModel;
    Model model;
    vec3 pos;
    vec3 dir;
    float len;
    bool sky;
};

Hit marchFirst(vec3 origin, inout vec3 rayDirection, float maxDist) {

    vec3 rayPosition;
    float rayLength = 0.;
    Model model;
    Model candidateModel;
    float candidateError = 1e12;
    vec3 candidateRayPosition = vec3(0);
    vec3 candidateRayDirection;
    float dist = 0.;
    bool bg = false;
    vec3 lastWarpedP;
    Model firstModel;

    for (int i = 0; i < 800; i++) {
        rayLength += dist * .75;
        rayPosition = origin + rayDirection * rayLength;
        lastWarpedP = warpedP;
        model = mapWarped(rayPosition);
        if (i == 0) {
            firstModel = model;
            initialsample = false;
        }
        dist = model.d;

        float error = dist / rayLength;

        if (abs(dist) < .0001) {
            candidateModel = model;
        	break;
        }
        
        if (error < candidateError) {
            candidateModel = model;
            candidateError = error;
            candidateRayPosition = warpedP;
            candidateRayDirection = normalize(warpedP - lastWarpedP);
        }
        
        
        if (rayLength > maxDist) {
            bg = true;
            candidateModel.meta.id = 0;
            break;
        }
    }
    
    model = candidateModel;
    
    rayPosition = candidateRayPosition;
    rayDirection = candidateRayDirection;

    return Hit(firstModel, model, rayPosition, rayDirection, rayLength, bg);
}

Hit march(vec3 origin, vec3 rayDirection, float maxDist) {

    vec3 rayPosition;
    float rayLength = 0.;
    Model model;
    Model candidateModel;
    float candidateError = 1e12;
    float dist = 0.;
    bool bg = false;
    Model firstModel;

    for (int i = 0; i < 200; i++) {
        rayLength += dist * .75;
        rayPosition = origin + rayDirection * rayLength;
        model = scene(rayPosition);
        dist = model.d;

        float error = dist / rayLength;

        if (abs(dist) < .0001) {
            candidateModel = model;
        	break;
        }
        
        if (error < candidateError) {
            candidateModel = model;
            candidateError = error;
        }
        
        if (rayLength > maxDist) {
            bg = true;
            candidateModel.meta.id = 0;
            break;
        }
    }
    
    model = candidateModel;

    return Hit(firstModel, model, rayPosition, rayDirection, rayLength, bg);
}

// tracing/lighting setup from yx
// https://www.shadertoy.com/view/ts2cWm
vec3 ortho(vec3 a){
    vec3 b=cross(vec3(-1,-1,.5),a);
    // assume b is nonzero
    return (b);
}

vec3 RandomUnitVector(vec2 state)
{
    float z = hash12(state) * 2. - 1.;
    float a = hash12(state) * PI * 2.;
    float r = sqrt(1. - z * z);
    float x = r * cos(a);
    float y = r * sin(a);
    return vec3(x, y, z);
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

/*
vec3 randomSphereDirection(vec2 seed) {
    vec2 h = hash22(seed) * vec2(2.,6.28318530718)-vec2(1,0);
    float phi = h.y;
	return vec3(sqrt(1.-h.x*h.x)*vec2(sin(phi),cos(phi)),h.x);
}

vec3 randomHemisphereDirection(vec3 n, vec2 seed ) {
	vec3 dr = randomSphereDirection(seed);
	return dot(dr,n) * dr;
}

vec3 sampleLight2(Hit hit, vec3 nor, vec2 seed, vec3 light, vec3 lightCol, int id, float radius) {
    vec3 offset = randomSphereDirection( seed ) * radius;
    light += offset;
    vec3 lightDir = (light - hit.pos);
    float diffuse = dot(nor, normalize(lightDir)) / length(lightDir);
    if (diffuse > 0.) {
        vec3 shadowOrigin = hit.pos + nor * .01;
        Hit sh = march(shadowOrigin, normalize(lightDir), 5.);
        if (sh.model.id == id) {
            return lightCol * diffuse;
        }
    }
    return vec3(0);
}
*/
vec3 sampleLight(Hit hit, vec3 nor, vec2 seed, vec3 light, vec3 lightCol, int id, float radius) {
    // shoot randomly perturbed ray towards light,
    // if it doesn't hit geo, add to result
    vec3 lightDir = (light - hit.pos);
    vec3 lightSampleDir = getConeSample(lightDir, radius, seed);
    float diffuse = dot(nor, lightSampleDir);
    vec3 shadowOrigin = hit.pos + nor * .0002;
    if (diffuse > 0.) {
        Hit sh = march(shadowOrigin, lightSampleDir, 5.);
        if (sh.model.meta.id == id) {
            return lightCol * diffuse;
        }
    }
    return vec3(0);
}

vec3 sampleLight2(Hit hit, vec3 nor, vec2 seed, vec3 light, int id, float radius) {
    // shoot randomly perturbed ray towards light,
    // if it doesn't hit geo, add to result
    vec3 lightDir = (light - hit.pos);
    vec3 lightSampleDir = getConeSample(lightDir, radius, seed);
    float diffuse = dot(nor, lightSampleDir) / length(lightDir);
    vec3 shadowOrigin = hit.pos + nor * .0002;
    if (diffuse > 0.) {
        Hit sh = march(shadowOrigin, lightSampleDir, 5.);
        if (sh.model.meta.id == id) {
            Material material = shadeModel(sh.model, nor);
            return material.albedo * diffuse;
        }
    }
    return vec3(0);
}



mat3 basisMatrix(vec3 forward, vec3 up) {
    vec3 ww = normalize(forward);
    vec3 uu = normalize(cross(up,ww));
    vec3 vv = normalize(cross(ww,uu));
    return mat3(-uu, vv, ww);
}

float graph(float y, float t) {
     float d = y - t;
     d /= fwidth(d);
     d = abs(d) - .1;
     d = 1. - clamp(d, 0., 1.);
     return d;
}

vec3 debugWarpspin(vec2 uv) {
    vec3 _, _2;
    warpspin(uv.x, false, _, _2);
    vec3 col = vec3(1,0,0) * graph(uv.y, gWarp/2.);
    col += vec3(0,1,0) * graph(uv.y, gSpin);
    col += vec3(0,0,1) * graph(uv.y, gAxblend);
    col += vec3(1) * graph(uv.x, fract(time));
    col += vec3(1) * graph(uv.y, gSpinBg);
    
    //col *= .3;
    return col;
}


// main path tracing loop, based on yx's
// https://www.shadertoy.com/view/ts2cWm
void mainImage(out vec4 fragColor, in vec2 fragCoord) {
    
 
    vec2 uv = fragCoord.xy / iResolution.xy;
    vec4 sampl = vec4(0);
    
    vec2 p = (-iResolution.xy + 2.* fragCoord) / iResolution.y;

    bool db = p.x > 0.;

    time = iTime / 8.;

    vec2 seed = hash22(fragCoord + (float(iFrame) + time * 30.) * 1.61803398875);
    
    // jitter for antialiasing
    p += 2. * (seed - .5) / iResolution.xy;

    #ifndef FREE_FLY
        // jitter for motion blur
        time += (hash12(seed) * 2. - 1.) * .0005;
    #endif

    timeRaw = time;
    time = fract(time);

    vec3 col = vec3(0);

    vec3 origin = eye;
    vec3 rayDir = normalize(vec3(p.x * fov, p.y * fov, -1.) * mat3(vView));

    #ifndef FREE_FLY
        float focalLength = 6.;
        camPos = vec3(0, 0, focalLength * 1.9);// * (1. + sinbump(0., 1., time) * .45);
        //camPos = vec3(0, 0, focalLength * mix(1.8, 2.2, sinbump(0., 1., fract(time))));
        vec3 camTar = vec3(0);
        vec2 im = .5 - vec2(.45,.42);
        //im = iMouse.xy / iResolution.xy; im = .5 - im; im.x *= 2.;
        pR(camPos.yz, im.y * PI / 2.);
        pR(camPos.xz, im.x * PI * 2.);   
        camTar = mix(camTar, camPos, .25);
        mat3 camMat = basisMatrix(camTar - camPos, vec3(0,1,0));
        rayDir = normalize(camMat * vec3(p.xy, focalLength));
        origin = camPos;
    #endif


    Hit hit;
    vec3 nor, ref;
    Material material;

    vec3 accum = vec3(1);
    vec3 bgCol = skyColor;
    
    Hit firstHit;

    const int MAX_BOUNCE = 8;

    passedwindow1 = 0;
    passedwindow2 = 0;
    initialsample = true;
    inside = false;
    isFirstRay = true;
    bool firstBounce = true;
    vec3 rd = rayDir;
    hit = marchFirst(origin, rayDir, 30.);
    firstHit = hit;

    for (int bounce = 0; bounce < MAX_BOUNCE; bounce++) {
   
        if (hit.sky) {
            if (isFirstRay) {
                break;
            }
            col += env(rayDir, false) * accum;
            break;
        }

        firstBounce = false;

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
            // disable emissive for 2nd bounce
            isFirstRay = false;
            
            // calculate direct lighting
            vec3 directLight = vec3(0);
            seed = hash22(seed);
            directLight += sampleLight(hit, nor, seed, sunPos, sunColor, 0, .005);
            directLight += sampleLight(hit, nor, seed, lampPos, bulbCol * 2., 8, .001);
            directLight += sampleLight2(hit, nor, seed, lampPos, 7, .01);

            // mix direct light
            col += accum * directLight;

            rayDir = diffuseRayDir;
        } else {
            // Calculate specular ray direction
            vec3 specularRayDir = reflect(rayDir, nor);
            rayDir = normalize(mix(specularRayDir, diffuseRayDir, material.roughness * material.roughness));
        }

        origin = hit.pos + nor * .0002;    
        seed = hash22(seed);
        hit = march(origin, rayDir, 5.);
    }

    if (hit.sky && isFirstRay) {
        // col = vec3(0);
        // col.r = float(passedwindow1) / 2.;
        // col.g = float(passedwindow2) / 2.;
        // col.b = startedinside ? 1. : 0.;
        bool isLight = ! firstBounce || (
            false
            || (time < .3 && ! startedinside && passedwindow1 < 2)
            || (time < .3 && startedinside && passedwindow2 == 1 && passedwindow1 == 0)
            || (time > .4 && time < .5 && startedinside && passedwindow1 == 1)
            || ( ! startedinside && passedwindow1 == 0 && passedwindow2 == 0)
            || ( ! startedinside && passedwindow1 == 1 && passedwindow2 == 1)
            || (time < .15 && startedinside && passedwindow1 == 1 && passedwindow2 == 1)
        );
        vec3 _;
        rayDir = firstBounce ? rd : rayDir;
        warpspin(time, ! isLight, _, rayDir);
        col = env(rayDir, ! isLight);
    }

    //vec3 cold = debugWarpspin(fragCoord.xy/iResolution.xy);
    //col = col + cold;

    //col = clamp(col, vec3(0), vec3(1));
      
    if (drawIndex > 0.) {
        vec3 lastCol = texture2D(previousSample, fragCoord.xy / iResolution.xy).rgb;
        col = mix(lastCol, col, 1. / (drawIndex + 1.));
    }

    fragColor = vec4(col, 1);
}

