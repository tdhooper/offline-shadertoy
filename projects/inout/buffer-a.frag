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

uniform sampler2D iChannel0; // /images/blue-noise.png filter: linear
uniform sampler2D lichenTex; // /images/lichen.png filter: linear wrap: repeat
uniform sampler2D revisionTex; // /images/revision-logo.png filter: linear
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

// Rotate on axis, blackle
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
vec2 hash22(vec2 p)
{
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

float rnd(ivec2 uv) {
    return texture2D(iChannel0, vec2(uv) / iChannel0Size.xy, -10000.).r;
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
            vec2 offset = hash22(cell) * 2. - 1.;
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
        vec2  o = hash22( n + g );
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
            vec2 offset = hash22(cell) * 2. - 1.;
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





//========================================================
// Modelling
//========================================================

// 0 1 2 3 4
#define ANIM 4

bool isFirstRay;

bool lightingPass;

struct Model {
    float d;
    vec3 col;
    int id;
};

Model opU(Model a, Model b) {
    if (a.d < b.d) return a;
    return b;
}

void mincol(inout float a, inout vec3 cola, float b, vec3 colb) {
    if (a < b) return;
    cola = colb;
    a = b;
}

float mincol(float a, float b, inout vec3 cola, vec3 colb) {
    if (a < b) return a;
    cola = colb;
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
vec3 woodcol = vec3(0.714,0.451,0.184);
vec3 whitecol = vec3(.5);
vec3 wallcol = vec3(179,179,195)/255./2.;
vec3 darkgrey = vec3(.09,.105,.11)*.8;
vec3 featurewallcol = vec3(.05,.26,.32);

Model fRoom(vec3 p, vec3 s, vec3 baysz) {
    #if ANIM != 0
        p.z = -p.z;
    #endif

    int id = 2;

    float d = 1e12;
    float d2, d3, d4, bound;
    p.x = -p.x;
    vec3 pp = p;    
    vec3 col = purple;
    vec3 p4;
    vec2 pc;
   
    vec3 p2 = pp - vec3(0,0,.1);
    vec3 p3 = pp + vec3(0,0,.11);
   
    vec3 doorsz = (vec3(35, 1981, 762) * .0001) / 2.;
    vec3 doorpos = vec3(s.x, doorsz.y - s.y, -.11);

    bound = -p.x + s.x * .55;
    if (bound > .0004) {
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
        d = mincol(d, d2, col, darkgrey);
        
        // tv
        p = p2;
        p.x = -p.x;
        p.xy += s.xy;
        vec3 tvsz = vec3(8.6, 71.1, 122.8) / 1000. / 2.;
        p.xy -= tvsz.xy;
        p.y -= tvusz.y * 2. + .005;
        p.x -= tvusz.x;
        d = mincol(d, fBox(p, tvsz), col, vec3(.01));
        d2 = fBox(p - vec3(.0041,0,0), tvsz - .004);
        if (d2 < d) {
            float logo = texture2D(revisionTex, (p.zy / tvusz.z / 1.) + .5).r;
            d = d2;
            col = mix(vec3(1.4), vec3(.2,1.4,.8), logo);
        }

        // shelf
        p = pp;
        p.x -= s.x;
        vec3 shelfsz = vec3(.03,.003,.1) / 2.;
        p.x += shelfsz.x;
        p.zy -= vec2(.1,-.02);
        d2 = fBox(p, shelfsz);
        d = mincol(d, d2, col, woodcol);

        // door
        p = pp - doorpos;
        d = mincol(d, fBox(p, doorsz), col, woodcol);  
    }
    
    // table
    p = pp - vec3(-.01,0,.1);
    pR(p.xz, .1);
    p.y += s.y;
    vec3 tablesz = vec3(.066,.05,.1) / 2.;
    p.y -= tablesz.y;
    bound = fBox(p, tablesz + .001);
    if (bound > .0004) {
        d = min(d, bound);
    } else {
        float ttop = .0015;
        d2 = fBox(p - vec3(0,tablesz.y - ttop,0), vec3(tablesz.x, ttop, tablesz.z) + .0002) - .0002;
        d = mincol(d, d2, col, woodcol);
        p.xz = abs(p.xz);
        float tleg = .0035;
        d2 = sdUberprim(p.xzy - vec3(0,0,tablesz.y-ttop-tleg*1.5), vec4(vec3(tablesz.xz - .003, tleg * 1.5), tleg/3.), vec3(.0002,.0002,0));
        p.xz -= tablesz.xz - tleg - .002;
        d2 = min(d2, fBox(p, vec3(tleg, tablesz.y, tleg) + .0002) - .0002);
        d = mincol(d, d2, col, whitecol);
    }
    
    // sofa
    p = p2;
    p.xy += s.xy;    
    vec3 sofasz = vec3(.08, .075, .2) / 2.;
    p.xy -= sofasz.xy;
    mincol(d, col, fBox(p, sofasz), pink);
    d = max(d, -fBox(p - sofasz * vec3(.5,1.25,0), sofasz * vec3(1,1,.6)));

    p = pp;
    p.y -= s.y;
    bound = max(-p.y - .09, length(p.xz) - .05);
    if (bound > .0004) {
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
        d = mincol(d, d2, col, whitecol);

        // light
        p = pp;
        p.y -= s.y;
        float lightoffset = .065;
        float lightheight = .02;
        p.y += lightoffset;
        pc = vec2(length(p.xz) - .04, p.y);
        d2 = fBox(pc, vec2(.0, lightheight)) - .0001;
        pc.y = abs(pc.y) - lightheight;
        d2 = min(d2, length(pc) - .0003);
        vec3 mainlightcol = pc.x * step(pc.y, -.0003) < 0. ? whitecol : lampshadeCol * 20.;
        d = mincol(d, d2, col, mainlightcol);

        // cable and bulb
        d2 = length(p) - .013;
        p.y = max(max(p.y - lightoffset, -p.y), .0);
        d2 = min(d2, length(p) - .0005);
        d = mincol(d, d2, col, whitecol);
    }

    // lamp
    p = p3;
   
    p.x += .3 / 2.;
    d2 = length(p.xz) - .007 / 2.;
    //p.y -= .01 / 2.;
    d2 = max(d2, p.y);
    
    d = min(d, d2);

    //p.y += .02 / 2.;
    p.y += s.y;
    d2 = length(p.xz) - .05 / 2.;
    d2 = max(d2, p.y - .01 / 2.);
    d = min(d, d2);

    p = p3;
    d = max(d, -p.y - s.y);

    // skirting
    p = pp;
    vec2 sksz = vec2(.002, .01) / 2.;
    d2 = fBox(p.xz, s.xz);
    d3 = fBox(p - doorpos + vec3(0,doorsz.y,0), doorsz * vec3(10,2,1) + sksz.y * 2.) + sksz.y * 2.;
    d3 = min(d3, p.y + s.y);
    d4 = fBox(vec2(-d2, d3) - sksz, sksz) - .0005;
    d = mincol(d, d4, col, whitecol);

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
    d = mincol(d, d4, col, whitecol);

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
    d = mincol(d, d4, col, whitecol);
 
    // picture
    p = p2;
    p.x += s.x;
    vec3 picsz = vec3(.005,.07,.09) / 2.;
    d2 = fBox(p - picsz * vec3(1,0,0), picsz);
    d2 = max(d2, -fBox(p - picsz * vec3(1.75,0,0), picsz * .8));
    d = mincol(d, d2, col, woodcol);

    // mirror
    p = pp;
    p.x += s.x;
    //p.z -= .15;
    d2 = length(p.yz) - .06 / 2.;
    d2 = max(d2, abs(p.x) - .01 / 2.);
    d3 = length(p.yz) - .04 / 2.;
    d2 = max(d2, -d3);    
    d = mincol(d, d2, col, woodcol);
    d3 = length(p - vec3(-.022,0,0)) - .033;
    d3 = max(d3, -p.x);
    if (d3 < d) {
        d = d3;
        col = vec3(1,.5,.3);
        id = 5;
    }

    p = p3;
    p.x += .3 / 2.;

    // lampshade
    d2 = length(p.xz) - .05 / 2. + p.y * .05;
    bool lampshadeOuter = d2 > 0.;
    d2 = abs(d2) - .0002;
    d2 = smax(d2, abs(p.y) - .04 / 2., 0.);
    if (d2 < d) {
        d = d2;
        col = lampshadeOuter ? lampshadeCol : bulbCol * .5;
        col *= isFirstRay ? 80. : 1.;
        id = 7;
    } 

    // lamp bulb
    d2 = length(p) - .01;
    if (d2 < d) {
        d = d2;
        col = bulbCol;
        col *= isFirstRay ? 80. : 1.;// * vec3(8.10,6.00,4.20);
        id = 8;
    }

    return Model(d, col, id);
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
    int id = 1;

    float wall = .05;
    vec3 roomsz = vec3(.2,.2,.2);
    vec3 mainsz = roomsz + wall;
    vec3 baysz = vec3(mainsz.xy * vec2(.7, .7), .08) * vec3(1,.8,1);    

    d = 1e12;

    float sc = 3.2;
    p /= sc;

    #ifdef ROOM_ONLY
        Model rm = fRoom(p, roomsz, baysz);
        rm.d *= sc;
        return rm;
    #endif
    
    //p.x = -p.x;

    p.z = -p.z;
    vec3 pp = p;
    vec3 col = darkpurple;
    
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
        //col = lightpurple;
        mincol(d, col, bd, lightpurple);
        //d = bd;
        //return Model(d, col, id);

        // roof
        p = pp;
        float tilebound = fBox(p, mainsz + vec3(0, 1, 0));
        p.y -= peak.y;
        p.y -= mainsz.y + peak.y;
        p.z -= peak.x;
        p.z = abs(p.z);
        pGr(p.zy, vec2(0), vec2(-(mainsz.z - peak.x) / 2., peak.y));
        float tiles = fBricks(p.xz, c, uv, hide);
        tiles = max(tiles, max(p.y-.01, -(p.y + .01)));
        tiles = max(tiles, tilebound);
        //return Model(tiles, col, id);

        if (p.y > d) {
            d = p.y;
            col = pink;
        }
        d = min(d, tiles);
    }
    
    // main
    p = pp;
    //p.z += baysz.z / 2.;
    float main = fBox(p, mainsz);
    p = pp;
    p.z -= mainsz.z - baysz.z;
    float bay = fBox(p, baysz * vec3(1,1,2));
    p.z -= baysz.z;
    p.x = abs(p.x);
    p.x -= baysz.x;
    pR(p.xz, -.7);
    bay = max(bay, p.z);
    //main = min(main, bay);
    //d = min(d, main);
    mincol(d, col, bay, pink);
    
    // window
    p = pp;
    p.z += roomsz.z;
    d = max(d, -fBox(p - vec3(0,.0,0), vec3(.2,.175,.3)/2.));
    p.x = abs(p.x);
    p.z -= roomsz.z * 2.;
    d = max(d, -fBox(p - vec3(0,0,0), vec3(.08,.12,.3)));
    d = max(d, -fBox(p - vec3(.12,0,0), vec3(.035,.12,.3)));
    
    
    pp.z *= -1.;
    //pp.x *= -1.;
    // roomA
    p = pp;

    d2 = -main - wall;
    d2 = max(d2, -bay - .02);

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
    
    //d2 = max(d2, p.y + .15);
    if (d2 > d) {
        col = wallcol;
        if (roomFace == vec3(0,-1,0)) col = woodcol;
        if (roomFace == vec3(-1,0,0)) col = featurewallcol;
        // if (roomFace == vec3(0,1,0)) {
        //     if (d2 < .008) {
        //         //d2 -= sin(p.x * 1000.) * sin(p.z * 1000.) * .0005;
        //         float tx = texture2D(lichenTex, p.xz * 20.).r * .002;
        //         d2 -= tx;
        //         d2 *= .7;
        //     }
        // }
        if (p.y > 0.1) col = whitecol;
        d = d2;
    }

    Model m = Model(d, col, id);
    
    if (fBox(p, roomsz) < 0.) {
        m = opU(m, fRoom(p, roomsz, baysz));
    }
    
    m.d *= sc;

    //if (inside) m.id = 99;

   // m.d = min(m.d, ld);

    return m;
}

float unlerp(float vmin, float vmax, float value) {
    return clamp((value - vmin) / (vmax - vmin), 0., 1.);
}

float sinbump(float a, float b, float x) {
    x = unlerp(a, b, x);
    return sin(x * PI * 2. - PI / 2.) * .5 + .5;
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

    float dbg = abs(p.y);
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
        model.col = vec3(fract(model.d * 10.), max(0., sign(model.d)), 0);
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

        if (abs(dist) < .00001 * rayLength) {
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
            candidateModel.id = 0;
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

        if (abs(dist) < .00001 * rayLength) {
            candidateModel = model;
        	break;
        }
        
        if (error < candidateError) {
            candidateModel = model;
            candidateError = error;
        }
        
        if (rayLength > maxDist) {
            bg = true;
            candidateModel.id = 0;
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
    vec3 shadowOrigin = hit.pos + nor * .01;
    if (diffuse > 0.) {
        Hit sh = march(shadowOrigin, lightSampleDir, 5.);
        if (sh.model.id == id) {
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
    vec3 shadowOrigin = hit.pos + nor * .01;
    if (diffuse > 0.) {
        Hit sh = march(shadowOrigin, lightSampleDir, 5.);
        if (sh.model.id == id) {
            return sh.model.col * diffuse;
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

    bool db = p.y > 0.;

    time = iTime / 8.;

    vec2 seed = hash22(fragCoord + (float(iFrame) + time * 30.) * 1.61803398875);
    
    // jitter for antialiasing
    p += 2. * (seed - .5) / iResolution.xy;

    #ifndef FREE_FLY
        // jitter for motion blur
        time += (hash12(seed) * 2. - 1.) * .001;
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

    vec3 accum = vec3(1);
    vec3 bgCol = skyColor;
    
    Hit firstHit;

    const int MAX_BOUNCE = 8;

    passedwindow1 = 0;
    passedwindow2 = 0;
    initialsample = true;
    inside = false;
    isFirstRay = true;
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

       	accum *= hit.model.col;
        nor = calcNormal(hit.pos);

        if (hit.model.id == 5) {
            origin = hit.pos + nor * .0002;
            rayDir = reflect(rayDir, nor); 
        } else {
            isFirstRay = false;
            col += accum * sampleLight(hit, nor, seed, sunPos, sunColor, 0, .005);
            col += accum * sampleLight(hit, nor, seed, lampPos, bulbCol * 2., 8, .001);
            col += accum * sampleLight2(hit, nor, seed, lampPos, 7, .01);
            rayDir = getSampleBiased(nor, 1., seed);
        }

        // set new origin and direction for dffuse bounce
        origin = hit.pos + nor * .0002;    
        seed = hash22(seed);
        hit = march(origin, rayDir, 5.);        
    }

    if (hit.sky && isFirstRay) {
        // col = vec3(0);
        // col.r = float(passedwindow1) / 2.;
        // col.g = float(passedwindow2) / 2.;
        // col.b = startedinside ? 1. : 0.;
        bool isLight = (
            false
            || (time < .3 && ! startedinside && passedwindow1 < 2)
            || (time < .3 && startedinside && passedwindow2 == 1 && passedwindow1 == 0)
            || (time > .4 && time < .5 && startedinside && passedwindow1 == 1)
            || ( ! startedinside && passedwindow1 == 0 && passedwindow2 == 0)
            || ( ! startedinside && passedwindow1 == 1 && passedwindow2 == 1)
            || (time < .15 && startedinside && passedwindow1 == 1 && passedwindow2 == 1)
        );
        vec3 _;
        warpspin(time, ! isLight, _, rd);
        col = env(rd, ! isLight);
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

