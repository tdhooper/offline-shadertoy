// framebuffer drawcount: 1

precision highp float;

uniform vec2 iResolution;
uniform mat4 cameraMatrix;
uniform sampler2D previousSample; // buffer-a.frag filter: linear
uniform float drawIndex;
uniform int iFrame;
uniform float iTime;

uniform sampler2D iChannel0; // /images/blue-noise.png
uniform vec2 iChannel0Size;

varying vec3 eye;
varying vec3 dir;
varying float fov;
varying float aspect;
varying mat4 vView;


void mainImage(out vec4 a, in vec2 b);

void main() {
    mainImage(gl_FragColor, gl_FragCoord.xy);
}

#define PI 3.1415926




//========================================================
// Modelling
//========================================================

// 0 1 2 3 4
#define ANIM 4

#define PHI 1.618033988749895

#define saturate(x) clamp(x, 0., 1.)

// HG_SDF
void pR(inout vec2 p, float a) {
    p = cos(a)*p + sin(a)*vec2(p.y, -p.x);
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
// Rotate on axis, blackle
vec3 erot(vec3 p, vec3 ax, float ro) {
  return mix(dot(ax,p)*ax, p, cos(ro))+sin(ro)*cross(ax,p);
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


// mla https://www.shadertoy.com/view/lsGyzm
vec4 inverseStereographic(vec3 p, out float k) {
  k = 2.0/(1.0+dot(p,p));
  return vec4(k*p,k-1.0);
}
vec3 stereographic(vec4 p4) {
  float k = 1.0/(1.0+p4.w);
  return k*p4.xyz;
}


float time;

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

void pGr(inout vec2 p, vec2 a, vec2 b) {
    vec2 n = normalize(b - a);
    float g = atan(n.y / n.x);
    pR(p.xy, g);
}

vec3 lightpurple = pow(vec3(0.890,0.851,0.831), vec3(2.2));
vec3 purple = pow(vec3(0.749,0.800,0.812), vec3(2.2));
vec3 darkpurple = pow(vec3(0.573,0.518,0.486), vec3(2.2));
vec3 pink = pow(vec3(0.533,0.302,0.247), vec3(2.2));
vec3 lightyellow = pow(vec3(1.000,0.925,0.722), vec3(2.2));

Model fRoom(vec3 p, vec3 s) {
    #if ANIM != 0
        p.z = -p.z;
    #endif

    float d = 1e12;
    float d2;
    p.x = -p.x;
    vec3 pp = p;    
    vec3 col = purple;
   
    vec3 p2 = pp - vec3(0,0,.1);
    vec3 p3 = pp + vec3(0,0,.11);
   
    // tv unit
    p = p2;
    p.x = -p.x;
    p.xy += s.xy;
    vec3 tvusz = vec3(.06, .04, .15) / 2.;
    p.xy -= tvusz.xy;
    d = min(d, fBox(p, tvusz));
    
    // tv
    p = p2;
    p.x = -p.x;
    p.xy += s.xy;
    vec3 tvsz = vec3(.01, .07, .1) / 2.;
    p.xy -= tvsz.xy;
    p.y -= .08 / 2.;
    p.x -= .05 / 2.;
    d = min(d, fBox(p, tvsz));
    
    // table
    p = p2;
    p.y += s.y;
    vec3 tablesz = vec3(.08,.05,.08) / 2.;
    float ttop = .01 / 2.;
    p.y -= tablesz.y * 2. - ttop;
    d = min(d, fBox(p, vec3(tablesz.x, ttop, tablesz.z)));
    p = p2;
    p.y += s.y;
    p.xz = abs(p.xz);
    float tleg = .01 / 2.;
    p -= tablesz - vec3(tleg, 0, tleg);
    d = min(d, fBox(p, vec3(tleg, tablesz.y, tleg)));
    
    // sofa
    p = p2;
    p.xy += s.xy;    
    vec3 sofasz = vec3(.08, .075, .2) / 2.;
    p.xy -= sofasz.xy;
    mincol(d, col, fBox(p, sofasz), pink);
    d = max(d, -fBox(p - sofasz * vec3(.5,1.25,0), sofasz * vec3(1,1,.6)));
    
    // picture
    p = p2;
    p.x += s.x;
    vec3 picsz = vec3(.005,.07,.09) / 2.;
    d = min(d, fBox(p - picsz * vec3(1,0,0), picsz));
    d = max(d, -fBox(p - picsz * vec3(1.75,0,0), picsz * .8));
    
    // picture2
    p = pp;
    p.x += s.x;
    //p.z -= .15;
    d2 = length(p.yz) - .06 / 2.;
    d2 = max(d2, abs(p.x) - .01 / 2.);
    float d3 = length(p.yz) - .04 / 2.;
    d3 = max(d3, abs(p.x - .01 / 2.) - .005 / 2.);
    d2 = max(d2, -d3);    
    d = min(d, d2);
        
    if ( ! lightingPass) {
        // light
        p = pp;
        p.y -= s.y;
        float lightsz = .04 / 2.;
        p.y += lightsz + .04 / 2.;
        d2 = length(p) - lightsz;
        d = min(d, d2);
    }


    // lamp
    p = p3;
   
    p.x += .3 / 2.;
    d2 = length(p.xz) - .007 / 2.;
    p.y -= .02 / 2.;
    d2 = max(d2, p.y);
    
    if ( ! lightingPass) {
        d = min(d, d2);
 
        d2 = length(p.xz) - .03 / 2. + p.y * .2;
        d2 = smax(d2, abs(p.y) - .03 / 2., 0.);
        d = min(d, d2);
    }
    
    p.y += .02 / 2.;
    p.y += s.y;
    d2 = length(p.xz) - .05 / 2.;
    d2 = max(d2, p.y - .01 / 2.);
    d = min(d, d2);

    p = p3;
    d = max(d, -p.y - s.y);
    
    // bookshelf
    p = p3;
    p.x -= s.x;
    p.y += s.y;
    vec3 bs = vec3(.02,.2,.1) / 2.;
    d = min(d, fBox(p - vec3(-bs.x, bs.y, 0), bs));
    
    return Model(d, col, 2);
}

float rnd(ivec2 uv) {
    return texture2D(iChannel0, vec2(uv) / iChannel0Size.xy).r;
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

Model scene(vec3 p) {
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
    vec3 baysz = vec3(mainsz.xy * vec2(.7, .7), .08);    

    d = 1e12;

    float sc = 3.25;
    p /= sc;
    
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
    float bay = fBox(p, baysz * vec3(1,.8,2));
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
    bool inside = d2 > 0.;
    
    //d2 = max(d2, p.y + .15);
    if (d2 > d) {
        d = d2;
        col = vec3(.8);
        if (roomFace == vec3(0,-1,0)) col = vec3(0.714,0.451,0.184);
        if (roomFace == vec3(-1,0,0)) col = vec3(0.000,0.278,0.302);
    }

    Model m = Model(d, col, id);
    
    if (fBox(p, roomsz) < 0.) {
        m = opU(m, fRoom(p, roomsz));
    }
    
    m.d *= sc;

    if (inside) m.id = 99;

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


float _spin;

void warpspin(float time, out float warp, inout vec3 p) {
    float tf = fract(time);
    warp = gain2(tf, 3., .5);
    float spin = mix(fract(time), gain(unlerp(.025, .825, fract(time)), 1.75), 1.) * 2.;
    vec3 ax = normalize(vec3(-1.,-sinbump(0., 1., tf) * -2.,-.0));
    p = erot(p, ax, spin * PI * -2.);
    _spin = spin;
}

vec3 warpedP;

Model sceneWarped(vec3 p) {

    float dbg = abs(p.y);
    dbg = 1e12;

    float k;
    vec4 p4 = inverseStereographic(p, k);
    
    float warp;
    vec3 _ = vec3(0);
    warpspin(time, warp, _);
    
    pR(p4.zw, -warp * PI * 2.);
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
    float _;
    warpspin(time, _, p);
    return sceneWarped(p);
}








//========================================================
// Rendering
//========================================================


// https://www.shadertoy.com/view/4djSRW
vec2 hash22(vec2 p)
{
	vec3 p3 = fract(vec3(p.xyx) * vec3(.1031, .1030, .0973));
    p3 += dot(p3, p3.yzx+33.33);
    return fract((p3.xx+p3.yz)*p3.zy);
}

vec3 calcNormal(vec3 p) {
  vec3 eps = vec3(.00001,0,0);
  vec3 n = vec3(
    map(p + eps.xyy).d - map(p - eps.xyy).d,
    map(p + eps.yxy).d - map(p - eps.yxy).d,
    map(p + eps.yyx).d - map(p - eps.yyx).d
  );
  return normalize(n);
}


vec3 sunPos = normalize(vec3(-5,5,7)) * 100.;
vec3 sunColor = vec3(8.10,6.00,4.20)/5.;
vec3 skyColor = vec3(0.50,0.70,1.00);

vec3 env(vec3 dir) {
    vec3 col = vec3(0);
    col += max(dir.y, 0.) * skyColor;
    #if 1
        col += smoothstep(.5, 1., dot(dir, normalize(sunPos))) * sunColor;// * 5.;
    #else
        col += smoothstep(.97, .99, dot(dir, normalize(sunPos))) * sunColor * 20.;
    #endif
    return col;
}


struct Hit {
    Model firstModel;
    Model model;
    vec3 pos;
    vec3 dir;
    float len;
    bool sky;
};

Hit marchFirst(vec3 origin, vec3 rayDirection, float maxDist) {

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
        if (i == 0) firstModel = model;
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
        rayLength += dist;// * .5;
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

// main path tracing loop, based on yx's
// https://www.shadertoy.com/view/ts2cWm
void mainImage(out vec4 fragColor, in vec2 fragCoord) {

    time = fract(iTime / 3.);
 
    vec2 uv = fragCoord.xy / iResolution.xy;
    vec4 sampl = vec4(0);
    
    vec2 p = (-iResolution.xy + 2.* fragCoord) / iResolution.y;

    vec2 seed = hash22(fragCoord + float(iFrame) * 1.61803398875);
    
    // jitter for antialiasing
    #ifndef PREVIEW
        p += 2. * (seed - .5) / iResolution.xy;
    #endif
    
    vec3 origin = eye;
    vec3 rayDir = normalize(vec3(p.x * fov, p.y * fov, -1.) * mat3(vView));

    Hit hit;
    vec3 col = vec3(0);
    vec3 nor, ref;

    vec3 accum = vec3(1);
    vec3 bgCol = skyColor;
    
    Hit firstHit;

    const int MAX_BOUNCE = 4;

    hit = marchFirst(origin, rayDir, 14.);
    firstHit = hit;

    for (int bounce = 0; bounce < MAX_BOUNCE; bounce++) {
   
        if (hit.sky) {
            if (bounce == 0) {
                //col = env(rayDir);
                col = bgCol;
                break;
            }
            col += env(rayDir) * accum;
            break;
        }
        
       	accum *= hit.model.col;
        nor = calcNormal(hit.pos);

        // shoot randomly perturbed ray towards sun,
        // if it doesn't hit geo, add to result
        vec3 sunDirection = sunPos - hit.pos;
        vec3 sunSampleDir = getConeSample(sunDirection, .005, seed);
        float sunLight = dot(nor, sunSampleDir);
        vec3 shadowOrigin = hit.pos + nor * .01;
        bool hitSun = sunLight > 0. && march(shadowOrigin, sunSampleDir, 5.).sky;
        
        if (hitSun) {
            col += accum * sunColor * sunLight;
        }

        // set new origin and direction for dffuse bounce
        origin = hit.pos + nor * .002;
        rayDir = getSampleBiased(nor, 1., seed);

        seed = hash22(seed);
        
        hit = march(origin, rayDir, 5.);        
    }
  
    if (drawIndex > 0.) {
        vec3 lastCol = texture2D(previousSample, fragCoord.xy / iResolution.xy).rgb;
        col = mix(lastCol, col, 1. / (drawIndex + 1.));
    }

    fragColor = vec4(col, 1);
}

