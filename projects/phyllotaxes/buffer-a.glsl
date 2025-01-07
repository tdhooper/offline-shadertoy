#version 300 es

precision highp float;

uniform vec2 iResolution;
uniform vec2 iOffset;
uniform float iGlobalTime;
uniform vec4 iMouse;
uniform sampler2D iChannel0; // buffer-a.glsl filter: linear wrap: clamp
uniform float drawIndex;
uniform int iFrame;

in vec3 eye;
in vec3 dir;
in float fov;
in float aspect;
in mat4 vView;

out vec4 fragColor;

void mainImage(out vec4 a, in vec2 b);

void main() {
    mainImage(fragColor, gl_FragCoord.xy + iOffset.xy);
}

#ifdef GL_ES
precision mediump float;
#endif

// HG_SDF

#define PI 3.1415926

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


// hash function by Blackle
// https://suricrasia.online/demoscene/functions/
//#define FK(k) floatBitsToInt(cos(k))^floatBitsToInt(k)
#define FK(k) floatBitsToInt(k*k/7.)^floatBitsToInt(k)
float hash(vec2 p) {
  int x = FK(p.x); int y = FK(p.y);
  return (float((x-y*y)*(x*x+y)-x)/2.14e9) * .5 + .5;
}

vec2 hash22(vec2 p){
    return vec2(hash(p), hash(p + vec2(1)));
}

vec2 hash12(float n) {
	return hash22(vec2(n));
}

float hash31(vec3 p) {
  return hash(p.xy + p.z * 1.6453);
}

vec3 hash33(vec3 p){
    return vec3(
        hash31(p),
        hash31(p + vec3(0.3183099, 0.3678794, 0.5376431)),
        hash31(p + vec3(0.167348, 0.665734, 0.84331))
    );
}

// noise function by IQ
// https://www.shadertoy.com/view/4sfGzS
float noise31(const in vec3 x ) {
    vec3 p = floor(x);
    vec3 f = fract(x);
    f = f*f*(3.0-2.0*f);
	
    return mix(mix(mix( hash31(p+vec3(0,0,0)), 
                        hash31(p+vec3(1,0,0)),f.x),
                   mix( hash31(p+vec3(0,1,0)), 
                        hash31(p+vec3(1,1,0)),f.x),f.y),
               mix(mix( hash31(p+vec3(0,0,1)), 
                        hash31(p+vec3(1,0,1)),f.x),
                   mix( hash31(p+vec3(0,1,1)), 
                        hash31(p+vec3(1,1,1)),f.x),f.y),f.z);
}

float fbm(const in vec3 p, const in int octaves) {
    float accum = 0.;
    vec3 temp_p = p;
    float weight = 1.;
    for (int i=0; i<octaves; i++) {
        accum += weight * noise31(temp_p);
        weight *= .5;
        temp_p *= 2.;
    }
    return abs(accum);
}

float voronoi3( in vec3 x )
{
    vec3 cell = floor(x);
    float d = 1e12;
    for( int k=-1; k<=1; k++ )
    for( int j=-1; j<=1; j++ )
    for( int i=-1; i<=1; i++ )
    {
        vec3 offset = vec3(float(i),float(j),float(k));
        vec3 pos = hash33( cell + offset );
        vec3 r = cell + offset + pos;
        d = min(d, length(x - r));
    }
    return d;
}

//========================================================
// Modelling
//========================================================

struct Model {
    float d;
    int id;  
    vec3 pos;
    vec2 uv;
    vec2 cell;
    float cellt;
    float wedges;
    float slice;
    float len;
    float edge;
    float point;
};

Model newModel() {
    return Model(1e12, 0, vec3(0), vec2(0), vec2(0), 0., 0., 0., 0., 0., 0.);
}

Model opU(Model a, Model b) {
    Model m = a;
    if (b.d < a.d) {
        m = b;
    }
    return m;
}

float hitEps = .001;

struct BloomSpec {
    float stretch;
    vec2 minmax;
    float size;
    float boundSize;
    float width;
    float thickness;
    float pointy;
    float cutback;
    vec3 color;
};

float rangec(float vmin, float vmax, float value) {
  return clamp((value - vmin) / (vmax - vmin), 0., 1.);
}

// Adapted from https://www.shadertoy.com/view/WdScDG
Model leaf(vec3 p, vec3 cellData, BloomSpec spec) {
    
    float scale = spec.size * .78;
    p /= scale;
    
    float thickness = spec.thickness;
    float width = spec.width;
    float pointy = spec.pointy;

    vec2 cell = cellData.xy;
    float cellTime = cellData.z * 2.;
    
    float d = 1e12;
    float d2 = 1e12;
    float slice = 1e12;
    float wedge, wedges;

    vec3 pp = p;

    float core = length(p) - .1;

    float len = cellTime;

    len *= mix(1., mix(.2, 1., rangec(.8, .6, cellTime/2.)), spec.cutback);
    
  	len = pow(len, .33);

    float llen = len;

    Model model = newModel();

    float top = p.y - len * .5;
    float curve = smoothstep(0., .6, cellTime);
    float lenCurve = len * mix(1.5, .65, curve);
    pR(p.zy, -mix(.2, .7, curve));
    slice = length(p - vec3(0,lenCurve,0)) - lenCurve;

    p = pp;
    float point = (p.z / len - .8) * 5. * (.5 - slice / thickness);

    // wedge
    float ins = .25;
    p.z += ins;
    vec3 n = normalize(vec3(1,0,.35));
    wedge = -dot(p, n);
    wedge = smax(wedge, dot(p, n * vec3(1,1,-1)), clamp((slice / thickness / 2.) + .25, 0., 1.) * .1);
    wedge = smax(wedge, p.z - len*1.12 - ins, len);
    p.z -= ins;

    // wedge2
    ins = .2;
    p.z += ins;
    n = normalize(vec3(1,0,width));
    float wedge2 = -dot(p, n);
    wedge2 = smax(wedge2, dot(p, n * vec3(1,1,-1)), .1);
    wedge2 = smax(wedge2, p.z - len*.95 - ins, len*.6);
    p.z -= ins;

    float r = len / 8.;

    d2 = abs(slice) - thickness;
    d2 = max(d2, top);

    float wedgeT = smax(d2, wedge, thickness);
    float wedgeT2 = smax(d2, wedge2, thickness);
    d = mix(wedgeT2, smin(wedgeT, wedgeT2, .01), pointy);
    wedges = mix(wedge2, wedge2, pointy);

    p = pp;
    vec2 uv = p.xz / len;

    model.pos = p;
    model.d = d * scale;
    model.uv = uv;
    model.cell = cell;
    model.cellt = cellData.z;
    model.wedges = mix(wedge2, smin(wedge, wedge2, .01), pointy);
    model.slice = slice;
    model.len = len;
    model.edge = distance(model.wedges*1.5, (slice + thickness / 2.));
    model.point = point;

    return model;
}

mat2 phyllotaxis;
void calcPhyllotaxis() {
    vec2 cc = vec2(5., 8.);
    float aa = atan(cc.x / cc.y);
    float scale = (PI*2.) / sqrt(cc.x*cc.x + cc.y*cc.y);
    mat2 mRot = mat2(cos(aa), -sin(aa), sin(aa), cos(aa));
    mat2 mScale = mat2(1./scale,0,0,1./scale);
    phyllotaxis = mRot * mScale;
}

vec3 calcCellData(
    vec2 cell,
    vec2 offset,
    mat2 worldToGrid,
    mat2 gridToWorld,
    BloomSpec spec
) {
    // Snap to cell center and move to neighbour
    cell = gridToWorld * (round(worldToGrid * cell) + offset);

    // Clamp first and last cell
    float o = .5 / spec.stretch;
    cell.y = clamp(cell.y, spec.minmax.x + o, spec.minmax.y - o);
    cell = gridToWorld * round(worldToGrid * cell);

    // Calc cell time
    float t = 1. - (cell.y - spec.minmax.x) / (spec.minmax.y - spec.minmax.x);

    return vec3(cell, t);
}

Model mBloom(
    vec3 p,
    int id,
    BloomSpec spec
) {
    Model model = newModel();
    float bound = length(p) - spec.boundSize;

    if (bound > hitEps * 2.) {
        model.d = bound;
        return model;
    }
    vec3 pp = p;
    vec2 cell = vec2(
        atan(p.x, p.z),
        atan(p.y, length(p.xz))
    );
    spec.minmax = spec.minmax * PI / 2.;
    mat2 worldToGrid = phyllotaxis * mat2(1,0,0,spec.stretch);
    mat2 gridToWorld = inverse(worldToGrid);    
    for( int m=-1; m<=1; m++ )
    for( int n=-1; n<=1; n++ )
    {
        vec3 cellData = calcCellData(cell, vec2(m,n), worldToGrid, gridToWorld, spec);
        p = pp;
        pR(p.xz, -cellData.x);
        pR(p.zy, cellData.y);
        model = opU(model, leaf(p, cellData, spec));
    }
    model.id = id;
    return model;
}

Model map(vec3 p) {

    Model model = newModel();

    float bound = p.y - .7;
    if (bound > hitEps * 2.) {
        model.d = bound;
        return model;
    }
    
    vec3 pp = p;
       
    pR(p.xz, .49);
    gmTransform(p);
    model = opU(model, mBloom(p, 1, BloomSpec(2.5, vec2(-.25, 1.), 1., .85, .37, .045, 1., .6, vec3(.5))));
    
   	p = pp;
    p -= vec3(-1.1,.06,0.15);
    pR(p.xz, 8.1);
    gmTransform(p);
    float f = dot(p, normalize(vec3(1.2,0,1)));
	model = opU(model, mBloom(p, 2, BloomSpec(1.9, vec2(.2, 1.), 1.3, .8, .13, .03, 0., smoothstep(.5, -.1, f) * 1.0, vec3(.5))));

   	p = pp;
    p -= vec3(-.7,.1,.8);
    pR(p.xz, 1. + 2.6);
    gmTransform(p);
    model = opU(model, mBloom(p, 3, BloomSpec(1.05, vec2(-.25, 1.), .45, .45, .41, .15, .5, -.5, vec3(.5))));

    return model;
}

float GIZMO_MAP(vec3 p) {
    return map(p).d;
}

//========================================================
// Rendering
//========================================================

// compile speed optim from IQ https://www.shadertoy.com/view/Xds3zN
vec3 calcNormal(vec3 pos){
    vec3 n = vec3(0.0);
    for( int i=0; i<4; i++ )
    {
        vec3 e = 0.5773*(2.0*vec3((((i+3)>>1)&1),((i>>1)&1),(i&1))-1.0);
        n += e*map(pos+0.0005*e).d;
    }
    return normalize(n);
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

mat3 basisMatrix(vec3 forward, vec3 up) {
    vec3 ww = normalize(forward);
    vec3 uu = normalize(cross(up,ww));
    vec3 vv = normalize(cross(ww,uu));
    return mat3(-uu, vv, ww);
}

struct Hit {
    Model model;
    vec3 pos;
    float len;
    bool sky;
};

Hit march(vec3 origin, vec3 rayDir, float maxDist) {
    vec3 p;
    float len = 0.;
    float dist = 0.;
    bool sky = false;
    Model model;

    for (float i = 0.; i < 150.; i++) {
        len += dist;
        p = origin + len * rayDir;
        model = map(p);
        dist = model.d;
        if (dist < hitEps) {
            break;
        }
        if (len >= maxDist) {
            sky = true;
            break;
        }
    }   

    return Hit(model, p, len, sky);
}

// bokeh offset from yx
// https://www.shadertoy.com/view/ts2cWm
vec2 bokeh(vec2 seed){
	vec2 a=seed;
    if(a.y>a.x)
        a=1.-a;
    a.y*=PI*2./a.x;
    return a.x*vec2(cos(a.y),sin(a.y));
}

// texturing, this is pretty messy :/
vec4 shade(Hit hit) {
    Model model = hit.model;
    vec3 p = hit.pos;

    vec3 col;

    float vor = 1. - voronoi3(p * 150.);
    float vor2 = 1. - voronoi3(p * 50.);
    float v1 = smoothstep(.7, .0, (mix(vor, vor * vor2, .5))/2. + .8 - model.edge * 20.);

    vec3 mp = model.pos*30.;

    if (model.id == 2) {
        mp *= 2.5;
    }
    if (model.id == 1) {
        mp *= 1.25;
    }

    float f = fbm(mp, 7);
    float v = smoothstep(.4, .8, noise31(p * 8.));
    
    if (model.id == 1) {
        col = vec3(.15,.16,.22);
        col = mix(col, col * vec3(.8,1.4,.9), 1.-smoothstep(.0, .05, model.slice+.025));
        //col = mix(col, col * vec3(1,1.2,1.5), smoothstep(.0, .05, model.edge) * step(vor2, .8) * smoothstep(1.3, .6, model.cell.y));
        col = mix(col, col *  vec3(1,1.2,1.5), smoothstep(.0, .05, model.edge) * step(vor2, .8) * smoothstep(.1, .52, model.cellt));
        col = mix(col, col * .7, (1. - step(vor2, .8)) * smoothstep(.0, .05, model.edge));
        //col = mix(col, col * vec3(3.,1.,1.), (1.-v1) * smoothstep(.05, 0., model.edge) * smoothstep(.6, 1., model.uv.y) * smoothstep(1.3, .6, model.cell.y));
        col = mix(col, col * vec3(3.,1.,1.), (1.-v1) * smoothstep(.05, 0., model.edge) * smoothstep(.6, 1., model.uv.y) * smoothstep(.1, .52, model.cellt));
        //col = mix(col, vec3(.08,.02,.07), (1.-v1) * smoothstep(-.1, 0., model.wedges) * smoothstep(.9, 1., model.uv.y) * smoothstep(2., 1., model.cell.y));
        col = mix(col, vec3(.08,.02,.07), (1.-v1) * smoothstep(-.1, 0., model.wedges) * smoothstep(.9, 1., model.uv.y) * smoothstep(-.2, .3, model.cellt));
        col = max(col, vec3(0));
        col = pow(col * vec3(1,1,1.6), vec3(1,1.1,1.3));      
    }

    if (model.id == 2) {
        col = vec3(.2,.3,.25);
        col = mix(col, vec3(.1,.25,.3), smoothstep(.7, .1, model.uv.y));
        col = mix(col, vec3(.7,.9,1), .5 * smoothstep(.4, 1., model.uv.y));
        col = mix(col, col * vec3(2.,3.,3.5), smoothstep(.8, .1, model.uv.y) * clamp(sin(f * 2. + 10. + v * 5.), 0., 1.) );
    }

    if (model.id == 3) {
        col = vec3(.125,.15,.3);
        col = mix(col, vec3(.3,.17,.3), smoothstep(.6, .2, model.cellt));
        col = mix(col, vec3(.2,.05,.12), smoothstep(.0, 1., model.point+.2));
        col = mix(col, col * vec3(.45,.4,.55), .7 * (1. - step(vor * (1.-vor2), .35)) * smoothstep(.0, .05, model.edge));
        col = mix(col, vec3(.2,.05,.12) * .5, .8 * smoothstep(.4, .0, vor2 * vor * (model.edge * 100. - model.point - .5)));
    }

    float dust = (sin(f * 4. + 3. + mp.x * .25) * .5 + .5);
    dust = pow(dust, 2.);
    dust = max(0., v - dust);
    dust = dust * .5 + pow(dust*.79, 3.);
    
    if (model.id == 1) {
	    dust *= smoothstep(.008, .1, model.edge);
        dust *= .66;
    }
    
    if (model.id == 3) {
	    dust *= smoothstep(.0, .05, model.edge);
        dust *= .2;
    }
    
    dust *= mix(1., smoothstep(.4, .6, 1.-vor), .5);
    col = mix(col, col * 3., dust);

    return vec4(col, 1.);
}

vec2 rndunit2(vec2 seed ) {
    vec2 h = seed * vec2(1,6.28318530718);
    float phi = h.y;
    float r = sqrt(h.x);
	return r*vec2(sin(phi),cos(phi));
}

// main path tracing loop, based on yx's
// https://www.shadertoy.com/view/ts2cWm
void mainImage(out vec4 fragColor, in vec2 fragCoord) {
    calcPhyllotaxis();

    vec2 uv = fragCoord.xy / iResolution.xy;
    vec4 sampl = vec4(0);
    
    #ifdef PREVIEW
        sampl = vec4(0);
    #endif

    vec2 p = (-iResolution.xy + 2.* fragCoord) / iResolution.y;

    vec2 seed = hash22(uv * 200. + float(iFrame) * .01);
    vec2 seed2 = hash22(uv * 300. + float(iFrame) * .02);

    // jitter for antialiasing
    #ifndef PREVIEW
        p += 2. * (seed - .5) / iResolution.xy;
    #endif

    vec3 camPos = vec3(.025,2.4,1.025);
    vec3 camTar = vec3(-.475,-.2,.025);

    camTar = mix(camTar, camPos, .25);
    
    #ifndef PREVIEW
        mat3 bokehMat = basisMatrix(camTar - camPos, vec3(1,0,0));
    	camPos += bokehMat * vec3(bokeh(seed2) * .02, 0);
    #endif

    mat3 camMat = basisMatrix(camTar - camPos, vec3(-1,1,-2));
    vec3 rayDir = normalize(camMat * vec3(p.xy, 4.));

    vec3 origin = camPos;

    origin = eye;
    rayDir = normalize(dir);

    float fpd = length(origin) - .5;//.385 * focalLength;
    vec3 fp = origin + rayDir * fpd;
    origin = origin + vec3(rndunit2(seed), 0.) * mat3(vView) * .08;
    rayDir = normalize(fp - origin);

    Hit hit;
    vec3 col = vec3(0);
    vec3 nor, ref;

    vec3 sunPos = vec3(5,5,-5);
    vec3 accum = vec3(1);
    vec3 sunColor = vec3(8.10,6.00,4.20)/5.;
    vec3 skyColor = vec3(0.50,0.70,1.00);
    vec3 bgCol = skyColor * .001 * vec3(.75,.75,1.);
    
    Hit firstHit;

    #ifdef PREVIEW
        const int MAX_BOUNCE = 1;
    #else
        const int MAX_BOUNCE = 4;
    #endif
    
    for (int bounce = 0; bounce < MAX_BOUNCE; bounce++) {
        hit = march(origin, rayDir, 15.);
        
        if (bounce == 0) {
            firstHit = hit;
            
            if (hit.sky) {
            	col = bgCol;
                break;
            }
        }
        
        if (hit.sky) {
            nor = normalize(hit.pos);
            col += max(nor.y, 0.) * accum * skyColor;
           	col += smoothstep(.2, 1., dot(nor, normalize(sunPos))) * sunColor * accum;
            break;
        }
        
       	vec4 material = shade(hit);
        accum *= material.rgb;
        nor = calcNormal(hit.pos);

        #ifdef PREVIEW
            col += max(nor.y + .3, 0.) * accum * skyColor * .5;
           	col += smoothstep(.2, 1., dot(nor, normalize(sunPos))) * sunColor * accum * .5;
        #endif
        
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

        seed = hash12(seed.x);
    }
    
    float fog = 1. - exp((firstHit.len - 1.9) * - 5.);
    col = mix(col, bgCol, clamp(fog, 0., 1.)); 

    if (drawIndex > 0.) {
        vec3 lastCol = texture(iChannel0, fragCoord.xy / iResolution.xy).rgb;
        col = mix(lastCol, col, 1. / (drawIndex + 1.));
    }
    
    fragColor = vec4(col,1);
}
