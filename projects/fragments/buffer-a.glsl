#version 300 es
precision mediump float;

float gmTransform(inout vec3 p, vec3 t, vec4 r, vec3 s) {
  p -= t;
  p = mix(dot(r.xyz,p)*r.xyz, p, cos(-r.w))+sin(-r.w)*cross(r.xyz,p);
  p /= s;
  return min(s.x, min(s.y, s.z));
}


precision highp float;

uniform vec2 iResolution;
uniform vec2 iOffset;
uniform float iGlobalTime;
uniform vec4 iMouse;
uniform sampler2D previousSample; // buffer-a.glsl filter: linear wrap: clamp
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

#define PI 3.1415926


// Spectrum palette
// IQ https://www.shadertoy.com/view/ll2GD3

vec3 pal( in float t, in vec3 a, in vec3 b, in vec3 c, in vec3 d ) {
    return a + b*cos( 6.28318*(c*t+d) );
}

vec3 spectrum(float n) {
    return pal( n, vec3(0.5,0.5,0.5),vec3(0.5,0.5,0.5),vec3(1.0,1.0,1.0),vec3(0.0,0.33,0.67) );
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

const uint k = 1103515245U;  // GLIB C

// https://www.shadertoy.com/view/XlXcW4
vec3 hash33( vec3 xs )
{
    uvec3 x = uvec3(xs);
    x = ((x>>8U)^x.yzx)*k;
    x = ((x>>8U)^x.yzx)*k;
    x = ((x>>8U)^x.yzx)*k;
    return vec3(x)*(1.0/float(0xffffffffU));
}

float hash13(vec3 p3)
{
	return hash33(p3).x;
}


//========================================================
// Utils
//========================================================

#define saturate(x) clamp(x, 0., 1.)

float smin(float a, float b, float k){
    float f = clamp(0.5 + 0.5 * ((a - b) / k), 0., 1.);
    return (1. - f) * a + f  * b - f * (1. - f) * k;
}

void pR(inout vec2 p, float a) {
    p = cos(a)*p + sin(a)*vec2(p.y, -p.x);
}

float vmin(vec3 v) {
    return min(min(v.x, v.y), v.z);
}

float vmax(vec3 v) {
    return max(max(v.x, v.y), v.z);
}

float vmax2(vec2 v) {
    return max(v.x, v.y);
}

float fBox(vec3 p, vec3 b) {
    vec3 d = abs(p) - b;
    return length(max(d, vec3(0))) + vmax(min(d, vec3(0)));
}

float fBox2(vec2 p, vec2 b) {
	vec2 d = abs(p) - b;
	return length(max(d, vec2(0))) + vmax2(min(d, vec2(0)));
}

mat3 lookUp(vec3 up, vec3 forward) {
    vec3 ww = normalize(up);
    vec3 uu = normalize(cross(ww,forward));
    vec3 vv = normalize(cross(uu,ww));
    return mat3(uu, ww, vv);
}

vec3 pLookUp(vec3 p, vec3 up, vec3 forward) {
    return p * lookUp(up, forward);
}

vec2 rndcircle(vec2 seed) {
    float a = seed.x * 2. * PI;
    float r = sqrt(seed.y);
    return vec2(r * cos(a), r * sin(a));
}

float unlerp(float minv, float maxv, float value) {
  return (value - minv) / (maxv - minv);
}


//========================================================
// Modelling
//========================================================

float invert;

struct Model {
    float d;
    int id;
};

float sdCrystalOne(vec3 size, vec3 p) {
    float d = fBox(p, size);
    d = max(d, -abs(p.x));
    d = max(d, -(d + vmin(size) * .333));
    return d;
}

float sdCrystalLoop(vec3 size, vec3 l, vec3 p, float seed) {

    p.y = max(p.y, .5 * size.y / l.y);
    
    p.y -= size.y * .5;
    size.y *= .5;

    float bs = 1.;
    float bound = fBox(p, size * vec3(1.5,1.4,1.5));
    if (bound > bs) {
        return bound;
    }
   
    vec3 pp = p;
    float d = 1e12;
    
    for (int x = 0; x < int(l.x); x++)
    for (int y = 0; y < int(l.y); y++)
    for (int z = 0; z < int(l.z); z++) {
        p = pp;
        vec3 c = vec3(x, y, z);
        p -= ((c + .5) / l - .5) * size * 2.;
        vec3 sz = size / l;
        vec3 h3 = hash33(c + 11. + seed);
        p -= (h3 * 2. - 1.) * sz * .5;
        float m = hash13(c * 10. + 27. + seed);
        sz *= mix(.6, 1.5, m);
        sz.xz *= mix(1.8, .45, pow(float(y) / (l.y - 1.), .5));
        float d2 = fBox(p, sz);
        d2 = max(d2, -abs(p.x));
        if (h3.z > .5 && c.y > 0.) {
            d2 = max(d2, -abs(p.y - (m * 2. - 1.) * sz.y * .5));
        }
        d = min(d, d2);
    }
    
    d = max(d, -(d + vmin(size / l) * .5));
    
    d = mix(d, bound, smoothstep(bs * .8, bs, bound));

    return d;
}


float sdCrystalLoop2(vec3 size, vec3 l, vec3 p) {

    size *= .9;
    
    p.y -= size.y * .5;
    size.y *= .5;
    
    float bs = 1.;
    float bound = fBox(p, size * vec3(1.2,1.8,1.2));
    if (bound > bs) {
        return bound;
    }

    vec3 pp = p;
    float d = 1e12;
    
    for (int x = 0; x < int(l.x); x++)
    for (int y = 0; y < int(l.y); y++)
    for (int z = 0; z < int(l.z); z++) {
        p = pp;
        vec3 c = vec3(x, y, z);
        p -= ((c + .5) / l - .5) * size * 2.;
        vec3 sz = size / l;
        float m = hash13(c+15.);
        sz *= mix(1.1, 1.75, m);
        float d2 = fBox(p, sz) + .01;
        if (c == vec3(0)) {
            d2 = max(d2, -abs(p.x));
        }
        d2 = max(d2, -d);
        d = min(d, d2);
    }
    
    d = max(d, -(d + vmin(size / l) * .5));
    
    d = mix(d, bound, smoothstep(bs * .8, bs, bound));
    
    return d;
}

float sdCrystalField(vec3 p) {
    float d = 1e12;
    float s = .2;
    vec3 pp = p;
    float scl = 1.;

    p = pLookUp(p - vec3(.8,0,-.8), vec3(.2,1,-.5), vec3(1,0,1));
    scl = gmTransform(p);
    d = sdCrystalLoop(vec3(.35, 1.6, .35), vec3(2,3,2), p, 0.) * scl;
    
    p = pp;
    p = pLookUp(p - vec3(1.8,-.15,-.3), vec3(0,1,0), vec3(1,0,-.25));
    scl = gmTransform(p);
    d = smin(d, sdCrystalOne(vec3(.13), p) * scl, s);
    
    p = pp;
    p = pLookUp(p - vec3(-.3,0,.5), vec3(-.0,1,.2), vec3(.0,0,1)) - vec3(0,-.2,0);
    scl = gmTransform(p);
    d = smin(d, sdCrystalLoop2(vec3(.3, .35, .3), vec3(2,1,2), p) * scl, s);
    
    p = pp;
    p = pLookUp(p - vec3(-1.8,-.15,-2.3), vec3(-1,2,-.5), vec3(-1,0,-2));
    scl = gmTransform(p);
    d = smin(d, sdCrystalLoop(vec3(.15,1.,.15), vec3(1,3,1), p, 11.) * scl, s);
    return d;
}


Model map(vec3 p) {
    //vec2 im = iMouse.xy / iResolution.xy;     
    vec2 im = vec2(.43,.43);
    pR(p.yz, (.5 - im.y) * PI);
    pR(p.xz, (.5 - im.x) * PI * 2.5);
    
    p.y += .6;
    p.xz -= vec2(-1,1) * .4;
    
    float d = p.y + .25;    
    d = smin(d, length(p - vec3(.6,-2.5,-.7)) - 2.5, .6);
    d = smin(d, length(p - vec3(-.3,-.5,.5)) - .5, .4);

    float d2 = sdCrystalField(p);

    float df = pow(d2 + .333, .5) * 1.5;
    float ripple = 7.;    
    d += cos(max(df, 0.) * ripple * PI * 2.) * .015;

    Model m = Model(d, 2);
    Model m2 = Model(d2 * invert, 1);
    
    if (m2.d < m.d) {
        m = m2;
    }

    return m;
}

float GIZMO_MAP(vec3 p) {
    return map(p).d;
}


//========================================================
// Rendering
//========================================================

const float sqrt3 = 1.7320508075688772;

// https://iquilezles.org/articles/normalsSDF
vec3 calcNormal( in vec3 pos )
{
    vec3 n = vec3(0.0);
    for( int i=0; i<4; i++ )
    {
        vec3 e = 0.5773*(2.0*vec3((((i+3)>>1)&1),((i>>1)&1),(i&1))-1.0);
        n += e*map(pos+0.001*e).d;
    }
    return normalize(n);
}

struct Hit {
    Model model;
    vec3 pos;
    float len;
};

Hit march(vec3 origin, vec3 rayDir, float maxDist) {
    vec3 p;
    float len = 0.;
    float dist = 0.;
    Model model;

    for (float i = 0.; i < 100.; i++) {
        len += dist;
        p = origin + len * rayDir;
        model = map(p);
        dist = model.d;
        if (abs(model.d) / len < .0002) {
            break;
        }
        if (len >= maxDist) {
            len = maxDist;
            model.id = 0;
            break;
        }
    }   

    return Hit(model, p, len);
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

float intersectPlane(vec3 rOrigin, vec3 rayDir, vec3 origin, vec3 normal, vec3 up, out vec2 uv) {
    float d = dot(normal, (origin - rOrigin)) / dot(rayDir, normal);
  	vec3 point = rOrigin + d * rayDir;
	vec3 tangent = cross(normal, up);
	vec3 bitangent = cross(normal, tangent);
    point -= origin;
    uv = vec2(dot(tangent, point), dot(bitangent, point));
    return max(sign(d), 0.);
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

mat3 envOrientation;

vec3 light(vec3 origin, vec3 rayDir) {

    origin = -origin;
    rayDir = -rayDir;

    origin *= envOrientation;
    rayDir *= envOrientation;

    vec2 uv;
    vec3 pos = vec3(-6);
    float hit = intersectPlane(origin, rayDir, pos, normalize(pos), normalize(vec3(-1,1,0)), uv);
    float l = smoothstep(.75, .0, fBox2(uv, vec2(.5,2)) - 1.);
    l *= smoothstep(6., 0., length(uv));
	return vec3(l) * hit * 2.;
}

vec2 rndunit2(vec2 seed ) {
    vec2 h = seed * vec2(1,6.28318530718);
    float phi = h.y;
    float r = sqrt(h.x);
	return r*vec2(sin(phi),cos(phi));
}

vec4 draw(vec2 fragCoord) {

    vec2 seed = hash22(fragCoord + (float(iFrame)) * sqrt3);

    invert = 1.;
    
    envOrientation = sphericalMatrix(((vec2(81.5, 119) / vec2(187)) * 2. - 1.) * 2.);

    vec2 p = (-iResolution.xy + 2.* fragCoord) / iResolution.y;
    
    // jitter for antialiasing
    p += 2. * (seed - .5) / iResolution.xy;

    float focalLength = 3.;
    vec3 camPos = vec3(0, 0, 1.5) * focalLength;
    vec3 camTar = vec3(0, 0, 0);
    
    // DOF
    camPos.xy += rndcircle(seed) * .05;
    seed = hash22(seed);
    
    vec3 ww = normalize(camTar - camPos);
    vec3 uu = normalize(cross(vec3(0,1,0),ww));
    vec3 vv = normalize(cross(ww,uu));
    mat3 camMat = mat3(-uu, vv, ww);

    vec3 rayDir = normalize(camMat * vec3(p.xy, focalLength));
    vec3 origin = camPos;

    origin = eye;
    rayDir = normalize(dir);

    // position on sensor plane
    vec3 cameraForward = -transpose(vView)[2].xyz;
    origin = origin + rayDir / dot(rayDir, cameraForward) * fov;
    
    // position on focal plane
    float focalDistance = length(origin);
    vec3 focalPlanePosition = origin + focalDistance * rayDir / dot(rayDir, cameraForward);
    origin = origin + vec3(rndunit2(seed), 0.) * mat3(vView) * .05;
    rayDir = normalize(focalPlanePosition - origin);

    Hit hit = march(origin, rayDir, 4. * focalLength);

    float firstHitLen = hit.len;
    bool isFloor = hit.model.id == 2;
    
    vec3 nor, ref, raf; 
    float ior, offset;
    
    float wavelength = seed.y;
    vec3 col = vec3(0);    

    for (int bounce = 0; bounce < 10; bounce++) {
   
        if (bounce > 0) {
           seed = hash22(seed);
           hit = march(origin, rayDir, 6.);
        }
        
        if (hit.model.id == 0) {
            break;
        }

        nor = calcNormal(hit.pos);
        
        if (hit.model.id == 1) {
            
            // Reflective bounce
            
            ref = reflect(rayDir, nor);
            
            // shade
            col += light(hit.pos, ref) * .5;
            col += pow(max(1. - abs(dot(rayDir, nor)), 0.), 5.) * .1;
            col *= vec3(.85,.85,.98);

            // refract
            ior = mix(1.2, 1.8, wavelength);
            ior = invert < 0. ? ior : 1. / ior;
            raf = refract(rayDir, nor, ior);
            bool tif = raf == vec3(0); // total internal reflection
            rayDir = tif ? ref : raf;
            invert *= -1.; // not correct but gives more interesting results
            //invert = tif ? 1. : -1.; // 'correct' refraction
            
        } else {
            
            // Diffuse bounce
            
            seed = hash22(seed);
            rayDir = getSampleBiased(nor, 1., seed);            
 
        }
        
        offset = .01 / abs(dot(rayDir, nor));
        origin = hit.pos + offset * rayDir;
    }    
    
    if (isFloor) {
        col *= 2.;
    }

    vec3 fogcol = vec3(.0);
    //col = mix(col, fogcol, saturate(1.0 - exp2(-.0006 * pow(firstHitLen - length(camPos*.666), 5.))));

    // Dispersion
    col *= spectrum(-wavelength+.30);

    return vec4(col, 1);
}

void mainImage(out vec4 fragColor, in vec2 fragCoord) {
    vec4 col = draw(fragCoord);
   
    if (drawIndex > 0.) {
        vec4 lastCol = texture(previousSample, fragCoord.xy / iResolution.xy);
        col = mix(lastCol, col, 1. / (drawIndex + 1.));
    }
    
    fragColor = vec4(col.rgb, 1);
}
