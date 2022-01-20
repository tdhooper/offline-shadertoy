// framebuffer drawcount: 10

precision highp float;

uniform vec2 iResolution;
uniform mat4 cameraMatrix;
uniform sampler2D previousSample; // buffer-a.glsl filter: linear
uniform float drawIndex;
uniform int iFrame;
uniform float iTime;

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

//float hash(vec2 p) { return fract(1e4 * sin(17.0 * p.x + p.y * 0.1) * (0.1 + abs(sin(p.y * 13.0 + p.x)))); }

// https://www.shadertoy.com/view/4djSRW
vec2 hash22(vec2 p)
{
	vec3 p3 = fract(vec3(p.xyx) * vec3(.1031, .1030, .0973));
    p3 += dot(p3, p3.yzx+33.33);
    return fract((p3.xx+p3.yz)*p3.zy);
}

struct Model {
    float d;
    vec3 albedo;
};

float time;

float vmax(vec3 v) {
	return max(max(v.x, v.y), v.z);
}
float fBox(vec3 p, vec3 b) {
	vec3 d = abs(p) - b;
	return length(max(d, vec3(0))) + vmax(min(d, vec3(0)));
}
float fBox(vec3 p, vec3 b, out vec3 face) {
	vec3 d = abs(p) - b;
    face = step(d.yzx, d.xyz)*step(d.zxy, d.xyz)*sign(p);
    return length(max(d, vec3(0))) + vmax(min(d, vec3(0)));
}
float smin(float a, float b, float k){
    float f = clamp(0.5 + 0.5 * ((a - b) / k), 0., 1.);
    return (1. - f) * a + f  * b - f * (1. - f) * k;
}

float mincol(inout float a, float b, inout vec3 cola, vec3 colb) {
    if (a < b) return a;
    cola = colb;
    return b;
}

Model map(vec3 p) {
    float d = 1e12;
    vec3 col = vec3(.5);
    vec3 face = vec3(1);
    float tm = time * PI * 2.;

    float scl = smoothstep(0., .2, time) - smoothstep(.8, 1., time);
    scl = .3;

    vec3 offset = vec3(sin(tm), -cos(tm + 2.), cos(tm + 5.) * .5) * scl;
    d = length(p + offset) - mix(.2, .4, sin(tm) * .5 + .5);

    offset = vec3(sin(tm + 2.5) * .5, cos(tm + 4.), -cos(tm + 3.)) * scl;
    d = smin(d, length(p + offset) - mix(.2, .4, sin(tm + .5) * .5 + .5), .2);

    offset = vec3(-sin(tm + 6.5), cos(tm + 1.5) * .5, -cos(tm + 4.5)) * scl;
    d = smin(d, length(p + offset) - mix(.2, .4, sin(tm + 1.) * .5 + .5), .2);
    
    p.z -= 1.;
    float box = fBox(p, vec3(1,1,2), face);
    box = max(box - .1, -box);
    box = max(box, p.z);
    d = mincol(d, box, col, 1. - (face * .5 + .5));
    return Model(d, col);
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

vec3 sunPos = normalize(vec3(3,5,7)) * 100.;
vec3 sunColor = vec3(8.10,6.00,4.20)/5.;
vec3 skyColor = vec3(0.50,0.70,1.00);

vec3 env(vec3 dir) {
    vec3 col = vec3(0);
    col += max(dir.y, 0.) * skyColor;
    col += smoothstep(.5, 1., dot(dir, normalize(sunPos))) * sunColor;
    return col;
}

struct Hit {
    Model model;
    vec3 pos;
    vec3 dir;
    float len;
    bool bg;
};

Hit march(vec3 origin, vec3 dir, float maxDist) {
    vec3 pos;
    float len = 0.;
    Model model;
    float dist = 0.;
    bool bg = false;
    for (int i = 0; i < 200; i++) {
        len += dist;
        pos = origin + dir * len;
        model = map(pos);
        dist = model.d;

        if (abs(dist) < .0001) {
        	break;
        }
        
        if (len > maxDist) {
            bg = true;
            break;
        }
    }
    return Hit(model, pos, dir, len, bg);
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

    #ifdef PREVIEW
        const int MAX_BOUNCE = 1;
    #else
        const int MAX_BOUNCE = 4;
    #endif

    for (int bounce = 0; bounce < MAX_BOUNCE; bounce++) {

        float maxDist = bounce == 0 ? 20. : 5.;
        hit = march(origin, rayDir, maxDist);

        if (hit.bg) {
            if (bounce == 0) {
                //col = env(rayDir);
                col = vec3(.02);
                break;
            }
            col += env(rayDir) * accum;
            break;
        }
        
        accum *= hit.model.albedo;
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
        bool hitSun = sunLight > 0. && march(shadowOrigin, sunSampleDir, 5.).bg;
        
        if (hitSun) {
            col += accum * sunColor * sunLight;
        }

        // set new origin and direction for dffuse bounce
        origin = hit.pos + nor * .002;
        rayDir = getSampleBiased(nor, 1., seed);

        seed = hash22(seed);
    }
        
    if (drawIndex > 0.) {
        vec3 lastCol = texture2D(previousSample, fragCoord.xy / iResolution.xy).rgb;
        col = mix(lastCol, col, 1. / (drawIndex + 1.));
    }

    fragColor = vec4(col, 1);
}
