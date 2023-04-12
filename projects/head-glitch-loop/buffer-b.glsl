
// framebuffer drawcount: 1, tile: 1
// filter: nearest, linear, mipmap
// wrap: clamp, repeat


precision highp float;

uniform sampler2D iChannel0; // buffer-a.glsl filter: linear wrap: clamp
uniform vec2 iChannel0Size;

uniform mat4 cameraMatrix;
uniform vec3 cameraPosition;

uniform sampler2D previousSample; // buffer-b.glsl filter: linear
uniform float drawIndex;
uniform int iFrame;
uniform vec4 iMouse;

varying vec3 eye;
varying vec3 dir;
varying float fov;
varying float aspect;
varying mat4 vView;

#pragma glslify: inverse = require(glsl-inverse)

#define ANIMATE
#define SSS
//#define DOF
//#define PREVIEW


float time;




float round(float t) { return floor(t + 0.5); }
vec2 round(vec2 t) { return floor(t + 0.5); }

void pR2(inout vec2 p, float a) {
    p = cos(a)*p + sin(a)*vec2(p.y, -p.x);
}

// Transform xyz coordinate in range -1,-1,-1 to 1,1,1
// to texture uv and channel
vec3 spaceToTex(vec3 p, vec2 size, float warp, out float warped) {

    p = clamp(p, -1., 1.);

    // p.x = mix(0., sin(p.x * 2.) * 10., .1);
    
    // p *= 1.3;
    // p.z -= .3;

    p = p * .5 + .5; // range 0:1

    // p.x = sin(p.x * 5.) / 2.;

    vec2 sub = texSubdivisions;
    vec2 subSize = floor(size / sub);

    // uv = clamp(uv, 0., 1.);

    // Work out the z index
    float zRange = sub.x * sub.y * 4. - 1.;

    float i = round(p.z * zRange);

    //return vec3(i/zRange);

    // return vec3(mod(i, sub.x)/sub.x);
    // translate uv into the micro offset in the z block
    vec2 coord = p.xy * subSize;

    // Work out the macro offset for the xy block from the z block
    coord += vec2(
        mod(i, sub.x),
        mod(floor(i / sub.x), sub.y)
    ) * subSize;

    // FUCK WITH IT...

    float tt = fract(iTime);

    vec2 c2 = coord;

    //coord *= mix(1., tan(coord.y*10./coord.x*5. + tt * PI), .04 / 100.);
    //coord *= mix(1., tan(coord.x*10./coord.y*5. + tt * PI * 1.), .02 / 100.);
    //coord *= mix(1., tan(coord.y/10. + tt * PI), .002);
    //coord *= mix(1., tan((coord.x*coord.y)/9000. - tt * PI), .0005);
    //coord *= mix(1., sin(coord.x/coord.y*50. - tt * PI * 2.), .002);
    coord *= mix(1., sin(coord.y/coord.x*200. - tt * PI * 2.), .002);

    coord = mix(c2, coord, warp * 8.);

    //coord = mix(c2, coord, 3.);

    warped = distance(c2, coord);

    float c = floor(i / (sub.x * sub.y));
    vec3 uvc = vec3(coord / size, c);


    float f = 1500.;
    uvc.xy += tt / f;
   // uvc.xy = mix(uvc.xy, round(uvc.xy * vec2(f)) / vec2(f), .5);
    uvc.xy -= tt / f;
    // pR2(uvc.xy, .015);



    return uvc;
}

float range(float vmin, float vmax, float value) {
  return clamp((value - vmin) / (vmax - vmin), 0., 1.);
}

float pickIndex(vec4 v, int i) {
    if (i == 0) return v.r;
    if (i == 1) return v.g;
    if (i == 2) return v.b;
    if (i == 3) return v.a;
}

float warpedA;
float warpedB;

float mapTex(sampler2D tex, vec3 p, vec2 size) {
    p = p;
    // stop x bleeding into the next cell as it's the mirror cut
    #ifdef MIRROR
        p.x = clamp(p.x, -.95, .95);
    #endif
    vec2 sub = texSubdivisions;
    float zRange = sub.x * sub.y * 4. - 1.;
    float z = p.z * .5 + .5; // range 0:1
    float zFloor = (floor(z * zRange) / zRange) * 2. - 1.;
    float zCeil = (ceil(z * zRange) / zRange) * 2. - 1.;
    float warp = smoothstep(.1, .8, p.y * .5 + .5);
    warp = pow(warp, 4.);
    vec3 uvcA = spaceToTex(vec3(p.xy, zFloor), size, warp, warpedA);
    vec3 uvcB = spaceToTex(vec3(p.xy, zCeil), size, warp, warpedB);
    float a = pickIndex(texture2D(tex, uvcA.xy), int(uvcA.z));
    float b = pickIndex(texture2D(tex, uvcB.xy), int(uvcB.z));
    // return a;
    float d = mix(a, b, range(zFloor, zCeil, p.z));
    //d -= warp * .05;
    return d;
}


float vmax(vec2 v) {
    return max(v.x, v.y);
}

float vmax(vec3 v) {
    return max(max(v.x, v.y), v.z);
}

float vmin(vec3 v) {
    return min(min(v.x, v.y), v.z);
}

float vmin(vec2 v) {
    return min(v.x, v.y);
}

float fBox(vec3 p, vec3 b) {
    vec3 d = abs(p) - b;
    return length(max(d, vec3(0))) + vmax(min(d, vec3(0)));
}

void pR(inout vec2 p, float a) {
    p = cos(a)*p + sin(a)*vec2(p.y, -p.x);
}

float mHead(vec3 p) {
    vec3 pa = p;
    float bound = fBox(p, vec3(.45,.65,.6));
    #ifdef MIRROR
        p.x = -abs(p.x);
    #endif
    p += OFFSET / SCALE;
    bound = fBox(p, 1./SCALE);
    //return bound;
    if (bound > .01) {
        // return bound;
    }
    //p.x = -abs(p.x);
    //p += OFFSET / SCALE;
    p *= SCALE;
    float d = mapTex(iChannel0, p, iChannel0Size);
    //return min(d, max(bound, pa.x));
    return d;
    return min(d, bound + .02);
}

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

//    vec3 skin = pow(vec3(0.890,0.769,0.710), vec3(2.2));
  //  float flush = smoothstep(-1.75, -.0, model.albedo.x);
    //skin += mix(vec3(-.6,.0,.15) * .5, vec3(.4,-.03,-.05), flush);
    //skin *= vec3(1.1,.8,.7);
    //skin = clamp(skin, vec3(0,0,0), vec3(1,1,1));
    bool sss = false;
    #ifdef SSS
    sss = true;
    #endif

    vec3 col = mix(model.albedo, nor * .5 + .5, model.uvw.x);

    return Material(col, .0, 1., sss);
}


vec3 pal( in float t, in vec3 a, in vec3 b, in vec3 c, in vec3 d ) {
    return a + b*cos( 6.28318*(c*t+d) );
}

vec3 spectrum(float n) {
    return pal( n, vec3(0.5,0.5,0.5),vec3(0.5,0.5,0.5),vec3(1.0,1.0,1.0),vec3(0.0,0.33,0.67) );
}


Model map(vec3 p) {
    // return length(p) - .5;
    p.y -= .15;
    //pR(p.yz, .2);
    // pR(p.xz, iTime/2. + .4);
    // pR(p.yz, iTime/2. + .4);
    float d = mHead(p);
   // d = mix(d, fBox(p, vec3(.7)), sin(iTime) * .5+ .5);
    //d = length(p) - .5;

    float warped =  min(max(warpedA, warpedB)/3., 1.);

    vec3 col = mix(vec3(1), vec3(1,0,0), warped);
    return Model(d, vec3(warped, 0, 0), vec3(.5), 1);


    //return d;
  //  vec2 uv = spaceToTex(p);
//    return texture2D(iChannel0, uv).r;
    //return length(p) - .5;
}
/*
bool isDebug = false;

float mapDebug(vec3 p) {
    float d = map(p);
    return d;
    float r = min(abs(p.z), min(abs(p.x), abs(p.y-.05))) - .001;
    if (r < d) {
        isDebug = true;
        return r;
    } else {
        isDebug = false;
    }
    return d;

}

const int NORMAL_STEPS = 6;
vec3 calcNormal(vec3 pos){
    vec3 eps = vec3(.0005,0,0);
    vec3 nor = vec3(0);
    float invert = 1.;
    for (int i = 0; i < NORMAL_STEPS; i++){
        nor += map(pos + eps * invert) * eps * invert;
        eps = eps.zxy;
        invert *= -1.;
    }
    // pR(nor.xz, 1.);
    return normalize(nor);
}



void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
    vec2 p = (-iResolution.xy + 2. * fragCoord.xy) / iResolution.y;
    // vec4 last = texture2D(iChannel0, fragCoord.xy/iResolution.xy);
    // fragColor = last;
    // return;
    // if (last.x != 0.) {
    //     fragColor = last;
    //     return;
    // }
    // fragColor = vec4(vec3(1,0,0), 1.);
    // return;
    // vec3 space = texToSpace(fragCoord.xy, 0, iResolution);
    // // fragColor = vec4(space, 1); return;
    // // if (p.x < .9) {fragColor = vec4(spectrum(1.), 1); return;}
    // // fragColor = vec4(spectrum(space.z * .5 + .5), 1); return;

    // vec3 tex = spaceToTex(space, iResolution);
    // tex.b /= 4.;
    // // fragColor = vec4(vec3(step(tex.x, iTime)), 1); return;
    // // fragColor = vec4(vec3(tex.z), 1); return;
    // fragColor = vec4(vec3(tex), 1); return;
    // // fragColor = vec4(spectrum(tex.z), 1); return;


    // vec3 camPos = vec3(0,.05,3.2) * .5;
    // vec3 rayDirection = normalize(vec3(p + vec2(0,-0),-4));

    // float r2 = .0;//iTime;
    // pR(camPos.yz, r2);
    // pR(rayDirection.yz, r2);

    // float r = .5;//iTime + .7;
    // pR(camPos.xz, r);
    // pR(rayDirection.xz, r);

    vec3 camPos = eye;
    vec3 rayDirection = dir;

    vec3 rayPosition = camPos;
    float rayLength = 0.;
    float dist = 0.;
    bool bg = false;
    vec3 col = vec3(.15,.05,.15) * 0.;

    for (int i = 0; i < 300; i++) {
        rayLength += dist;
        rayPosition = camPos + rayDirection * rayLength;
        dist = mapDebug(rayPosition);

        if (abs(dist) < .001) {
            break;
        }
        
        if (rayLength > 10.) {
            bg = true;
            break;
        }
    }
    
    if ( ! bg) {
        vec3 n = calcNormal(rayPosition);
        col = n * .5 + .5;

        //col = vec3(.1) + clamp(dot(n, normalize(vec3(-1,1,1))), 0., 1.);
        
        if (isDebug) {
            float d = map(rayPosition);
            col = vec3(mod(d * 10., 1.));
        }
    }
    
  //  col = vec3(spaceToTex(vec3(-1,-1,-.7)), 0.);

    //vec2 uv = fragCoord.xy / iResolution.xy;
    //vec3 ps = texToSpace(uv)[3].xyz;
    //col = abs(ps);
    //col = vec3(texture2D(iChannel0, uv).a);
    
    fragColor = vec4(col,1.0);
}

*/












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

// iq https://www.shadertoy.com/view/tl23Rm
vec2 rndunit2(vec2 seed ) {
    vec2 h = seed * vec2(1,6.28318530718);
    float phi = h.y;
    float r = sqrt(h.x);
	return r*vec2(sin(phi),cos(phi));
}



//========================================================
// Rendering
//========================================================

vec3 sunPos = normalize(vec3(-1.5,1.5,.2)) * 100.;
vec3 skyColor = vec3(0.50,0.70,1.00);
vec3 sunColor = vec3(8.10,6.00,4.20) * 5.;


vec3 env(vec3 dir, bool includeSun) {
   vec3 col = mix(vec3(.5,.7,1) * .0, vec3(.5,.7,1) * 1., smoothstep(-.2, .2, dir.y));
   return col * .66;
}

/*
vec3 sunPos = normalize(vec3(-.5,1.5,-.2)) * 100.;
//vec3 sunPos = normalize(vec3(-1.5,1.5,-.7)) * 100.;
vec3 skyColor = vec3(0.50,0.70,1.00);
vec3 sunColor = vec3(10.10,6.00,4.20) * 5.;


vec3 env(vec3 dir, bool includeSun) {
    vec3 col = mix(vec3(0), skyColor, smoothstep(-.2, .2, dir.y));
    return col;
}
*/

vec3 calcNormal( in vec3 p ) // for function f(p)
{
    const float eps = 0.0001; // or some other value
    const vec2 h = vec2(eps,0);
    return normalize( vec3(map(p+h.xyy).d - map(p-h.xyy).d,
                           map(p+h.yxy).d - map(p-h.yxy).d,
                           map(p+h.yyx).d - map(p-h.yyx).d ) );
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
    vec3 bgCol = vec3(0.025,0.02,0.05) * .5;
    bool doSpecular = true;
    float pathLength = 0.;

    #ifndef PREVIEW
        const int MAX_BOUNCE = 4;
    #else
        const int MAX_BOUNCE = 1;
    #endif

    for (int bounce = 0; bounce < MAX_BOUNCE; bounce++) {
   
        hit = march(origin, rayDir, 3., 1.);

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

            //float fogAmount = 1.0 - exp( -hit.rayLength * .4 );
            //throughput *= mix(material.albedo, bgCol, fogAmount);
            throughput *= material.albedo;
        }

        if (doSSS) {
            origin = hit.pos;
            
            seed = hash22(seed);
            hit = walkOnSpheres(origin, nor, .033, seed);
            nor = calcNormal(hit.pos);

            float extinctionDist = distance(origin, hit.pos) * 10.;
            vec3 extinctionCol = material.albedo;
            //extinctionCol = mix(mix(extinctionCol, vec3(0,0,1), .5), vec3(1,0,0), clamp(extinctionDist - 1., 0., 1.));
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


const float sqrt3 = 1.7320508075688772;

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

    //col = vec3(1) * depth * .1;
/*
    #ifdef DOF
    float lo = length(off);
    col = mix(col, col * mix(vec3(1,1,0), vec3(0,0,1), lo * lo) * 2., .666);
    //col *= spectrum(lo * lo) * 1.5;
    #endif
*/
    return vec4(col, 1);
}

void mainImage(out vec4 fragColor, in vec2 fragCoord) {


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
