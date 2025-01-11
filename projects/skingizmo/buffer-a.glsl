#version 300 es
precision mediump float;

float gmTransform(inout vec3 p, vec3 t, vec4 r, vec3 s) {
  p -= t;
  p = mix(dot(r.xyz,p)*r.xyz, p, cos(-r.w))+sin(-r.w)*cross(r.xyz,p);
  p /= s;
  return min(s.x, min(s.y, s.z));
}


// framebuffer drawcount: 1

precision highp float;

uniform vec2 iResolution;
uniform mat4 cameraViewMatrix;
uniform sampler2D previousSample; // buffer-a.glsl filter: linear
uniform float drawIndex;
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

// HG_SDF
// https://www.shadertoy.com/view/Xs3GRB

#define PI 3.14159265359

void pR(inout vec2 p, float a) {
    p = cos(a)*p + sin(a)*vec2(p.y, -p.x);
}

float vmax(vec3 v) {
	return max(max(v.x, v.y), v.z);
}

float smin(float a, float b, float k){
    float f = clamp(0.5 + 0.5 * ((a - b) / k), 0., 1.);
    return (1. - f) * a + f  * b - f * (1. - f) * k;
}

float smax(float a, float b, float k) {
    return -smin(-a, -b, k);
}


float fBox(vec3 p, vec3 b) {
	vec3 d = abs(p) - b;
	return length(max(d, vec3(0))) + vmax(min(d, vec3(0)));
}

const float sqrt3 = 1.7320508075688772;


//========================================================
// Modeling
//========================================================


struct Material {
    vec3 albedo;
    float specular;
    float roughness;
    bool sss;
};

struct Model {
    float d;
    vec3 uvw;
    int id;
    float understep;
};


Material shadeModel(float rlen, Model model, inout vec3 nor) {
    vec3 skin = pow(vec3(0.890,0.769,0.710), vec3(2.2));
    skin = mix(skin, vec3(.4,.5,.5), .25);
    skin = mix(skin, vec3(1,1,0) * .5, .02);
   
    // seams
    skin = mix(skin, skin * vec3(.8,0,0), model.uvw.y * .5);
   
    if (model.id == 1) {
        return Material(skin, .005, .2, true);
    }

    model.uvw /= .25 * .85;

    vec3 floorAlbedo = vec3(0.50,0.70,1.00);
    vec3 albedo = floorAlbedo * .05;
    float spec = .02;
    float rough = .3;

    float px = .0005 * rlen;
    float w = .003;
    float sp = 1.;
    float line = (1. - (  (abs(mod(model.uvw.x + sp / 2., sp) - sp / 2.) - w) / px  ));
    line = max(line, (1. - (  (abs(mod(model.uvw.z + sp / 2., sp) - sp / 2.) - w) / px  )));
    line = clamp(line, 0., 1.);
    spec = mix(spec, .1, line);
    albedo = mix(albedo, floorAlbedo * .1, line);
    float line2 = line;

    w /= 4.;
    sp /= 8.;
    line = (1. - (  (abs(mod(model.uvw.x + sp / 2., sp) - sp / 2.) - w) / px  ));
    line = max(line, (1. - (  (abs(mod(model.uvw.z + sp / 2., sp) - sp / 2.) - w) / px  )));
    line = clamp(line, 0., 1.);
    spec = mix(spec, .1, line);
    albedo = mix(albedo, floorAlbedo * .0666, line * (1.-line2));

    return Material(albedo, spec, rough, false);
}

float sin3(vec3 x) {
    return sin(x.x) * sin(x.y) * sin(x.z);
}

void pR45(inout vec2 p) {
	p = (p + vec2(p.y, -p.x))*sqrt(0.5);
}
float time = 0.;

Model skinbox(vec3 p) {

    float o = 0.;

    float scl = 1.;

    scl *= gmTransform(p);

    vec3 pp = p;
    vec3 uvw = p;

    float d = fBox(p, vec3(.4 * .2125)) - .1 * .2125;

    d *= scl;

    float thin = smoothstep(.3, .5, length(p.yz));
    thin = 1.;

    p = pp;    

    p /= .2125;

    vec3 p3 = p * 30.;
    pR45(p3.xy);
    pR45(p3.yz);
    pR45(p3.zx);
    p3 += sin3(p * 30.);

    vec3 p2 = p;
    pR45(p2.xy);
    pR45(p2.yz);
    pR45(p2.zx);
    float l = 0.;

    float spots = (pow(abs(sin3(p2 * 10.)), 10.));
    l += spots * 12.; // spots
    float spotMask = max(spots, smoothstep(.1, .4, spots));
    spotMask *= thin;

    p2 += sin3(p * 2. + .5) * 1.5;
    
    float seam = abs(sin3(p2 * 25.));
    seam = min(seam, mix(1., abs(sin3(p2 * 75.)), .95));
    float seamMask = seam;
    uvw.y = (1. - pow(seamMask, .15)) * (1. - spotMask);
    seam = (1. - pow(seam, .02)) * (1. - spotMask);
    l -= seam * 20.; // seams
    l *= -.005;

    // wrinkles
    float w = 0.;
    w -= -(abs(sin3(p * 1. + sin(p3 * 5.) * 2.)) * 2. - 1.) * .5;
    w += abs(sin3(p3 * 250. / 30.));
    w *= .00001;
    w *= pow(seamMask, .5) * (1. - spotMask * .5);

    l *= thin;
    w *= thin;
            
    if (abs(d) < .1) {
        d += l * .2125 * scl;
    }
    
    if (abs(d) < .01) {
        d += w * 600. * .2125 * scl;
    }
    
    p = pp;
    return Model(d, uvw, 1, 1.);
}

Model map(vec3 p) {
    pR(p.yz, (.5 - .25) * PI / 2.);
    pR(p.xz, (.5 - .6) * PI * 2.);

    Model m = skinbox(p);

    float d = p.y + .5 * .25 * .85;   
    Model m2 = Model(d, p, 2, 1.);

    if (m2.d < m.d) {
        m = m2;
    }

    return m;
}

const float boundRadius = 10.3;

float GIZMO_MAP(vec3 p) {
    return map(p).d;
}


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


vec3 sunPos = normalize(vec3(-1,1,-.75)) * 100.;
vec3 skyColor = vec3(0.50,0.70,1.00);
vec3 sunColor = vec3(8.10,6.00,4.20) * 3. * .1;

vec3 env(vec3 dir, bool includeSun) {
   vec3 col = mix(vec3(.5,.7,1) * .0, vec3(.5,.7,1) * 1., smoothstep(-.2, .2, dir.y));
   return col * .5;
}

struct Hit {
    Model model;
    vec3 pos;
};

Hit march(vec3 origin, vec3 rayDirection, float maxDist, float understep) {

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

// Walk on spheres subsurface scattering
// inspired by blackle https://www.shadertoy.com/view/wsfBDB
Hit walkOnSpheres(vec3 origin, vec3 normal, float startdepth, inout vec2 seed) {
    Model model;
    
    vec2 lastSeed = seed;
    seed = hash22(seed);
    normal = normalize(tan(vec3(seed.x, seed.y, lastSeed.x) * 2. - 1.));
    
    float understep = .4;
    
    model = map(origin - normal * startdepth);
    origin -= normal * abs(model.d * understep);
    
    for (int v = 0; v < 256; v++) {
        model = map(origin);

        if (abs(model.d) < .0002) break;
        
        vec2 lastSeed = seed;
        seed = hash22(seed);
        vec3 dir = normalize(tan(vec3(seed.x, seed.y, lastSeed.x) * 2. - 1.));
        
        origin += dir * abs(model.d * understep);
    }
    return Hit(model, origin);
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
        Hit sh = march(shadowOrigin, lightDir, 1., 1.);
        if (sh.model.id == 0) {
            col += sunColor * specular;
        }
    }
    return col;
}

// origin sphere intersection
// returns entry and exit distances from ray origin
vec2 iSphere( in vec3 ro, in vec3 rd, float r )
{
	vec3 oc = ro;
	float b = dot( oc, rd );
	float c = dot( oc, oc ) - r*r;
	float h = b*b - c;
	if( h<0.0 ) return vec2(-1.0);
	h = sqrt(h);
	return vec2(-b-h, -b+h );
}

// main path tracing loop, based on yx's
// https://www.shadertoy.com/view/ts2cWm
// with a bit of demofox's
// https://www.shadertoy.com/view/WsBBR3
vec4 draw(vec2 fragCoord, int frame) {

    vec2 p = (-iResolution.xy + 2.* fragCoord) / iResolution.y;
    p /= 2.;
   
    vec2 seed = hash22(fragCoord + (float(frame)) * sqrt3);
    
    // jitter for antialiasing
    p += 2. * (seed - .5) / iResolution.xy;

    vec3 col = vec3(0);

    float focalLength = 6.;
    vec3 camPos = vec3(0,0,.4) * focalLength * 1.;
    vec3 camTar = vec3(0,-.02,0);
    
    vec3 ww = normalize(camTar - camPos);
    vec3 uu = normalize(cross(vec3(0,1,0),ww));
    vec3 vv = normalize(cross(ww,uu));
    mat3 camMat = mat3(-uu, vv, ww);
    
    vec3 rayDir = normalize(camMat * vec3(p.xy, focalLength));
    vec3 origin = camPos;

    origin = eye;
    rayDir = normalize(dir);

    #ifdef DOF
    float fpd = length(origin) - .13;//.385 * focalLength;
    vec3 fp = origin + rayDir * fpd;
    origin = origin + camMat * vec3(rndunit2(seed), 0.) * .01;
    rayDir = normalize(fp - origin);
    #endif

    Hit hit;
    vec3 nor, ref;
    Material material;
    vec3 throughput = vec3(1);
    vec3 bgCol = skyColor * .05;
    bool doSpecular = true;

    vec2 bound = iSphere(origin, rayDir, boundRadius);
    if (bound.x < 0.) {
    //	return vec4(bgCol, 1);
    }
    
    const int MAX_BOUNCE = 2;
    
    //origin += rayDir * bound.x;

    for (int bounce = 0; bounce < MAX_BOUNCE; bounce++) {

        hit = march(origin, rayDir, 100., 1.);
   
        if (hit.model.id == 0)
        {
            if (bounce > 0) {
                col += env(rayDir, doSpecular) * throughput;
            } else {
                col = bgCol;
            } 
            break;
        }

        nor = calcNormal(hit.pos);
        material = shadeModel(distance(camPos, hit.pos), hit.model, nor);

        // calculate whether we are going to do a diffuse or specular reflection ray 
        seed = hash22(seed);
        doSpecular = hash12(seed) < material.specular;
        
        if (bounce == 0) { // fix fireflies from diffuse-bounce specular
            col += sampleDirectSpec(hit, rayDir, nor, material.roughness) * throughput * material.specular;
        }

        bool doSSS = material.sss && bounce < 1 && ! doSpecular;
        if (doSSS) {
            seed = hash22(seed);
            doSSS = hash12(seed) < .9;
        }
        
        if ( ! doSpecular) {
            throughput *= material.albedo;
        }

        if (doSSS) {
            origin = hit.pos;
            
            seed = hash22(seed);
            hit = walkOnSpheres(origin, nor, .015, seed);
            nor = calcNormal(hit.pos);

            float extinctionDist = distance(origin, hit.pos) * 20.;
            vec3 extinctionCol = material.albedo;
            extinctionCol = mix(mix(extinctionCol, vec3(0,0,1), .25), vec3(1,0,0), clamp(1. - extinctionDist, 0., 1.));
            extinctionCol = vec3(1,0,0);
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

    return vec4(col, 1);
}

void mainImage(out vec4 fragColor, in vec2 fragCoord) {
    vec4 col = draw(fragCoord, iFrame);
   
    if (drawIndex > 0.) {
        vec4 lastCol = texture(previousSample, fragCoord.xy / iResolution.xy);
        col = mix(lastCol, col, 1. / (drawIndex + 1.));
    }
    
    fragColor = vec4(col.rgb, 1);
}
