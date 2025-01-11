// framebuffer drawcount: 1

precision highp float;

uniform vec2 iResolution;
uniform mat4 cameraViewMatrix;
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


void mainImage(out vec4 a, in vec2 b);

void main() {
    mainImage(gl_FragColor, gl_FragCoord.xy);
}

//#define ANIMATE
//#define DOF

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

// iq https://www.shadertoy.com/view/Xds3zN
float sdRoundCone( in vec3 p, in float r1, float r2, float h )
{
    vec2 q = vec2( length(p.xz), p.y );
    
    float b = (r1-r2)/h;
    float a = sqrt(1.0-b*b);
    float k = dot(q,vec2(-b,a));
    
    if( k < 0.0 ) return length(q) - r1;
    if( k > a*h ) return length(q-vec2(0.0,h)) - r2;
        
    return dot(q, vec2(a,b) ) - r1;
}

float sdRoundCone(vec3 p, vec3 dir, float r1, float r2, float h) {
    p = reflect(p, normalize(mix(vec3(0,1,0), -dir, .5)));
    return sdRoundCone(p, r1, r2, h);
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

vec2 closestHex(vec2 p) {
    p = cart2tri * p;
	vec2 pi = floor(p);
	vec2 pf = fract(p);
	vec2 nn = pick3(
        vec2(0, 0),
        vec2(1, 1),
        vec2(1, 0),
        pi.x + pi.y
    );
	vec2 hex = mix(nn.xy, nn.yx, step(pf.x, pf.y)) + pi;
    hex = tri2cart * hex;
    return hex;
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

vec3 geodesicTri(vec3 p, float subdivisions) {
	float uvScale = subdivisions / faceRadius;
    vec2 uv = icosahedronFaceCoordinates(p);
    uvScale /= 1.3333;
    vec2 closest = closestHex(uv * uvScale); 
    return faceToSphere(closest / uvScale);
}



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
    vec3 albedo;
    int id;
};

Material shadeModel(Model model, inout vec3 nor) {
    vec3 skin = pow(vec3(0.890,0.769,0.710), vec3(2.2));

    if (model.id == 1)
        return Material(skin, .025, .3, true);

    return Material(vec3(.02), .01, .2, false);
}

float sin3(vec3 x) {
    return sin(x.x) * sin(x.y) * sin(x.z);
}

float ball(vec3 p, float hs, float rep, float radius) {

    float o = sin3( (p + sin(p * 10. * .66)) * 100.) * .0001 * 2.;
    o += sin3( (p + cos(p * 18. * .66)) * 132.) * .00005 * 2.;

    float d = length(p) - (radius + .01 * hs);
    
    vec3 sp = sfold(p, .00005);
    p = fold(p);
    
    vec3 spp = p;
    
    vec3 point = geodesicTri(p, rep);

    p -= point * radius;
    sp -= point * radius;
    
    float d2 = length(sp) - .02 * hs;

    d = smin(d, d2, .05 * hs);
    d = smax(d, -d2, .03 * hs);
    
    d2 += .01 * hs;
    d2 = smin(d2, sdRoundCone(sp, normalize(point), .01 * hs, .000, .125 * hs * radius), .03 * hs);
    
    d = min(d, d2);
    
    d += o;
    
    return d;
}

const float boundRadius = .3;

Model map(vec3 p) {
    int id = 1;
    
    vec2 rot = vec2(.0);
    
    if (iMouse.x > 0.) {
        rot = (.5 - iMouse.yx / iResolution.yx + vec2(0,0)) * PI * vec2(1., 2.);
    }
    
    rot.x = -.45;
    rot.y = .85;
    
    pR(p.yz, rot.x);
    pR(p.xz, rot.y);

    float t = iTime * 3.;
    
    #ifndef ANIMATE
    t = 1.25;
    #endif
    
    float b = sin(dot(normalize(p), pbc) * 8. + t) * .5 + .5;
    float b2 = sin(dot(normalize(p), pbc) * 4. + t) * .5 + .5;
    
    float s = 1.2 - .1 * b2;
    p /= s;

    float d = ball(p, 3.1, 1.5, .175);
    float d2 = ball(p, 1.2, 2.5, .2);

    d = mix(d, d2, b);

    return Model(d * s, p, vec3(.5), id);
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


vec3 sunPos = normalize(vec3(-1,1,-.75)) * 100.;
vec3 skyColor = vec3(0.50,0.70,1.00);
vec3 sunColor = vec3(8.10,6.00,4.20) * 3.;


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

    for (int i = 0; i < 200; i++) {
        rayPosition = origin + rayDirection * rayLength;
        model = map(rayPosition);
        rayLength += model.d * understep;

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
            col += sunColor * 10. * specular;
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
    //p /= 2.;
   
    vec2 seed = hash22(fragCoord + (float(frame)) * sqrt3);
    
    // jitter for antialiasing
    p += 2. * (seed - .5) / iResolution.xy;

    vec3 col = vec3(0);

    float focalLength = 6.;
    vec3 camPos = vec3(0,0,.4) * focalLength * 1.;
    vec3 camTar = vec3(0);
    
    vec3 ww = normalize(camTar - camPos);
    vec3 uu = normalize(cross(vec3(0,1,0),ww));
    vec3 vv = normalize(cross(ww,uu));
    mat3 camMat = mat3(-uu, vv, ww);
    
    vec3 rayDir = normalize(camMat * vec3(p.xy, focalLength));
    vec3 origin = camPos;

    #ifdef DOF
    float fpd = .37 * focalLength;
    vec3 fp = origin + rayDir * fpd;
    origin = origin + camMat * vec3(rndunit2(seed), 0.) * .05;
    rayDir = normalize(fp - origin);
    #endif

    vec2 bound = iSphere(origin, rayDir, boundRadius);
    if (bound.x < 0.) {
    	return vec4(col, 1);
    }

    Hit hit;
    vec3 nor, ref;
    Material material;
    vec3 throughput = vec3(1);
    vec3 bgCol = skyColor;
    bool doSpecular = true;

    const int MAX_BOUNCE = 2;
    
    origin += rayDir * bound.x;

    for (int bounce = 0; bounce < MAX_BOUNCE; bounce++) {
   
        hit = march(origin, rayDir, 1., 1.);
   
        if (hit.model.id == 0)
        {
            if (bounce > 0)
                col += env(rayDir, doSpecular) * throughput;
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
            doSSS = hash12(seed) < .9;
        }
        
        if ( ! doSpecular) {
            // update the colorMultiplier
            throughput *= material.albedo;
        }

        if (doSSS) {
            origin = hit.pos;
            
            seed = hash22(seed);
            hit = walkOnSpheres(origin, nor, .075, seed);
            nor = calcNormal(hit.pos);

            float extinctionDist = distance(origin, hit.pos) * 10.;
            vec3 extinctionCol = material.albedo;
            extinctionCol = mix(mix(extinctionCol, vec3(0,0,1), .25), vec3(1,0,0), clamp(extinctionDist - 1., 0., 1.));
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

    return vec4(col, 1);
}

void mainImage(out vec4 fragColor, in vec2 fragCoord) {
    init();

    vec4 col = draw(fragCoord, iFrame);
       
    if (drawIndex > 0.) {
        vec4 lastCol = texture2D(previousSample, fragCoord.xy / iResolution.xy);
        col = mix(lastCol, col, 1. / (drawIndex + 1.));
    }
    
    fragColor = vec4(col.rgb, 1);
}
