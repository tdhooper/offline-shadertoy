// framebuffer drawcount: 1

precision highp float;

uniform vec2 iResolution;
uniform mat4 cameraMatrix;
uniform sampler2D previousSample; // buffer-a.glsl filter: linear
uniform float drawIndex;
uniform int iFrame;
uniform float iTime;
uniform vec4 iMouse;

uniform float guiKleinR;
uniform float guiKleinI;
uniform float guiBoxSizeX;
uniform float guiBoxSizeZ;
uniform float guiOffsetX;
uniform float guiOffsetY;
uniform float guiOffsetZ;

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


#define PI 3.14159265359

vec3 pal( in float t, in vec3 a, in vec3 b, in vec3 c, in vec3 d ) {
    return a + b*cos( 6.28318*(c*t+d) );
}

vec3 spectrum(float n) {
    return pal(
        n,
        vec3(0.5,0.5,0.5),
        vec3(0.5,0.5,0.5),
        vec3(1.0,1.0,1.0),
        vec3(0.0,0.33,0.67)
    );
}

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

float vmax(vec2 v) {
	return max(v.x, v.y);
}

float vmin(vec2 v) {
	return min(v.x, v.y);
}

float vmax(vec3 v) {
	return max(max(v.x, v.y), v.z);
}

float vmin(vec3 v) {
	return min(min(v.x, v.y), v.z);
}

float fBox(vec2 p, vec2 b) {
	vec2 d = abs(p) - b;
	return length(max(d, vec2(0))) + vmax(min(d, vec2(0)));
}

float fBox(vec3 p, vec3 b) {
	vec3 d = abs(p) - b;
	return length(max(d, vec3(0))) + vmax(min(d, vec3(0)));
}

float time;

float spaceAnimFreq = .1;
float startScale = 1.5;

vec3 animAmp = vec3(-.05,.05,.05) * .5;
 
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


    float blend = smoothstep(-1.5, -0., model.albedo.z);

    
    
    blend = smoothstep(-.1, .3, model.albedo.y);

    //blend = step(model.albedo.y, .005);

    //blend = smoothstep(-1.75, -.0, model.albedo.x);
    skin += mix(vec3(-.6,.0,.15) * .5, vec3(.4,-.03,-.05), blend);

    skin *= vec3(1.1,.8,.7);

    skin = clamp(skin, vec3(0,0,0), vec3(1,1,1));

    //skin += vec3(.1,-.05,-.05);
   // return Material(vec3(.8), .0, .0, false);

    if (model.id == 1) {
        //return Material(skin, .15, .3, true);
    }

    blend = smoothstep(-.1, .6, model.albedo.y);

    float spec = 0.;
    float rough = 0.;


    vec3 col = vec3(.0);

    if (blend > .5) {
        //spec = 1.;
        //rough = 1.;
        //col = vec3(10);
    }

    col = mix(col, vec3(5), blend);

    col = spectrum(model.albedo.y * 2.) * mix(1., 3., smoothstep(0., .4, model.albedo.y));
    
    //blend = smoothstep(.0, -3., model.albedo.x);


   
    return Material(col, spec, rough, false);
}


float box_size_x=guiBoxSizeX * 2.;
float box_size_z=guiBoxSizeZ * 2.;
vec3 light=vec3(50,10,-50);
float slice_start=-1.;
float slice_end=1.;
float fudge_factor=0.5;


//sphere inversion
bool SI=true;
vec3 InvCenter=vec3(guiOffsetX,guiOffsetY,guiOffsetZ);
float rad=.8;

vec2 wrap(vec2 x, vec2 a, vec2 s){
	x -= s; 
	return (x-a*floor(x/a)) + s;
}

void TransA(inout vec3 z, inout float DF, float a, float b){
	float iR = 1. / dot(z,z);
	z *= -iR;
	z.x = -b - z.x; z.y = a + z.y; 
	DF *= iR;//max(1.,iR);
}

// https://www.shadertoy.com/view/XlVXzh
vec4 JosKleinian(vec3 z)
{
    float t = 0.;

    float KleinR = 1.5 + guiKleinR;
   float KleinI = (guiKleinI * 2. - 1.);
	vec3 lz=z+vec3(1.), llz=z+vec3(-1.);
    float d=0.; float d2=0.;
    
    if(SI) {
             z=z-InvCenter;
		d=length(z);
		d2=d*d;
		z=(rad*rad/d2)*z+InvCenter;
            }

    vec3 orbitTrap = vec3(1e20);

	float DE=1e10;
	float DF = 1.0;
	float a = KleinR;
    float b = KleinI;
	float f = sign(b)*1. ;     
	for (int i = 0; i < 100 ; i++) 
	{
		z.x=z.x+b/a*z.y;
		z.xz = wrap(z.xz, vec2(2. * box_size_x, 2. * box_size_z), vec2(- box_size_x, - box_size_z));
		z.x=z.x-b/a*z.y;
               
		//If above the separation line, rotate by 180° about (-b/2, a/2)
        if  (z.y >= a * 0.5 + f *(2.*a-1.95)/4. * sign(z.x + b * 0.5)* (1. - exp(-(7.2-(1.95-a)*15.)* abs(z.x + b * 0.5))))	
        {z = vec3(-b, a, 0.) - z;}
        
		//Apply transformation a
		TransA(z, DF, a, b);
		
		//If the iterated points enters a 2-cycle , bail out.
        if(dot(z-llz,z-llz) < 1e-5) {break;}
		
		//Store prévious iterates
		llz=lz; lz=z;

        orbitTrap = min(orbitTrap, z);
	}
	
	
	float y =  min(z.y, a-z.y) ;
	DE=min(DE,min(y,0.3)/max(DF,2.));
      if (SI) {DE=DE*d2/(rad+d*DE);}

	return vec4(DE, orbitTrap);
}



Model map(vec3 p) {

    //float dd = length(p) - .5;

    float s = 1.;

    p /= s;

    //vec2 res = PseudoKleinian(p);
    //float d = res.x;
    //float orbitTrap = res.y;
    
    float dd = length(p) - 2.5;

    vec3 center = vec3(-.79,.89,1.5);

    center = vec3(-.86,1.16,1.76);

    p += center;

    vec4 res = JosKleinian(p);
    float d = res.x;
    vec3 orbitTrap = res.yzw;

    d = max(d, dd);

    d *= s;

    //d = min(d, dd);

    return Model(d, p, orbitTrap, 1);

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


vec3 sunPos = normalize(vec3(-.1,1.5,-.5)) * 100.;
vec3 skyColor = vec3(0.50,0.70,1.00);
vec3 sunColor = vec3(8.10,6.00,4.20) * 1.5;


vec3 env(vec3 dir, bool includeSun) {
    vec3 col = mix(vec3(.5,.7,1) * .0, vec3(.5,.7,1) * 1., smoothstep(-.2, .2, dot(dir, normalize(vec3(.5,1.,-.5)))));
    return col;
}

struct Hit {
    Model model;
    vec3 pos;
    float rayLength;
};

Hit march(vec3 origin, vec3 rayDirection, float maxDist, float understep) {

    vec3 rayPosition;
    float rayLength, dist = 0.;
    Model model;

    for (int i = 0; i < 400; i++) {
        rayPosition = origin + rayDirection * rayLength;
        model = map(rayPosition);
        rayLength += model.d * understep;

        if (model.d < .00002) break;

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

    vec3 initialOrigin = origin;
    
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
    return Hit(model, origin, distance(initialOrigin, origin));
}

vec3 sampleDirect(Hit hit, vec3 nor, vec3 throughput, inout vec2 seed) {
    vec3 col = vec3(0);
    vec3 lightDir = (sunPos - hit.pos);
    vec3 lightSampleDir = getConeSample(lightDir, .0005, seed);
    //lightSampleDir = normalize(lightDir);
    seed = hash22(seed);
    float diffuse = dot(nor, lightSampleDir);
    vec3 shadowOrigin = hit.pos + nor * (.0002 / abs(dot(lightSampleDir, nor)));
    if (diffuse > 0.) {
        Hit sh = march(shadowOrigin, lightSampleDir, 1., .5);
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
        Hit sh = march(shadowOrigin, lightDir, 1., .5);
        if (sh.model.id == 0) {
            col += sunColor * specular * .1;
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

const float sqrt3 = 1.7320508075688772;

// main path tracing loop, based on yx's
// https://www.shadertoy.com/view/ts2cWm
// with a bit of demofox's
// https://www.shadertoy.com/view/WsBBR3
vec4 draw(vec2 fragCoord, int frame) {

    time = fract(iTime / 4.);

    vec2 p = (-iResolution.xy + 2.* fragCoord) / iResolution.y;
    
    p *= .85;

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
    

    //vec3 rayDir = normalize(camMat * vec3(p.xy, focalLength));
    //vec3 origin = camPos;

    vec3 origin = eye;
    vec3 rayDir = normalize(vec3(p.x * fov, p.y * fov, -1.) * mat3(vView));

    float fpd = .88;
    #if 1
    vec3 fp = origin + rayDir * fpd;
    origin = origin + vec3(rndunit2(seed), 0.) * mat3(vView) * .02;
    //origin = origin + camMat * vec3(rndunit2(seed), 0.) * .05;
    rayDir = normalize(fp - origin);
    #endif

    Hit hit;
    vec3 nor, ref;
    Material material;
    vec3 throughput = vec3(1);
    vec3 bgCol = skyColor * .075 * 0.;
    bool doSpecular = true;
    float rayLength = 0.;

    const int MAX_BOUNCE = 6;
    
    for (int bounce = 0; bounce < MAX_BOUNCE; bounce++) {
   
        hit = march(origin, rayDir, 3., .333);
   
        if (hit.model.id == 0)
        {
            if (bounce > 0 && ! doSpecular)
                col += env(rayDir, doSpecular) * throughput;
            if (bounce == 0) {
                col = bgCol;
            }
            break;
        }

        rayLength += hit.rayLength;

        nor = calcNormal(hit.pos);
        material = shadeModel(hit.model, nor);

        // calculate whether we are going to do a diffuse or specular reflection ray 
        seed = hash22(seed);
        doSpecular = hash12(seed) < material.specular;
        
        bool doSSS = material.sss && bounce < 1 && ! doSpecular;
        if (doSSS) {
            seed = hash22(seed);
            doSSS = hash12(seed) < .5;
        }
        
        if ( ! doSpecular) {
            // update the colorMultiplier
            throughput *= material.albedo;
        }

        if (doSSS) {
            origin = hit.pos;
            
            seed = hash22(seed);
            hit = walkOnSpheres(origin, nor, .25, seed);
            nor = calcNormal(hit.pos);

            float extinctionDist = distance(origin, hit.pos) * 10.;
            vec3 extinctionCol = material.albedo;
            extinctionCol = mix(mix(extinctionCol, vec3(0,0,1), .5), vec3(1,0,0), clamp(extinctionDist - 1., 0., 1.));
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

    float fog = 1. - exp((rayLength - fpd) * -1.);
    col = mix(col, bgCol, clamp(fog, 0., 1.)); 

    return vec4(col, 1);
}

void mainImage(out vec4 fragColor, in vec2 fragCoord) {
    vec4 col = draw(fragCoord, iFrame);
       
    if (drawIndex > 0.) {
        vec4 lastCol = texture2D(previousSample, fragCoord.xy / iResolution.xy);
        col = mix(lastCol, col, 1. / (drawIndex + 1.));
    }
    
    fragColor = vec4(col.rgb, 1);
}
