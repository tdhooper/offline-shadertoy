// framebuffer drawcount: 128, tile: 4

precision highp float;

uniform vec2 iResolution;
uniform mat4 cameraViewMatrix;
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

#pragma glslify: inverse = require(glsl-inverse)

void mainImage(out vec4 a, in vec2 b);

void main() {
    mainImage(gl_FragColor, gl_FragCoord.xy);
}

const int MAX_BOUNCE = 6; // Try 6 if you have the power
const float UNDERSTEP = .5;
const float BOUNCE_UNDERSTEP = 1.;

//#define DEBUG

#define PI 3.14159265359

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

// Rotate around axis
// blackle https://suricrasia.online/demoscene/functions/
vec3 erot(vec3 p, vec3 ax, float ro) {
  return mix(dot(ax, p)*ax, p, cos(ro)) + cross(ax,p)*sin(ro);
}

void pR(inout vec2 p, float a) {
	p = cos(a)*p + sin(a)*vec2(p.y, -p.x);
}

// Spectrum palette, iq https://www.shadertoy.com/view/ll2GD3

vec3 pal( in float t, in vec3 a, in vec3 b, in vec3 c, in vec3 d ) {
    return a + b*cos( 6.28318*(c*t+d) );
}

vec3 spectrum(float n) {
    return pal( n, vec3(0.5,0.5,0.5),vec3(0.5,0.5,0.5),vec3(1.0,1.0,1.0),vec3(0.0,0.33,0.67) );
}


// Kleinian group distance estimator
// Jos Leys & Knighty https://www.shadertoy.com/view/XlVXzh

vec2 box_size = vec2(-0.40445, 0.34) * 2.;

//sphere inversion
bool SI=true;
vec3 InvCenter=vec3(0,1,1.);
float rad=.8;

vec2 wrap(vec2 x, vec2 a, vec2 s){
    x -= s; 
    return (x - a * floor(x / a)) + s;
}

void TransA(inout vec3 z, inout float DF, float a, float b){
	float iR = 1. / dot(z,z);
	z *= -iR;
	z.x = -b - z.x;
    z.y = a + z.y; 
	DF *= iR;//max(1.,iR);
}


float smin(float a, float b, float k){
    float f = clamp(0.5 + 0.5 * ((a - b) / k), 0., 1.);
    return (1. - f) * a + f  * b - f * (1. - f) * k;
}


mat3 rotationMatrix(vec3 axis, float angle)
{
    axis = normalize(axis);
    float s = sin(angle);
    float c = cos(angle);
    float oc = 1.0 - c;

    return mat3(
        oc * axis.x * axis.x + c,           oc * axis.x * axis.y - axis.z * s,  oc * axis.z * axis.x + axis.y * s,
        oc * axis.x * axis.y + axis.z * s,  oc * axis.y * axis.y + c,           oc * axis.y * axis.z - axis.x * s,
        oc * axis.z * axis.x - axis.y * s,  oc * axis.y * axis.z + axis.x * s,  oc * axis.z * axis.z + c
    );
}



//========================================================
// Modelling
//========================================================

float time;

struct Material {
    vec3 albedo;
    float specular;
    float roughness;
};

struct Model {
    float d;
    float understep;
    vec3 uvw;
    vec3 albedo;
    int id;
};

Material shadeModel(Model model, inout vec3 nor) {
    float spec = 0.;
    float rough = 0.;
    vec3 col = spectrum(clamp(model.albedo.y * 2., 0., 1.)) * mix(1., 3., smoothstep(0., .4, model.albedo.y));
    return Material(col, spec, rough);
}

float KleinR = 1.89;
float KleinI = .1;

vec3 apex;
vec3 apex2;

void calcApex() {
/*    if (SI) {
        z=z-InvCenter;
        d=length(z);
        d2=d*d;
        z=(rad*rad/d2)*z+InvCenter;
    }
*/
    float a = KleinR;
    float b = KleinI;
    float f = sign(b) * .45;   
    
    vec3 pt = vec3(0, a, 0);
    float f1 = 1.;
 
    float dd = 1e12;
    for (int i = 0; i < 100; i++) 
    {
        //if  (pt.y >= a * 0.5 + f *(2.*a-1.95)/4. * sign(pt.x + b * 0.5)* (1. - exp(-(7.2-(1.95-a)*15.)* abs(pt.x + b * 0.5))))	
        //{pt = vec3(-b, a, 0.) - pt;}
        TransA(pt, f1, a, b);
    }

    apex = pt;

    //apex.x = -.406;
    //apex.y = 1.0775;
    //apex.z = 0.;

    apex.z += box_size.y * 2.;
    
    apex2 = vec3(-b, a, 0) - apex * vec3(1,1,-1);
    
    apex2 = apex + vec3(0,0,.0001);
    
    if (SI) {
        apex -= InvCenter;
        float d = length(apex);
        float d2 = d * d;
        apex = (rad * rad / d2) * apex + InvCenter;
    }
    
    if (SI) {
        apex2 -= InvCenter;
        float d = length(apex2);
        float d2 = d * d;
        apex2 = (rad * rad / d2) * apex2 + InvCenter;
    }
}


Model map(vec3 p) {

   // pR(p.yz, iTime);

    p += vec3(-.86,1.16,1.76);
    
    float t = mod(time * 20., 1.);
    
   // t = 1. - t;
    
    t *= .72;
    
    t += 9.;
    
    //t *= 20.;
    
    float scl = pow(1.48, t);
    
    p -= apex;

    float bound = length(p) - .4;

    p = erot(p, normalize(apex2 - apex), t);
    p /= scl;
    p += apex;

   // float dd = abs(length(p - apex) - .05);
   // dd = min(dd, abs(length(p - apex2) - .05));

    // JosKleinian
    
    vec3 z = p;
    

    vec3 lz=z+vec3(1.), llz=z+vec3(-1.);
    float d=0.; float d2=0.;

    if (SI) {
        z=z-InvCenter;
        d=length(z);
        d2=d*d;
        z=(rad*rad/d2)*z+InvCenter;
    }

    vec3 orbitTrap = vec3(1e20);


    float a = KleinR;
    float b = KleinI;
    float f = sign(b) * .45;   
    
    vec3 pt = vec3(0, a, 0);
    float f1 = 1.;
    p = z;
   // p.z -= box_size.y * 2.;
   
    //if  (p.y <= a * 0.5 + f *(2.*a-1.95)/4. * sign(p.x + b * 0.5)* (1. - exp(-(7.2-(1.95-a)*15.)* abs(p.x + b * 0.5))))	
    //{p = vec3(-b,a,0) - p;}


    if (SI) {
        //dd = dd * d2 / (rad + d * dd);
    }
    
    float DE = 1e12;
    float DF = 1.;
    
    
    for (int i = 0; i < 80 ; i++) 
    {
        z.x += b / a * z.y;
        z.xz = wrap(z.xz, box_size * 2., -box_size);
        //z.z = wrap(z.xz, box_size * 2., -box_size).y;
        z.x -= b / a * z.y;

        //if (i > 1) {
        //If above the separation line, rotate by 180° about (-b/2, a/2)
        if  (z.y >= a * 0.5 + f *(2.*a-1.95)/4. * sign(z.x + b * 0.5)* (1. - exp(-(7.2-(1.95-a)*15.)* abs(z.x + b * 0.5))))	
        {z = vec3(-b, a, 0.) - z;}
        //}

        //Apply transformation a
        TransA(z, DF, a, b);

        //If the iterated points enters a 2-cycle , bail out.
        if(dot(z-llz,z-llz) < 1e-12) {break;}

        //Store prévious iterates
        llz=lz; lz=z;

        orbitTrap = min(orbitTrap, z);
    }
    

    float y =  min(z.y, a - z.y);
    DE = min(y, .3) / DF;
    
    if (SI) {
        DE = DE * d2 / (rad + d * DE);
    }

   // DE = min(DE, dd);
    //DE = dd;
    
    DE *= scl;

  //  DE = max(DE, bound);
    
    return Model(DE, .5, p, orbitTrap, 1);
}


//========================================================
// Rendering
//========================================================

//vec3 sunPos = normalize(vec3(-.1,1.5,-.5)) * 100.;
//vec3 sunPos = normalize(vec3(-.3,1.,-.5)) * 100.;
vec3 sunPos = normalize(vec3(-.35,.8,-.4)) * 100.;
vec3 skyColor = vec3(0.50,0.70,1.00);
vec3 sunColor = vec3(8.10,6.00,4.20) * 1.5;

vec3 env(vec3 dir) {
    vec3 col = mix(vec3(.5,.7,1) * .0, vec3(.5,.7,1) * 1., smoothstep(-.2, .2, dot(dir, normalize(vec3(.5,1.,-.5)))));
    return col;
}

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

    vec3 rayPosition;
    float rayLength, dist = 0.;
    Model model;

    for (int i = 0; i < 400; i++) {
        model = map(rayPosition);
        rayLength += model.d * understep;
        rayPosition = origin + rayDirection * rayLength;

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

vec3 sampleDirect(Hit hit, vec3 nor, vec3 throughput, inout vec2 seed) {
    vec3 col = vec3(0);
    vec3 lightDir = (sunPos - hit.pos);
    vec3 lightSampleDir = getConeSample(lightDir, .0005, seed);
    seed = hash22(seed);
    float diffuse = dot(nor, lightSampleDir);
    vec3 shadowOrigin = hit.pos + nor * (.0002 / abs(dot(lightSampleDir, nor)));
    if (diffuse > 0.) {
        Hit sh = march(shadowOrigin, lightSampleDir, 1., BOUNCE_UNDERSTEP);
        if (sh.model.id == 0) {
            col += throughput * sunColor/10. * diffuse;
        }
    }
    return col;
}

const float sqrt3 = 1.7320508075688772;

#define INSTA

// main path tracing loop, based on yx's
// https://www.shadertoy.com/view/ts2cWm
// with a bit of demofox's
// https://www.shadertoy.com/view/WsBBR3
vec4 draw(vec2 fragCoord, int frame) {
    
    //vec2 grid = floor(fragCoord / 10.);
    //grid = mod(grid, vec2(6.));
    //vec3 c = grid == vec2(drawIndex) ? spectrum(drawIndex / 6.) : vec3(0); 
    //return vec4(c, 1);

    vec2 p = (-iResolution.xy + 2.* fragCoord) / iResolution.y;
    
    #ifdef INSTA
        float ratio = 2160./2700.;
        float oldratio = 2560./1440.;

//        if (abs(p.x) > ratio) return vec4(0);

        p = vec2(p.y,-p.x) * oldratio;
    #endif

    vec2 seed = hash22(fragCoord + (float(frame)) * sqrt3);
    
    // jitter for antialiasing
    p += 2. * (seed - .5) / iResolution.xy;

    vec3 col = vec3(0);

    float focalLength = 3.;
    vec3 camPos = vec3(-.8346,-.1214,-.4026) * .3;
    vec3 camTar = vec3(0,-.02,-.03);
    vec3 up = normalize(vec3(1,.45,0));
    
    //camPos = vec3(-.5,.0,-2.); camTar = vec3(0,0,-box_size.y*2.); up = vec3(0,1,0);
    
    vec3 ww = normalize(camTar - camPos);
    vec3 uu = normalize(cross(up,ww));
    vec3 vv = normalize(cross(ww,uu));
    mat3 camMat = mat3(-uu, vv, ww);
    
    mat3 txm = rotationMatrix(normalize(vec3(-1,.1,-.75)), (time * PI * 2.));
    //mat3 txm = rotationMatrix(normalize(vec3(0,0,1)), -.5 * .125 * (iTime * PI * 2.));
    //mat3 txm = rotationMatrix(normalize(vec3(0,1,1)), .5 * .125 * (iTime * PI * 2.));
    
    //txm = mat3(1,0,0, 0,1,0, 0,0,1);
    
    camMat = inverse(txm) * camMat;
    camPos = camPos * txm;
    
    float rayLength = 0.;
    
    vec3 rayDir = normalize(camMat * vec3(p.xy, focalLength));
    vec3 origin = camPos;
    
    Hit hit;
    
    
    // Focus Stacking (with help from @Aliatraces)
    // Do an initial march with no circle of confusion but stop at the focal plane,
    // if we hit something, continue as normal,
    // if we don't hit anything, add a circle of confusion to the ray direction before continuing

    float focalPointDist = distance(camPos, vec3(0));
    float focalPlaneDist = dot(camMat[2], rayDir) * focalPointDist;
    
    /*
    hit = march(origin, rayDir, focalPlaneDist);
    if (hit.model.id == 0)
    {
        vec3 rayFocalPoint = origin + rayDir * focalPlaneDist;
        origin += camMat * vec3(rndunit2(seed), 0.) * .033;
        rayDir = normalize(rayFocalPoint - origin);
        origin = hit.pos;
        rayLength += hit.rayLength;
        hit = march(origin, rayDir, 2., UNDERSTEP);
    }
    /*/
    
    vec3 rayFocalPoint = origin + rayDir * focalPlaneDist;
    origin += camMat * vec3(rndunit2(seed), 0.) * .01;
    rayDir = normalize(rayFocalPoint - origin);
    hit = march(origin, rayDir, 30., UNDERSTEP);
        
    //*/


    #ifdef DEBUG
        if (hit.model.id == 0) {
            return vec4(0);
        } else {
            vec3 nor = calcNormal(hit.pos);
            //vec3 col = nor * .5 + .5;
            vec3 col = shadeModel(hit.model, nor).albedo;
            col *= dot(nor, normalize(vec3(0,1,-1))) * .5 + .5;
            float fog = 1. - exp((rayLength - .7) * -3.);
            col = mix(col, vec3(0), clamp(fog, 0., 1.)); 
            return vec4(col, 1.);
        }
    #endif
    

    vec3 nor, ref;
    Material material;
    vec3 throughput = vec3(1);
    vec3 bgCol = vec3(.01,.0,.015);
    
    for (int bounce = 0; bounce < MAX_BOUNCE; bounce++) {
   
        if (bounce > 0) {
            hit = march(origin, rayDir, 2., BOUNCE_UNDERSTEP);
        }
       
        if (hit.model.id == 0)
        {
            if (bounce > 0)
            {
                col += env(rayDir) * throughput;
            }
            if (bounce == 0)
            {
                col = bgCol;
            }
            break;
        }

        rayLength += hit.rayLength;

        nor = calcNormal(hit.pos);
        material = shadeModel(hit.model, nor);
        
        // update the colorMultiplier
        throughput *= material.albedo;

        // Calculate diffuse ray direction
        seed = hash22(seed);
        vec3 diffuseRayDir = getSampleBiased(nor, 1., seed);

        seed = hash22(seed);
        col += sampleDirect(hit, nor, throughput, seed);
        rayDir = diffuseRayDir;
    
        // offset from sufrace https://www.shadertoy.com/view/lsXGzH
        origin = hit.pos + nor * (.0002 / abs(dot(rayDir, nor)));
    }

    float fog = 1. - exp((rayLength - focalPointDist) * -1.);
    col = mix(col, bgCol, clamp(fog, 0., 1.)); 

    return vec4(col, 1);
}

void mainImage(out vec4 fragColor, in vec2 fragCoord) {

    calcApex();

    time = mod(iTime / 12. / 1., 1.);

    vec4 col = draw(fragCoord, iFrame);
    //vec4 col = vec4(1);
       
    if (drawIndex > 0.) {
        vec4 lastCol = texture2D(previousSample, fragCoord.xy / iResolution.xy);
        col = mix(lastCol, col, 1. / (drawIndex + 1.));
    }
    
    fragColor = vec4(col.rgb, 1);
}
