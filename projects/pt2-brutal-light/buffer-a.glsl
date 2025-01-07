#version 300 es

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

// Fork of "tdhpr-template-9" by tdhooper. https://shadertoy.com/view/sstXD7
// 2022-01-25 22:43:59

#define PI 3.14159265359

// HG_SDF
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


// https://iquilezles.org/articles/distfunctions/distfunctions.htm
float sdBoundingBox( vec3 p, vec3 b, float e )
{
       p = abs(p  )-b;
  vec3 q = abs(p+e)-e;
  return min(min(
      length(max(vec3(p.x,q.y,q.z),0.0))+min(max(p.x,max(q.y,q.z)),0.0),
      length(max(vec3(q.x,p.y,q.z),0.0))+min(max(q.x,max(p.y,q.z)),0.0)),
      length(max(vec3(q.x,q.y,p.z),0.0))+min(max(q.x,max(q.y,p.z)),0.0));
}

float vmax(vec3 v) {
	return max(max(v.x, v.y), v.z);
}

float vmax(vec2 v) {
	return max(v.x, v.y);
}

float vmin(vec3 v) {
	return min(min(v.x, v.y), v.z);
}


// Box: correct distance to corners
float fBox(vec3 p, vec3 b) {
	vec3 d = abs(p) - b;
	return length(max(d, vec3(0))) + vmax(min(d, vec3(0)));
}


// iq https://www.shadertoy.com/view/tl23Rm
vec2 rndunit2(inout vec2 seed) {
    vec2 h = seed * vec2(1,6.28318530718);
    float phi = h.y;
    float r = sqrt(h.x);
	return r*vec2(sin(phi),cos(phi));
}

#define PHI 1.618033988749895

vec3 boolSign(vec3 v) {
    return max(vec3(0), sign(v)) * 2. - 1.;
}

// Closest icosahedron vertex
vec3 icosahedronVertex(vec3 p) {
    vec3 ap = abs(p);
    vec3                           v = vec3(PHI,1,0);
    if (ap.x+ap.z*PHI > dot(ap,v)) v = vec3(1,0,PHI);
    if (ap.z+ap.y*PHI > dot(ap,v)) v = vec3(0,PHI,1);
    return v * 0.52573111 * boolSign(p);
}


// taken from: https://www.shadertoy.com/view/4sSSW3
// n is the z axis
mat3 basisZ(vec3 n) {
    vec3 f = vec3(0 , -1, 0);
    vec3 r = vec3(-1, 0, 0);
    if (n.z > -0.999999) {
    	float a = 1./(1. + n.z);
    	float b = -n.x*n.y*a;
    	f = vec3(1. - n.x*n.x*a, b, -n.x);
    	r = vec3(b, 1. - n.y*n.y*a , -n.y);
    }
    return mat3(f, r, n);
}

// n is the y axis
mat3 basisY(vec3 n) {
    mat3 b = basisZ(n);
    return mat3(b[0], b[2], b[1]);
}

mat3 basisY(vec3 n, vec3 up) {
    vec3 a = normalize(cross(n, up));
    vec3 b = normalize(cross(a, n));
    return mat3(a, n, b);
}


//========================================================
// Scene
//========================================================

vec3 lightPos = vec3(0,0,0);
float lightRadius = 2.;

vec3 skyColor = vec3(0.50,0.70,1.00);

const int BACKGROUND_ID = 0;
const int LIGHT_ID = 100;

vec3 env(vec3 dir) {
    //return vec3(0);
    //return pow(texture(iChannel1, dir).xyz, vec3(2.2)) * .1;
    vec3 col = mix(vec3(.5,.7,1) * .0, vec3(.5,.7,1) * .5, smoothstep(-.4, .4, dir.y));
    return col * .1;
}

struct Model {
    float d;
    vec3 albedo;
    vec3 emissive;
    float roughness;
    int id;
};

//#define PREVIEW


Model map(vec3 p) {
    vec3 pp = p;
    
    float d, dd;
    int id = 1;    
    vec3 albedo = vec3(1);
    vec3 emissive = vec3(0);
    float roughness = 1.;
    vec3 uvw;

    vec3 bs = vec3(1116,12,1116)/6.7;
    p.z = max(p.z, 0.);
    float box = fBox(p, bs);
    if ((-p - bs).y > -.011) {
        roughness = .2;
        albedo *= .2;
    }
    float bound = box;
    box = abs(box) - .01;
    p = pp;
    //box = max(box, -p.z - bs.z);
    d = box;
    d = 1e12;
    //d = p.y + bs.y;
    //albedo = vec3(1);


//    p = pp;
  //  dd = length(p - vec3(0,0,0));


    //if (bound < .001)
    //if (false)
    {
        float os = 3.6;
        p /= os;
        dd = 1e12;
        float s = 1.;

/*
        p -= vec3(1.4,-1.9,1.4);
        pR(p.xy, PI * -.2);
        pR(p.yz, PI * -.33);
        pR(p.xz, PI * -.2);
        */
        
        /*
        p -= vec3(-1.,-0.,1.9);
        pR(p.yz, PI * -.22);
        pR(p.xz, PI * -.22);
        pR(p.xy, PI * .05);
        */
        
        p -= vec3(-2.2,-.6,.6);
        vec3 albedo2;
        float roughness2;
        vec3 emissive2;

        for (int i = 0; i < 2; i++)
        {


            //if (mod(float(i), 3.) > 1.)
            //{
            float e = step(p.x, p.z);
            p.xz = e > 0. ? p.xz : p.zx;
            p = p.yzx;
            vec2 n = sign(p.xz) * .5 + .5;
            p.xz = abs(p.xz);
            p.y -= .9;
            //p.y = max(p.y, -.5);
            //}
            float sc = .8;
            s /= sc;
            p /= sc;
            
            //if (i < 1) continue;

            float c = floor(p.y + .5);
            float c2 = clamp(floor(p.z + .5), 1., 1.);
            c = clamp(c, -0., 1.);
            p.y -= c;
           // p.z -= c2;
            
            //if (i < 5) continue;
            
            //pR(p.xz, -.5);
            vec3 bss = vec3(.8,.3-(1.-c) * .2,.5+(1.-c) * .2);
            bss = vec3(.8,.3,.5);
            
            bool flip = c == 0.;
            
            float d2;
            
            if (!flip) {
                d2 = sdBoundingBox(p, bss, .05) - .01;
            } else {
                bss = vec3(.8,.1,.5);
                d2 = fBox(p - vec3(0,.15,0), bss) - .01;
            }
            //d2 = max(d2, -sdBoundingBox(p, bss + .01, .025));
            //d2 = fBox(p, vec3(1.,.3,1.5));
            //d2 = fBox(p, bss);
            //pR(p.xz, .5);

            vec3 col2 = vec3(.5);
//           int id2 = 1;

            if (p.y < 0. && i == 4 && c == 0.) {
                //col2 = vec3(0,1,1)*1.;
               // id2 = LIGHT_ID;
            }

            d2 /= s;

            if (d2 < dd) 
            {
                dd = d2;
                vec3 edge = step(abs(p), bss - vec3(.1, .275, .0));
                vec3 k = step(p, vec3(1.,.3,1.5) - .05);
    /*
                albedo2 = spectrum(edge * .5);
                if (edgeA > 0.) {
                    albedo2 = spectrum(.75);
                }
                */
                albedo2 = vec3(1.);
                emissive2 = vec3(0);
                roughness2 = 1.;
                
                
                if (!flip && i == 1 && fBox(p, bss - .09) <= 0.) {
                
                    emissive2 = vec3(1);
                }
                
                if (!flip) {
                    roughness2 = .3;
                }

                
                /*
                if (edge.z == 0.) {
                    albedo2 = vec3(1.000,0.616,0.180);
                    roughness2 = .01;
                }
                if (edge.y > 0.) {
                    albedo2 = vec3(1.000,0.616,0.180);
                    albedo2 *= 0.;
                    roughness2 = .01;
                    emissive2 = vec3(5);
                }
                if (edge.x == 0.) {
                    albedo2 = vec3(.1);
                    roughness2 = .1;
                }
                */

                //albedo2 = spectrum(e * .5 + c * .25);
  //              id = id2;
            }

        }
        //col = vec3(.5);
        dd *= os;
        
        //dd = max(dd, bound);
        
        if (dd < d) {
            d = dd;
            albedo = albedo2;
            roughness = roughness2;
            emissive = emissive2;
            //albedo = vec3(1);
            //emissive = vec3(0);
            //roughness = 1.;
        }
    }
    
    /*
    d = p.y + 2.;
    albedo = vec3(1);
    emissive = vec3(0);
    roughness = mix(.1, .9, mod(floor(p/2.).x - floor(p/2.).z, 3.));
    */

    p = pp;
    p -= lightPos;
    
    dd = length(p) - lightRadius;
    if (dd < d) {
        d = dd;
        id = LIGHT_ID;
        //emissive = lightColor * 10.;
        albedo = vec3(0);
        emissive = vec3(1) * 5.;
        float l = 3.;
        vec3 dr = normalize(vec3(1,1,0));
       // dr = vec3(0,1,0);
        float kk = dot(p * 2., dr) / lightRadius * .5 * l;
        kk = acos(dot(normalize(p), dr)) * 2.;
       // kk -= iTime;
        emissive = spectrum(floor(kk + .25) / l + .25) * 5.;

        //col = (normalize(p) * .5 + .5) * 10.;
        //emissive = mix(vec3(1,1,0), vec3(0,1,1), step(p.x,0.)) * 10.;
    }
    
    
    
    return Model(d, albedo, emissive, roughness, id);
}



//========================================================
// Marching
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

struct Hit {
    Model model;
    vec3 pos;
    float rayLength;
};

Hit march(vec3 origin, vec3 rayDirection, float maxDist) {
    vec3 rayPosition;
    float rayLength, dist = 0.;
    Model model;
    for (int i = 0; i < 500; i++) {
        rayPosition = origin + rayDirection * rayLength;
        model = map(rayPosition);
        rayLength += model.d;

        if (abs(model.d) < .0001) break;

        if (rayLength > maxDist) {
            model.id = BACKGROUND_ID;
            break;
        }
    }
    return Hit(model, rayPosition, rayLength);
}


//========================================================
// Noise
//========================================================

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

const float sqrt3 = 1.7320508075688772;



//========================================================
// Light sampling
//========================================================

// http://blog.hvidtfeldts.net/index.php/2015/01/path-tracing-3d-fractals/
vec3 ortho(vec3 a){
    vec3 b=cross(vec3(-1,-1,.5),a);
    // assume b is nonzero
    return (b);
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


//========================================================
// Path trace loop
//========================================================

// https://jcgt.org/published/0007/04/01/
vec3 sampleGGXVNDF(vec3 Ve, float alpha_x, float alpha_y, float U1, float U2)
{
	// Section 3.2: transforming the view direction to the hemisphere configuration
	vec3 Vh = normalize(vec3(alpha_x * Ve.x, alpha_y * Ve.y, Ve.z));
	// Section 4.1: orthonormal basis (with special case if cross product is zero)
	float lensq = Vh.x * Vh.x + Vh.y * Vh.y;
	vec3 T1 = lensq > 0. ? vec3(-Vh.y, Vh.x, 0) * inversesqrt(lensq) : vec3(1,0,0);
	vec3 T2 = cross(Vh, T1);
	// Section 4.2: parameterization of the projected area
	float r = sqrt(U1);	
	float phi = 2.0 * PI * U2;	
	float t1 = r * cos(phi);
	float t2 = r * sin(phi);
	float s = 0.5 * (1.0 + Vh.z);
	t2 = (1.0 - s)*sqrt(1.0 - t1*t1) + s*t2;
	// Section 4.3: reprojection onto hemisphere
	vec3 Nh = t1*T1 + t2*T2 + sqrt(max(0.0, 1.0 - t1*t1 - t2*t2))*Vh;
	// Section 3.4: transforming the normal back to the ellipsoid configuration
	vec3 Ne = normalize(vec3(alpha_x * Nh.x, alpha_y * Nh.y, max(0.0, Nh.z)));	
	return Ne;
}




vec3 sampleGGXVNDF(vec3 outDir, vec3 nor, float roughness, vec2 seed)
{
    mat3 trans = basisZ(nor);
    mat3 inv_trans = inverse( trans );
    float alpha = roughness * roughness;
    vec3 outDirLocal = inv_trans * outDir; //convert directions to local space
    vec3 wh = sampleGGXVNDF(outDirLocal, alpha, alpha, seed.x, seed.y);
    wh = trans * wh;
    //wh = normalize(wh + nor); // this stops us return samples over the whole sphere, but it does not match the bsdf
    vec3 inDir = reflect(-outDir, wh);
    return inDir;
}

// http://blog.hvidtfeldts.net/index.php/2015/01/path-tracing-3d-fractals/
vec3 getSampleBiased(vec3 dir, float power, inout vec2 seed) {
	dir = normalize(dir);
	vec3 o1 = normalize(ortho(dir));
	vec3 o2 = normalize(cross(dir, o1));
	vec2 r = seed;
	r.x=r.x*2.*PI;
	r.y=pow(r.y,1.0/(power+1.0));
	float oneminus = sqrt(1.0-r.y*r.y);
	return cos(r.x)*oneminus*o1+sin(r.x)*oneminus*o2+r.y*dir;
}

vec3 getSample(vec3 dir, inout vec2 seed) {
	return getSampleBiased(dir, 0., seed); // <- unbiased!
}

vec3 getCosineWeightedSample(vec3 dir, inout vec2 seed) {
	return getSampleBiased(dir, 1., seed);
}

float G(float dotNV, float k){
	return 1.0/(dotNV*(1.0f-k)+k);
}


// from http://filmicworlds.com/blog/optimizing-ggx-shaders-with-dotlh/
float ggx(vec3 N, vec3 V, vec3 L, float roughness, float F0){
	float alpha = roughness*roughness;

	vec3 H = normalize(V+L);

	float dotNL = clamp(dot(N,L),0.,1.);
	float dotNV = clamp(dot(N,V),0.,1.);
	float dotNH = clamp(dot(N,H),0.,1.);
	float dotLH = clamp(dot(L,H),0.,1.);

	float F, D, vis;

	float alphaSqr = alpha*alpha;
	float pi = 3.14159;
	float denom = dotNH * dotNH *(alphaSqr - 1.0) + 1.0;
	D = alphaSqr/(pi * denom * denom);

	float dotLH5 = pow(1.0 - dotLH, 5.0);
	F = F0 + (1.0 - F0)*(dotLH5);

	float k = alpha * 0.5;

	return dotNL * D * F * G(dotNL,k)*G(dotNV,k);
}


#define MEDIUMP_FLT_MAX    65504.0
#define saturateMediump(x) min(x, MEDIUMP_FLT_MAX)

float pdfMaterial(Model model, vec3 outDir, vec3 inDir, vec3 nor) {
    return saturateMediump(.5 / ggx(nor, -outDir, inDir, model.roughness, 1.));
    return .5 / dot(inDir, nor);
}

bool ACCEPT_UNDER_HEMISPHERE;



void sampleMaterial(Model model, vec3 outDir, vec3 nor, inout vec2 seed, out vec3 inDir, out float probability)
{   

    inDir = sampleGGXVNDF(-outDir, nor, model.roughness, seed);
    probability = 1.;

    if (!ACCEPT_UNDER_HEMISPHERE && dot(inDir, nor) < 0.) {
        probability = 1./.00001;
    }
    
    probability = saturateMediump(probability);

/*
    doSpecular = hash12(seed) < material.specular;

    seed = hash22(seed);
    vec3 diffuseRayDir = getCosineWeightedSample(nor, seed);

    if (doSpecular)
    {
        vec3 specularRayDir = reflect(outDir, nor);    
        inDir = normalize(mix(specularRayDir, diffuseRayDir, material.roughness));
        probability = 1.;
    }
    else
    {
        inDir = diffuseRayDir;
        probability = 1.;
    }
    */
}

/*
void sampleLight(inout vec2 seed, vec3 pos, vec3 lightPos, float lightRadius, out vec3 inDir, out float probability) {
    vec3 l = lightPos - pos;
    float lightDistance = length(l);
    vec3 lightDirection = l / lightDistance;
    float solidAngle = atan(lightRadius / lightDistance) * 2.;
    float extent = 1. - cos(solidAngle);
    inDir = getConeSample(lightDirection, extent, seed);
    probability = 1. / extent;
}
*/

float pdfLight(vec3 pos, vec3 lightPos, float lightRadius, vec3 inDir) {
    float lightDistance = length(lightPos - pos);
    float solidAngle = asin(lightRadius / lightDistance);
 
    vec3 lightDir = pos - lightPos;
    float b = dot(lightDir, inDir);
    float c = dot(lightDir, lightDir) - lightRadius * lightRadius;
    float h = b * b - c;
    if (h < 0.) return 0.;
    return 1.;
}

void sampleLight(inout vec2 seed, vec3 pos, vec3 lightPos, float lightRadius, out vec3 inDir, out float probability) {
    vec3 l = lightPos - pos;
    float lightDistance = length(l);
    float solidAngle = asin(lightRadius / lightDistance);
    
    float b = sqrt(lightDistance * lightDistance - lightRadius * lightRadius);
    float discDist = b * cos(solidAngle);
    float discSize = b * sin(solidAngle);
    
    vec3 dir = normalize(vec3(rndunit2(seed) * discSize, discDist));

    vec3 lightDirection = l / lightDistance;
    mat3 trans = basisZ(lightDirection);

    inDir = trans * dir;

    float extent = 1. - cos(solidAngle);
    probability = saturateMediump(1. / extent);
}

mat3 calcLookAtMatrix(vec3 ro, vec3 ta, vec3 up) {
    vec3 ww = normalize(ta - ro);
    vec3 uu = normalize(cross(ww,up));
    vec3 vv = normalize(cross(uu,ww));
    return mat3(uu, vv, ww);
}

float PowerHeuristic(float numf, float fPdf, float numg, float gPdf) {
    float f = numf * fPdf;
    float g = numg * gPdf;

    return (f * f) / (f * f + g * g);
}

vec3 newOrigin(vec3 pos, vec3 rayDir, vec3 nor) { 
    return pos + nor * (.0002 / abs(dot(rayDir, nor)));
}

vec3 estimateDirectLighting(Model model, vec3 origin, vec3 rayDir, vec3 nor, vec2 seed) {
    vec3 directLighting = vec3(0);

    vec3 lightInDir;
    float lightProbability;
    seed = hash22(seed);
    sampleLight(seed, origin, lightPos, lightRadius, lightInDir, lightProbability);
    float lightMaterialProbability = pdfMaterial(model, rayDir, lightInDir, nor);

    if (lightProbability != 0. && lightMaterialProbability != 0.) {
        Hit hit = march(newOrigin(origin, lightInDir, nor), lightInDir, 200.);
        
        if (hit.model.id == LIGHT_ID) {

            float lightWeight = PowerHeuristic(1., lightProbability, 1., lightMaterialProbability);
            directLighting += hit.model.emissive * lightWeight / lightProbability;
        }
    }


    vec3 materialInDir;
    float materialProbability;
    seed = hash22(seed);
    sampleMaterial(model, rayDir, nor, seed, materialInDir, materialProbability);
    float materialLightProbability = pdfLight(origin, lightPos, lightRadius, materialInDir);

    if (materialProbability > 0. && materialLightProbability > 0.)
    {
        Hit hit = march(newOrigin(origin, materialInDir, nor), materialInDir, 200.);
        
        if (hit.model.id == LIGHT_ID) {
    
            float materialWeight = PowerHeuristic(1., materialProbability, 1., materialLightProbability);
            directLighting += hit.model.emissive * materialWeight / materialProbability;

        }
    }


    return directLighting;
}

bool ESTIMATE_DIRECT;

vec4 draw(vec2 fragCoord, int frame) {
    vec2 p = (-iResolution.xy + 2. * fragCoord.xy) / iResolution.y;

    ACCEPT_UNDER_HEMISPHERE = p.x > 0.;
    ACCEPT_UNDER_HEMISPHERE = false;
    
    ESTIMATE_DIRECT = p.x > 0.;
    ESTIMATE_DIRECT = true;

    vec2 seed = hash22(fragCoord + (float(frame)) * sqrt3); 
    
    // jitter for antialiasing
    p += 2. * (seed - .5) / iResolution.xy;

    float focalLength = 5.;

    //vec3 camPos = vec3(-3,3,-3) * focalLength / 3.;
    vec3 camPos = vec3(16,12,-16) * focalLength / 3.;
    
    vec2 im = iMouse.xy / iResolution.xy;
    
    if (iMouse.x <= 0.)
    {
        im = vec2(.5,.5);
    }
    
//    pR(camPos.yz, (.5 - im.y) * PI / -1.);
//    pR(camPos.xz, (.5 - im.x) * PI * 2.5);
    //camPos.y = max(camPos.y, 0.01);
//    camPos = camPos; // what?
    mat3 camMat = calcLookAtMatrix(camPos, vec3(0,0,0), vec3(0,1,0));
    
    vec3 rayDir = normalize(camMat * vec3(p.xy, focalLength));
    vec3 origin = camPos;
    
    #if 0
    #ifndef PREVIEW
    float fpd = length(camPos) * 1.;
    vec3 fp = origin + rayDir * fpd;
    origin = origin + camMat * vec3(rndunit2(seed), 0.) * .3;
    rayDir = normalize(fp - origin);
    #endif
    #endif

    origin = eye;
    rayDir = normalize(dir);


    Hit hit;
    
    vec3 nor, ref;
    vec3 col = vec3(0);
    vec3 throughput = vec3(1);
    
    vec3 inDir;

    const int MAX_BOUNCE = 6;

    for (int bounce = 0; bounce < MAX_BOUNCE; bounce++) {
   
        hit = march(origin, rayDir, 20. * focalLength);

        if (hit.model.id == BACKGROUND_ID && bounce == 0)
        {
            
            break;
        }


        if (hit.model.id == BACKGROUND_ID)
        {
            col += env(rayDir) * throughput;
            break;
        }

        nor = calcNormal(hit.pos);

        #ifdef PREVIEW
            col += hit.model.albedo * env(nor);
            col += .01 * vec3(10) * hit.model.albedo * clamp(dot(nor, normalize(lightPos - hit.pos)), 0., 1. );
            col += hit.model.emissive;
            break;
        #endif

        
        if (ESTIMATE_DIRECT)
        {    
            if (hit.model.id != LIGHT_ID || bounce == 0) {
                col += throughput * hit.model.emissive;
            }

            col += throughput * estimateDirectLighting(hit.model, hit.pos, rayDir, nor, seed);
        }
        else
        {
            col += throughput * hit.model.emissive;        
        }
        
        vec3 materialInDir;
        float materialProbability;
        seed = hash22(seed);
        sampleMaterial(hit.model, rayDir, nor, seed, materialInDir, materialProbability);
        throughput *= hit.model.albedo / materialProbability;
        rayDir = materialInDir;
        
        //throughput *= 2.;

        // offset from sufrace https://www.shadertoy.com/view/lsXGzH
        origin = hit.pos + nor * (.0002 / abs(dot(rayDir, nor)));
    }
    


    return vec4(col, 1.);
}

void mainImage(out vec4 fragColor, in vec2 fragCoord) {
    vec4 col = draw(fragCoord, iFrame);
   
    if (drawIndex > 0.) {
        vec4 lastCol = texture(previousSample, fragCoord.xy / iResolution.xy);
        col = mix(lastCol, col, 1. / (drawIndex + 1.));
    }
    
    fragColor = vec4(col.rgb, 1);
}
