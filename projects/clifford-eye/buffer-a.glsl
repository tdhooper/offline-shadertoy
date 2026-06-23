#version 300 es

// framexbuffer drawcount: 4

precision highp float;

uniform vec2 iResolution;
uniform mat4 cameraViewMatrix;
uniform sampler2D previousSample; // buffer-a.glsl filter: linear
uniform int drawIndex;
uniform int iFrame;
uniform float iTime;
uniform vec4 iMouse;

in vec3 eye;
in vec3 dir;
in float fov;
in float aspect;
in mat4 vView;

out vec4 fragColorOut;

void mainImage(out vec4 a, in vec2 b);

void main() {
    mainImage(fragColorOut, gl_FragCoord.xy);
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

// Fork of "Clifford Torus Rotation" by tdhooper. https://shadertoy.com/view/wsfGDS
// 2019-10-13 19:56:51

/*

    Clifford Torus Rotation
    -----------------------

    Getting a good distance for this 4D stereographic projection was
    tricky, see the notes in 'Main SDF', or just toggle DEBUG below to
    see what's going on.

    Big thanks to Matthew Arcus (mla) for providing a better torus
    equation and improving the projection distance fix.

    See also:

    * Animation by Jason Hise https://www.youtube.com/watch?v=1_pzjvVixL0
    * Clifford Torus by mla https://www.shadertoy.com/view/3ss3z4
    * https://en.wikipedia.org/wiki/Clifford_torus
    * http://virtualmathmuseum.org/Surface/clifford_torus/clifford_torus.html

*/

//#define DEBUG

// --------------------------------------------------------
// HG_SDF
// https://www.shadertoy.com/view/Xs3GRB
// --------------------------------------------------------

#define PI 3.14159265359

void pR(inout vec2 p, float a) {
    p = cos(a)*p + sin(a)*vec2(p.y, -p.x);
}

vec2 pMod2(inout vec2 p, vec2 size) {
    vec2 c = floor((p + size*0.5)/size);
    p = mod(p + size*0.5,size) - size*0.5;
    return c;
}

float smax(float a, float b, float r) {
    vec2 u = max(vec2(r + a,r + b), vec2(0));
    return min(-r, max (a, b)) + length(u);
}

float smin2(float a, float b, float r) {
    vec2 u = max(vec2(r - a,r - b), vec2(0));
    return max(r, min (a, b)) - length(u);
}

float smax2(float a, float b, float r) {
    vec2 u = max(vec2(r + a,r + b), vec2(0));
    return min(-r, max (a, b)) + length(u);
}

float smin(float a, float b, float k){
    float f = clamp(0.5 + 0.5 * ((a - b) / k), 0., 1.);
    return (1. - f) * a + f  * b - f * (1. - f) * k;
}

float fTorus(vec3 p, float smallRadius, float largeRadius) {
    return length(vec2(length(p.xz) - largeRadius, p.y)) - smallRadius;
}

// --------------------------------------------------------
// Spectrum colour palette
// IQ https://www.shadertoy.com/view/ll2GD3
// --------------------------------------------------------

vec3 pal( in float t, in vec3 a, in vec3 b, in vec3 c, in vec3 d ) {
    return a + b*cos( 6.28318*(c*t+d) );
}

vec3 spectrum(float n) {
    return pal( n, vec3(0.5,0.5,0.5),vec3(0.5,0.5,0.5),vec3(1.0,1.0,1.0),vec3(0.0,0.33,0.67) );
}

// --------------------------------------------------------
// Modelling Components
// --------------------------------------------------------

// Inverse stereographic projection of p,
// p4 lies onto the unit 3-sphere centered at 0.
// - mla https://www.shadertoy.com/view/lsGyzm
vec4 inverseStereographic(vec3 p, out float k) {
    k = 2.0/(1.0+dot(p,p));
    return vec4(k*p,k-1.0);
}

float fTorus(vec4 p4, out vec2 uv) {

    // Torus distance
    // We want the inside and outside to look the same, so use the
    // inverted outside for the inside.
    float d1 = length(p4.xy) / length(p4.zw) - 1.;
    float d2 = length(p4.zw) / length(p4.xy) - 1.;
    float d = d1 < 0. ? -d1 : d2;

    // Because of the projection, distances aren't lipschitz continuous,
    // so scale down the distance at the most warped point - the inside
    // edge of the torus such that it is 1:1 with the domain.
    d /= PI;

    // UV coordinates over the surface, from 0 - 1
    uv = (vec2(
        atan(p4.y, p4.x),
        atan(p4.z, p4.w)
    ) / PI) * .5 + .5;

    return d;
}

// Distances get warped by the stereographic projection, this applies
// some hacky adjustments which makes them lipschitz continuous.

// The numbers have been hand picked by comparing our 4D torus SDF to
// a usual 3D torus of the same size, see DEBUG.

// vec3 d
//   SDF to fix, this should be applied after the last step of
//   modelling on the torus.

// vec3 k
//   stereographic scale factor

float fixDistance(float d, float k) {
    float sn = sign(d);
    d = abs(d);
    d = d / k * 1.82;
    d += 1.;
    d = pow(d, .5);
    d -= 1.;
    d *= 5./3.;
    d *= sn;
    return d;
}

float spintime;
float time;
vec2 modelUv;
bool hitDebugTorus = false;

float warpSpace(inout vec3 p) {

    float k;
    vec4 p4 = inverseStereographic(p,k);

    // The inside-out rotation puts the torus at a different
    // orientation, so rotate to point it at back in the same
    // direction
   	pR(p4.zy, spintime * -PI / 2.);

    // Rotate in 4D, turning the torus inside-out
    pR(p4.xw, spintime * -PI / 2.);

    vec2 uv;
    float d = fTorus(p4, uv);
    modelUv = uv;

    #ifdef DEBUG
       // d = fixDistance(d, k);
       // return d;
    #endif

    // Magic number that makes z distance the same scale as xy distances
    d /= 2.25;
    
    // Recreate domain to be wrapped around the torus surface
    // xy = surface / face, z = depth / distance
    p = vec3(uv, d);
    
    return k;

}

float vmax(vec3 v) {
    return max(max(v.x, v.y), v.z);
}

float fBox(vec3 p, vec3 b) {
    vec3 d = abs(p) - b;
    return length(max(d, vec3(0))) + vmax(min(d, vec3(0)));
}

//#define FOO


//========================================================
// Modeling
//========================================================

struct Material {
    vec3 albedo;
    float specular;
    float roughness;
};

struct Model {
    float d;
    vec3 uvw;
    int id;
    float understep;
};

Material shadeModel(float rlen, Model model, inout vec3 nor) {
    return Material(vec3(.3), .02, .3);
}

Model map(vec3 p) {
    
    float d;
    
    //p.xy = mod(p.xy, 1.0);
    
    float db = length(p) - 2.;
    
    float k = 1.;

    //pR(p.xy, iTime * PI * -.25);
    //pR(p.yz, iTime * PI * -1.25);

    #ifndef FOO
    k = warpSpace(p);
    #endif
    
    
    float bound = fBox(p-vec3(.5), vec3(.5));
        
    
    // Draw some repeated circles

    float n = 4.*4.;
    float repeat = 1. / n;

    repeat = sqrt(2.) / n;
    pR(p.xy, PI/4.);
    
    p.xy += repeat / 2.;

    //p.xy += iTime * -.15;
    
    float aa = spintime * PI * 1.;
    aa += PI * .25;
   // p.xy += vec2(cos(aa), sin(aa)) * .25;
   // p.xy += repeat / 2.;

    vec2 c = floor((p.xy + repeat*0.5)/repeat);
    p.xy = mod(p.xy + repeat*0.5,repeat) - repeat*0.5;

    
    //p.z = mod(p.z + repeat/2., repeat) - repeat/2.;
    /*if (p.z > 0.) {
    	pR(p.xy, PI/2.);
    }
    p.z = abs(p.z);*/
    
    if (p.z < 0.) {
       p.x = -p.x;
    }

    
    p.z = abs(p.z);
    float hs = 2.7/repeat;
    d = smin(d, abs(p.z) - .005, .005);
    d = abs(p.z)- .005;
    d = smax(d, length(p.xy) - repeat * .3, .003);
    d = fBox(p, vec3(vec2(.3*repeat), 100.)) - .15 * repeat;
    d = mix(d, length(p.xy) - .45 * repeat, .5);
    d = smax(d, abs(p.z) - .005, .005);
    vec3 pp = p;
    p -= vec3(0,0,repeat * -.02);
    float d2 = length(p) - repeat * .3;
    d2 = smax(d2, min(
        dot(p, normalize(vec3(1,1,.8))),
        dot(p, -normalize(vec3(1,1,-.8)))
    ), .001);
    d = smin(d, d2, repeat * .1);
    d2 = length(p) - repeat * .26;
    pR(p.xy, PI/4.);
    pR(p.xz, -.2);
    d2 = smax(d2, -smax(-d2-1.002, length(p.xy) - repeat * .06, .001), .001);
    d2 = smax(d2, -abs(length(p.xy) - repeat * .13), .001);
    d = min(d, d2);
    p = pp;
    //d = smax(d, -(length(p.xy) - repeat * .05), .003);
    
    //d = length(p.xy) - repeat * .3;
    //d = smax(d, abs(p.z) - .01, .005);
    //d = length(p) - repeat * .4;
    
   // pR(p.xy, PI/-4.);
    
    
    if (mod(c.y, 2.) == 0.) {
       // p.x = abs(p.x);
	   // p.x -= repeat/2.;
    }
    
    
    if (mod(c.y, 2.) == 0.) {
	    p.z *= -1.;
    }

    
    float scale = 1.;
    p /= scale;
    
    //p.z = abs(p.z);
    //d = smin(d, mHead(p * 15.)/15., .01);
        
    //d = mHead(p * 20.)/20.;
   // d = max(d, -p.z);
   // d = smin(d, max(fBox(p, vec3(.025)) - .005, abs(p.z)-.002), .005);
    
    //d = min(d, length(p.yz) - .001);
    
    //p.x = abs(p.x);
    //p.x -= repeat/2.;
    //d = min(d, length(p) - .005);
    
    //d = length(p) - .01;
    /*
    d = mHead(p * 20.)/20.;
    pR(p.xy, PI/2.);
    p.z *= -1.;
    d = smin(d, mHead(p * 20.)/20., .004);
    d = max(d, length(p) - .04);
*/
    
    //d = length(p) - .03;
    
    #ifdef FOO
    d = max(d, bound);
    #endif
    
    d *= scale;
    
    #ifndef FOO
    d = fixDistance(d, k) * 2.;
	#endif
    
   // d = min(d, db);
    
    vec3 uvw = p;
    int id = 1;
    float understep = 1.;
    return Model(d, uvw, id, understep);
}



// --------------------------------------------------------
// Rendering - clifford
// --------------------------------------------------------


mat3 calcLookAtMatrix(vec3 ro, vec3 ta, vec3 up) {
    vec3 ww = normalize(ta - ro);
    vec3 uu = normalize(cross(ww,up));
    vec3 vv = normalize(cross(uu,ww));
    return mat3(uu, vv, ww);
}

#define TAU 6.2831853

vec3 sphereCam(in vec2 p){
    
    //return normalize(vec3(p, 2)); // Debug.
    
    // A more conventional way to spherize.
    //return vec3(sin(p.x)*cos(p.y), sin(p.y), cos(p.x)*cos(p.y));
  
    float t = 1./(1. + dot(p, p)/3.);
    return vec3(p*t, 2.*t - 1.);
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

float boundRadius = 100.;

vec3 sunPos = normalize(vec3(1,-1,.0)) * 100.;
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

Hit march(vec3 origin, vec3 rayDirection, float maxDist, float understep, float boundRadius) {

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

vec3 sampleDirect(Hit hit, vec3 nor, vec3 throughput, inout vec2 seed) {
    vec3 col = vec3(0);
    vec3 lightDir = (sunPos - hit.pos);
    vec3 lightSampleDir = getConeSample(lightDir, .0005, seed);
    seed = hash22(seed);
    float diffuse = dot(nor, lightSampleDir);
    vec3 shadowOrigin = hit.pos + nor * (.0002 / abs(dot(lightSampleDir, nor)));
    if (diffuse > 0.) {
        Hit sh = march(shadowOrigin, lightSampleDir, 1., 1., boundRadius);
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
        Hit sh = march(shadowOrigin, lightDir, 1., 1., boundRadius);
        if (sh.model.id == 0) {
            col += sunColor * specular;
        }
    }
    return col;
}

const float sqrt3 = 1.7320508075688772;

// main path tracing loop, based on yx's
// https://www.shadertoy.com/view/ts2cWm
// with a bit of demofox's
// https://www.shadertoy.com/view/WsBBR3
vec4 draw(vec2 fragCoord, int frame) {

    vec2 p = (-iResolution.xy + 2.* fragCoord) / iResolution.y;
   
    vec2 seed = hash22(fragCoord + (float(frame)) * sqrt3);
    
    // jitter for antialiasing
    p += 2. * (seed - .5) / iResolution.xy;

    vec3 col = vec3(0);






    time = fract(iTime / 24.);
    //time = .5 / 8.;
    //time = 1.5 / 8.;

    //time = mod(iTime / 3., 2.);
    spintime = time * 8.;
    #ifdef DEBUG
        time = iTime / 6.;
    #endif

    vec3 camPos = mix(vec3(-0.211, -0.631, 0.0), vec3(-0.473, -1.231, 0.0), pow((sin(time * PI * 4. + PI * 1.45) * .5 + .5), 2.));
    vec3 camTar = vec3(.1,0,.1);
    vec3 camUp = vec3(-1,0,-1.5);
    
    mat3 camMat = calcLookAtMatrix(camPos, camTar, camUp);

    float focalLength = 0.886;
    vec2 uv = p;

	//vec3 lookAt = u_camera_pos + u_camera_dir;  // "Look At" position.
    vec3 lookDir = vec3(1., 1.0, 0.0);
    
    //vec3 lookDir = vec3(1.0, 9.0, 0.0);
    //pR(lookDir.xz, u_time * .25);
    vec3 lookAt = camPos + lookDir;
    lookAt = vec3(0.0, 0.0, 0.0);

    // Using the above to produce the unit ray-direction vector.
    float FOV = TAU/6.; // FOV - Field of view.
    vec3 forward = normalize(lookAt - camPos);
    vec3 right = normalize(vec3(forward.z, 0, forward.x )); 
    vec3 up = cross(forward, right);

    // rd - Ray direction.
    //vec3 rd = normalize(uv.x*right + uv.y*up + forward/FOV );
    mat3 cam = mat3(right, up, forward);
    //vec3 rd = cam*normalize(vec3(uv, 1./FOV));
    vec3 rd = cam*sphereCam(uv*PI*.7/FOV);

    pR(rd.xz, time * PI * -2.0f);
    //pR(rd.yz, sin(time * PI * -4.0) * -.5);
    //pR(rd.yz, -1.);

    vec3 origin = camPos;
    vec3 rayDir = rd;

    //origin = eye;
    //rayDir = normalize(dir);



/*
    // TODO: Figure out accurate DOF for sphere camera
    #ifdef DOF

    // position on sensor plane
    vec3 cameraForward = -transpose(vView)[2].xyz;
    
    Hit dofHit = march(origin, cameraForward, 100., 1., 100.);
    float focalDistance = length(origin - dofHit.pos) - fov;

    origin = origin + rayDir / dot(rayDir, cameraForward) * fov;
    
    // position on focal plane
    //float focalDistance = length(origin);
    vec3 focalPlanePosition = origin + focalDistance * rayDir / dot(rayDir, cameraForward);
    origin = origin + vec3(rndunit2(seed), 0.) * mat3(vView) * .01;
    rayDir = normalize(focalPlanePosition - origin);

    #endif
*/





    Hit hit;
    vec3 nor, ref;
    Material material;
    vec3 throughput = vec3(1);
    vec3 bgCol = skyColor * .05;
    bool doSpecular = true;

    const int MAX_BOUNCE = 2;
    

    for (int bounce = 0; bounce < MAX_BOUNCE; bounce++) {

        hit = march(origin, rayDir, 100., 1., boundRadius);
   
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
        
        if ( ! doSpecular) {
            throughput *= material.albedo;
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
   
    if (drawIndex > 0) {
        vec4 lastCol = texture(previousSample, fragCoord.xy / iResolution.xy);
        col = mix(lastCol, col, 1. / float(drawIndex + 1));
    }
    
    fragColor = vec4(col.rgb, 1);
}
