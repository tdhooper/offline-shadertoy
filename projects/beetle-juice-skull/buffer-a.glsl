// framebuffer drawcount: 20, tile: 1

precision highp float;

uniform vec2 iResolution;
uniform vec2 iOffset;
uniform float iGlobalTime;
uniform vec4 iMouse;
uniform int iFrame;

uniform sampler2D previousSample; // buffer-a.glsl filter: linear
uniform float drawIndex;

uniform sampler2D volumeData; // volume-generate.glsl filter: linear wrap: clamp
uniform vec2 volumeDataSize;

uniform sampler2D iChannel0; // images/blue-noise.png filter: linear wrap: clamp
uniform vec2 iChannel0Size;

void mainImage(out vec4 a, in vec2 b);

void main() {
    mainImage(gl_FragColor, gl_FragCoord.xy + iOffset.xy);
}

#ifdef GL_ES
precision mediump float;
#endif


/* SHADERTOY FROM HERE */

#pragma glslify: mapTex = require(./volume-read.glsl)
#pragma glslify: transpose = require(glsl-transpose)


//#define WOBBLE

//#define DARK_MODE


#if 0
    const float MAX_DISPERSE = 5.; // 20
    const float MAX_BOUNCE = 5.; // 10
#else
    const float MAX_DISPERSE = 4.;
    const float MAX_BOUNCE = 4.;
#endif


#define PI 3.14159265359
#define PHI 1.618033988749895


// HG_SDF
// https://www.shadertoy.com/view/Xs3GRB

#define PI 3.14159265359
#define TAU 6.28318530718

#define saturate(x) clamp(x, 0., 1.)

void pR(inout vec2 p, float a) {
    p = cos(a)*p + sin(a)*vec2(p.y, -p.x);
}

float smax(float a, float b, float r) {
    vec2 u = max(vec2(r + a,r + b), vec2(0));
    return min(-r, max (a, b)) + length(u);
}

float vmax(vec2 v) {
	return max(v.x, v.y);
}

float vmax(vec3 v) {
	return max(max(v.x, v.y), v.z);
}

float fBox(vec2 p, vec2 b) {
	vec2 d = abs(p) - b;
	return length(max(d, vec2(0))) + vmax(min(d, vec2(0)));
}

float fBox(vec3 p, vec3 b) {
	vec3 d = abs(p) - b;
	return length(max(d, vec3(0))) + vmax(min(d, vec3(0)));
}


// Spectrum palette
// IQ https://www.shadertoy.com/view/ll2GD3

vec3 pal( in float t, in vec3 a, in vec3 b, in vec3 c, in vec3 d ) {
    return a + b*cos( 6.28318*(c*t+d) );
}

vec3 spectrum(float n) {
    return pal( n, vec3(0.5,0.5,0.5),vec3(0.5,0.5,0.5),vec3(1.0,1.0,1.0),vec3(0.0,0.33,0.67) );
}



//========================================================
// Matrix
//========================================================


mat3 rotX(float a) {
	return mat3(1, 0, 0, 0, cos(a), -sin(a), 0, sin(a), cos(a));
}

mat3 rotY(float a) {
	return mat3(cos(a), 0, sin(a), 0, 1, 0, -sin(a), 0, cos(a));
}

mat3 rotZ(float a) {
	return mat3(cos(a), -sin(a), 0, sin(a), cos(a), 0, 0, 0, 1);
}





//========================================================
// Skull
//========================================================


float smin(float a, float b, float k){
    float f = clamp(0.5 + 0.5 * ((a - b) / k), 0., 1.);
    return (1. - f) * a + f  * b - f * (1. - f) * k;
}

/*
float sdSkull(vec3 p) {
    // return length(p - .2);
    #ifdef MIRROR
        p.x = -abs(p.x);
    #endif
    p += OFFSET / SCALE;
    float bound = fBox(p, 1./SCALE);
    if ( ! lightingPass && bound > .01) {
        return bound;
    }
    // bound = max(bound, vmax(abs(mod(p + .0125, .025) - .0125)) - .003);
    p *= SCALE;
    float d = mapTex(volumeData, p, volumeDataSize);
    if (lightingPass && bound > .01) {
        return d + bound;
    }
    return d;
}
*/

float sdSkull(vec3 p) {

    p.y += .15;
    p.z -= .1;
    float rad = .3;
    float bound = fBox(p - vec3(0,-.13,-.02), vec3(.45,.43,.54) - rad) - rad;
    bound = smin(bound, fBox(p - vec3(0,.29,-.4), vec3(.25,.2,.18)), .2);
    if (bound > .01) {
        return bound;
    }

    p.x = -abs(p.x);
    p += OFFSET / SCALE;
    p *= SCALE;
    float d = mapTex(volumeData, p, volumeDataSize);
	return d;
}


//========================================================
// Modelling
//========================================================


float time;

vec2 map(vec3 p) {

    p.x -= .075;
    //p.y -= .05;

    float scl = .9;

    if (iMouse.z > 0.) {
        pR(p.yz, (.5 - iMouse.y / iResolution.y) * PI / 2.);
        pR(p.xz, (.5 - iMouse.x / iResolution.x) * PI * 2.);
        //scl *= 1.75;
    } else {
        pR(p.yz, (-.3) * PI / 2.);
        pR(p.xz, (-.075) * PI * 2.);
    }

    p /= scl;
    
    pR(p.yz, sin(time * PI * 2. - PI/ 2.) * .033);


    p.y += .075;

    float wrp = smoothstep(.8, .0, -dot(p, normalize(vec3(0,1,-1))));
    //float wrp2 = smoothstep(2., .0, -dot(p, normalize(vec3(0,1,-1))));
    wrp = mix(wrp, 1., 0.0);
    //wrp = .5;

    mat3 trs = rotZ(PI * .25) * rotY(PI * .25);
    p *= transpose(trs);

    float sc2 = 2.5;
    float sc = .4;
    p += (sin(p.x * 15. * sc + (-time) * PI * 2.) * sin(p.y * 10. * sc + .5) * sin(p.z * 10. * sc + .5)) * .4 * mix(.02, 1., wrp);

    //p += sin(p.y * 50. - time * PI * 2. * 0.) * .03 * mix(.2, 1., wrp);

    p += sin(sin(p * 5. + 7.) * 8. - time * PI * 4.) * .033 * mix(.2, 1., wrp);
    p += (sin(p.z * 2. * sc2  ) * sin(p.x * 2. * sc2 ) * sin(p.y * 20. * sc2 - 0. * PI * 2.)) * .02 * mix(.2, 1., wrp);
       
    //p += (sin(p.z * 10. * sc2  ) * sin(p.x * 10. * sc2 ) * sin(p.y * 10. * sc2 - time * PI * 2.)) * .03 * mix(.1, 1., wrp);


    //p *= transpose(trs);
    p *= trs;
    //pR(p.xy, -PI/4.);
    //pR(p.xz, -PI/4.);


    float skscl = 2.5;
    p /= skscl;
    p = -p.xyz;
    p.y -= .15;
    float skull = sdSkull(p) * skscl;
    //skull = max(skull, -(skull + .01));
    //return vec2(skull, 1.);

    float d = skull;
    d = mix(d, max(d, -(d + .01)), wrp);
    
    d *= scl;
    
    return vec2(d, 1);
}


//========================================================
// Lighting
//========================================================

vec3 BGCOL = vec3(1.000,1.000,1.000) * 1.;

float intersectPlane(vec3 rOrigin, vec3 rayDir, vec3 origin, vec3 normal, vec3 up, out vec2 uv) {
    float d = dot(normal, (origin - rOrigin)) / dot(rayDir, normal);
  	vec3 point = rOrigin + d * rayDir;
	vec3 tangent = cross(normal, up);
	vec3 bitangent = cross(normal, tangent);
    point -= origin;
    uv = vec2(dot(tangent, point), dot(bitangent, point));
    return max(sign(d), 0.);
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
    float l = smoothstep(.75, .0, fBox(uv, vec2(.5,2)) - 1.);
    l *= smoothstep(6., 0., length(uv));
	return vec3(l) * hit;
}

vec3 env(vec3 origin, vec3 rayDir) {    
    origin = -(vec4(origin, 1)).xyz;
    rayDir = -(vec4(rayDir, 0)).xyz;

    origin *= envOrientation;
    rayDir *= envOrientation;

    float l = smoothstep(.0, 1.7, dot(rayDir, vec3(.5,-.3,1))) * .4;
   	return vec3(l) * BGCOL;
}



//========================================================
// Marching
//========================================================

#define ZERO (min(iFrame,0))


vec3 calcNormal( in vec3 p ) // for function f(p)
{
    const float eps = 0.0001; // or some other value
    const vec2 h = vec2(eps,0);
    return normalize( vec3(map(p+h.xyy).x - map(p-h.xyy).x,
                           map(p+h.yxy).x - map(p-h.yxy).x,
                           map(p+h.yyx).x - map(p-h.yyx).x ) );
}


struct Hit {
    vec2 res;
    vec3 p;
    float len;
    float steps;
};

float pixelRadius = .0003;

Hit march(vec3 origin, vec3 rayDir, float invert, float maxDist, float understep) {

#if 0    
   // understep *= .2;
    
    float omega = 1.2;
    float len = 0.; // t
    float candidateError = 1e12;
    vec2 candidateRes = vec2(0); // candidate_t
    vec3 candidateP = origin;
    float previousRadius = 0.;
    float stepLength = 0.;
    vec3 p;

    for (int i = 0; i < 1500; ++i) {
        p = rayDir * len + origin;
        vec2 res = map(p);
        float signedRadius = res.x * invert;
        float radius = abs(signedRadius);

        bool sorFail = omega > 1. && (radius + previousRadius) < stepLength;
        if (sorFail) {
            stepLength -= omega * stepLength * understep;
            omega = 1.;
        } else {
            stepLength = signedRadius * omega * understep;
        }

        previousRadius = radius;
        float error = radius / len;

        if ( ! sorFail && error < candidateError) {
            candidateRes = res;
            candidateP = p;
            candidateError = error;
        }
        
        if ( ! sorFail && error < pixelRadius || len > maxDist) {
            if (len > maxDist) candidateRes.y = 0.;
            break;
        }
        
        len += stepLength;
    }
    
    //if ( (len > maxDist || candidate_error > pixelRadius) && ! forceHit) {
    //    return INFINITY;
    //}

    return Hit(candidateRes, candidateP, len, 0.);
#else

    vec3 p;
    float len = 0.;
    float dist = 0.;
    vec2 res = vec2(0.);
    vec2 candidate = vec2(0.);
    float steps = 0.;
    
   // understep *= .2;

    for (float i = 0.; i < 1500.; i++) {
        len += dist * understep;
        p = origin + len * rayDir;
        candidate = map(p);
        dist = candidate.x * invert;
        steps += 1.;
        res = candidate;
        if (dist < .001) {
            break;
        }
        if (len >= maxDist) {
            len = maxDist;
            res.y = 0.;
            break;
        }
    }   

    return Hit(res, p, len, steps);
#endif
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

mat3 calcLookAtMatrix(vec3 ro, vec3 ta, vec3 up) {
    vec3 ww = normalize(ta - ro);
    vec3 uu = normalize(cross(ww,up));
    vec3 vv = normalize(cross(uu,ww));
    return mat3(uu, vv, ww);
}

float range(float vmin, float vmax, float value) {
  return clamp((value - vmin) / (vmax - vmin), 0., 1.);
}

const float sqrt3 = 1.7320508075688772;

// Dave_Hoskins https://www.shadertoy.com/view/4djSRW
vec2 hash22(vec2 p)
{
    p += 1.61803398875; // fix artifacts when reseeding
	vec3 p3 = fract(vec3(p.xyx) * vec3(.1031, .1030, .0973));
    p3 += dot(p3, p3.yzx+33.33);
    return fract((p3.xx+p3.yz)*p3.zy);
}

vec4 draw(vec2 fragCoord, int frame)
{

    vec2 seed = hash22(fragCoord + (float(frame)) * sqrt3);

    #ifndef DARK_MODE
        envOrientation = sphericalMatrix(((vec2(81.5, 119) / vec2(187)) * 2. - 1.) * 2.);
    #else
        envOrientation = sphericalMatrix((vec2(0.7299465240641712,0.3048128342245989) * 2. - 1.) * 2.);
    #endif

    vec2 uv = (2. * fragCoord - iResolution.xy) / iResolution.y;

    // jitter for antialiasing
    uv += 2. * (seed - .5) / iResolution.xy;

    Hit hit, firstHit;
    vec2 res;
    vec3 p, rayDir, origin, sam, ref, raf, nor, camOrigin, camDir;
    float invert, ior, offset, extinctionDist, maxDist, firstLen, bounceCount, wavelength;
    
    vec3 col = vec3(0);
    float focal = 3.8;
    bool refracted;

    vec3 bgCol = BGCOL * .22;

    invert = 1.;
    maxDist = 15.; 
    
	camOrigin = vec3(0,0,9.5);
   	camDir = normalize(vec3(uv * .168, -1.));

    //camOrigin = vec3(1.8, 5.5, -5.5) * 1.75;

    firstHit = march(camOrigin, camDir, invert, maxDist, .1);
    firstLen = firstHit.len;

    float steps = 0.;

    seed = hash22(seed);
    float rand = seed.x;

    //float rand = texture2D(iChannel0, (fragCoord + floor(iTime * 60.) * 10.) / iChannel0Size.xy).r;
    
    for (float disperse = 0.; disperse < MAX_DISPERSE; disperse++) {
        invert = 1.;
    	sam = vec3(0);

        origin = camOrigin;
        rayDir = camDir;

        extinctionDist = 0.;
        wavelength = disperse / MAX_DISPERSE;
        wavelength += (rand * 2. - 1.) * (.5 / MAX_DISPERSE);
        wavelength = mix(-.5/5., 1. - .5/5., mod(wavelength, 1.));
        
		bounceCount = 0.;

        for (float bounce = 0.; bounce < MAX_BOUNCE; bounce++) {

            if (bounce == 0.) {
                hit = firstHit;
            } else {
                hit = march(origin, rayDir, invert, maxDist / 5., .1);
            }
            
            steps += hit.steps;
            
            res = hit.res;
            p = hit.p;
            
            if (invert < 0.) {
	            extinctionDist += hit.len;
            }

            // hit background
            if ( res.y == 0.) {
                break;
            }


            vec3 nor = calcNormal(p) * invert;            
            ref = reflect(rayDir, nor);
            
            // shade
            sam += light(p, ref) * .5;
            sam += pow(max(1. - abs(dot(rayDir, nor)), 0.), 5.) * .1;
            sam *= vec3(.85,.85,.98);

            // refract
            float ior = mix(.1, .95, wavelength);
            ior = invert < 0. ? ior : 1. / ior;
            raf = refract(rayDir, nor, ior);
            bool tif = raf == vec3(0); // total internal reflection
            rayDir = tif ? ref : raf;
            offset = .01 / abs(dot(rayDir, nor));
            origin = p + offset * rayDir;
            //invert = tif ? invert : invert * -1.;
            invert *= -1.; // not correct but gives more interesting results

            bounceCount = bounce;
        }

        #ifndef DARK_MODE
            sam += bounceCount == 0. ? bgCol : env(p, rayDir);	
        #endif

        if (bounceCount == 0.) {
            // didn't bounce, so don't bother calculating dispersion
            col += sam * MAX_DISPERSE / 2.;
            break;
        } else {
            vec3 extinction = vec3(.5,.5,.5) * .0;
            extinction = 1. / (1. + (extinction * extinctionDist));	
            col += sam * extinction * spectrum(-wavelength+.15);
        }
	}
    
    // debug
 	//fragColor = vec4(spectrum(steps / 2000.), 1); return;
    //fragColor = vec4(vec3(bounceCount / MAX_BOUNCE), 1); return;
    //fragColor = vec4(vec3(firstHit.steps / 100.), 1); return;

    col /= MAX_DISPERSE;
    
    //col = mix(col, bgCol * .5, (1.0 - exp2(-0.005 * pow(firstLen - 7., 3.))) * .5);
        
    return vec4(col, range(0., 13., firstLen));
}


void mainImage(out vec4 fragColor, in vec2 fragCoord) {

    float duration = 2.;
    time = mod(iTime / duration, 1.);

    vec4 col = draw(fragCoord, iFrame);

    if (drawIndex > 0.) {
        vec4 lastCol = texture2D(previousSample, fragCoord.xy / iResolution.xy);
        col = mix(lastCol, col, 1. / (drawIndex + 1.));
    }
    
    fragColor = col;
}
