precision highp float;

uniform vec2 iResolution;
uniform float iTime;
uniform vec4 iMouse;

uniform mat4 cameraMatrix;

varying vec3 eye;
varying vec3 dir;

uniform vec3 debugPlanePosition;
uniform mat4 debugPlaneMatrix;

uniform sampler2D iChannel0; // images/blue-noise.png filter: linear wrap: repeat
uniform vec2 iChannel0Size;

void mainImage(out vec4 a, in vec2 b);

void main() {
    mainImage(gl_FragColor, gl_FragCoord.xy);
}


//#define LIGHT_MODE

// HG_SDF

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

float range(float vmin, float vmax, float value) {
  return clamp((value - vmin) / (vmax - vmin), 0., 1.);
}

float vmax(vec2 v) {
	return max(v.x, v.y);
}

float fBox(vec2 p, vec2 b) {
	vec2 d = abs(p) - b;
	return length(max(d, vec2(0))) + vmax(min(d, vec2(0)));
}

// IQ Spectrum

vec3 pal( in float t, in vec3 a, in vec3 b, in vec3 c, in vec3 d ) {
    return a + b*cos( 6.28318*(c*t+d) );
}

vec3 spectrum(float n) {
    return pal( n, vec3(0.5,0.5,0.5),vec3(0.5,0.5,0.5),vec3(1.0,1.0,1.0),vec3(0.0,0.33,0.67) );
}

// Knighty polyhedra

int Type=3;

vec3 nc,pab,pbc,pca;
void initPoly() {//setup folding planes and vertex
    float cospin=cos(PI/float(Type)), scospin=sqrt(0.75-cospin*cospin);
	nc=vec3(-0.5,-cospin,scospin);//3rd folding plane. The two others are xz and yz planes
	pab=vec3(0.,0.,1.);
	pbc=vec3(scospin,0.,0.5);//No normalization in order to have 'barycentric' coordinates work evenly
	pca=vec3(0.,scospin,cospin);
	pbc=normalize(pbc);	pca=normalize(pca);//for slightly better DE. In reality it's not necesary to apply normalization :) 
}

vec3 fold(vec3 pos) {
	for(int i=0;i<3 /*Type*/;i++){
		pos.xy=abs(pos.xy);//fold about xz and yz planes
		pos-=2.*min(0.,dot(pos,nc))*nc;//fold about nc plane
	}
	return pos;
}

// Modelling

float time;

float tweenOffset(float t, float start, float duration) {
    t = range(start, start + duration, t);
    t = pow(t, 2.);
    return t;
}

float tweenBlend(float t, float start, float duration) {
    t = range(start, start + duration, t);
    t = pow(t, .5);
    t = smoothstep(0., 1., t);
    return t;
}

float tetBase(vec3 p, float sz, float r) {
    vec3 n1 = pca;
    vec3 n2 = normalize(pca * vec3(-1,-1,1));
    vec3 n3 = normalize(pbc * vec3(1,-1,-1));
    float d = dot(p, n1) - sz;
    d = smax(d, (dot(p, n2) - sz), r);
    d = smax(d, (dot(p, n3) - sz), r);
    return d;
}

const float STEP_SCALE = 1./3.;

float tetAnim(vec3 p, float time) {
    
    p = fold(p);

    // config
    float sz = .3;
    float rBase = .04;
    float rInner = rBase * STEP_SCALE;
    float blendDuration = .75;
    float offsetDuration = .75;
    float t = time * (blendDuration + offsetDuration);
    offsetDuration *= 2.; // extend animation beyond loop section
    float offsetDistance = .6;

    // animation
    float blend = tweenBlend(t, .0, blendDuration);
    float offsetT = tweenOffset(t, blendDuration, offsetDuration);
    float offset = offsetT * offsetDistance;

    // skip if animation hasn't started or is complete
    if (t < 0. || offsetT >= 1.) {
        return 1e12;
    }

    // tetrahedron planes
    vec3 n1 = pca;
    vec3 n2 = normalize(pca * vec3(-1,-1,1));
    vec3 n3 = normalize(pbc * vec3(1,-1,-1));
    vec3 n4 = normalize(pbc * vec3(-1,-1,-1));

    float sep = .001 * (1. - offsetT);
    float scale = 1. - offsetT;

    float bound = (dot((p + (n4 + n3) * offset) / scale, n1) - sz) * scale;
    if (bound > .004) {
        return bound;
    }

    vec3 pp = p;

    // base tet
    float base = tetBase(p, sz, rBase);

    // inner tet
    float inner = -(dot(p, n4) + .1 - sep);
    inner = smax(inner, -(dot(p, n3) + .1 - sep), rInner);
    inner = smax(inner, -(dot(p, n2) + .1 - sep), rInner);

    // octahedrons
    p = pp + n4 * offset;
    p /= scale;
    float oct = tetBase(p, sz, rBase);
    oct = smax(oct, -(dot(p, n4) + .5 - sep), rInner);
    oct = smax(oct, -(dot(p, n3) + .1 - sep), rInner);
    oct = smax(oct, (dot(p, n4) + .1 + sep), rInner);
    oct = smax(oct, -(dot(p, n2) + .1 - sep), rInner);
    oct *= scale;

    // edge tets
    p = pp + (n4 + n3) * offset;
    p /= scale;
    float edge = tetBase(p, sz, rBase);
    edge = smax(edge, (dot(p, n3) + .1 + sep), rInner);
    edge = smax(edge, (dot(p, n4) + .1 + sep), rInner);
    edge *= scale;

    // vertex tets
    p = pp + n4 * (offset + offset);
    p /= scale;
    float vert = tetBase(p, sz, rBase);
    vert = smax(vert, (dot(p, n4) + .5 + sep), rInner);
    vert *= scale;

    float sliced = min(min(oct, edge), vert);

    // inner tet gets replaced with the next iteration
    if (time < 1.) {
        sliced = min(sliced, inner);
    }

    if (blend >= 1.) {
        return sliced;
    }

    float surface = 1. - saturate(-base / sz); // 1 at surface, 0 at center
    
    // blend indentations into the surface
    float surfaceBlend = saturate(blend * .66 * range(.9, 1., surface));
    base = mix(base, sliced, surfaceBlend);
    
    // grow the sliced tet from the center of the unsliced tet
    float slicedS = min(sliced, -base - (.3 - .3 * blend));
    float d = max(base, slicedS);
    d = mix(d, sliced, smoothstep(.9, 1., blend));

    return d;
}

float tetLoop(vec3 p) {
    pR(p.xy, PI/2. * -time + PI/2.);

    float t = time;
    float scale = pow(STEP_SCALE, t);
    float d = tetAnim(p * scale, time) / scale;

    scale *= STEP_SCALE;
    pR(p.xy, PI/2. * -1.);
    d = min(d, tetAnim(p * scale, time + 1.) / scale);

    return d;
}

vec2 map(vec3 p) {
    if (iMouse.x > 0.) {
    //	pR(p.yz, ((iMouse.y / iResolution.y) * 2. - 1.) * 2.);
   // 	pR(p.xz, ((iMouse.x / iResolution.x) * 2. - 1.) * 3.);
    }
    float d = tetLoop(p);
    return vec2(d, 1.);
}


//========================================================
// Lighting
//========================================================


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
    origin = -(cameraMatrix * vec4(origin, 1)).xyz;
    rayDir = -(cameraMatrix * vec4(rayDir, 0)).xyz;

    origin *= envOrientation;
    rayDir *= envOrientation;

    vec2 uv;
    float hit = intersectPlane(origin, rayDir, vec3(5,-2,-8), normalize(vec3(1,-.5,-.1)), normalize(vec3(0,1,0)), uv);
    float l = smoothstep(.75, .0, fBox(uv, vec2(.4,1.2) * 6.));
	return vec3(l) * hit;
}

vec3 env(vec3 origin, vec3 rayDir) {
    
    origin = -(cameraMatrix * vec4(origin, 1)).xyz;
    rayDir = -(cameraMatrix * vec4(rayDir, 0)).xyz;

    origin *= envOrientation;
    rayDir *= envOrientation;

    float l = smoothstep(.0, 1.7, dot(rayDir, vec3(.5,-.3,1))) * .4;
    //l = smoothstep(-.5, 2.2, dot(rayDir, vec3(.5,-.3,1))) * .4;
    //l = smoothstep(.2, .5, dot(rayDir, vec3(.5,-.3,1))) * .4;
    //l = smoothstep(-1., 2., dot(rayDir, vec3(.5,-.3,1))) * .4;
   	return vec3(l) * vec3(1,1,1);
}



//========================================================
// Marching
//========================================================

const float MAX_DISPERSE = 5.;
const float MAX_BOUNCE = 15.;

vec3 normal(in vec3 p){
  vec3 v = vec3(.001, 0, 0);
  vec3 n = vec3(
      map(p + v.xyy).x,
      map(p + v.yxy).x,
      map(p + v.yyx).x
  ) - map(p).x;
  return normalize(n);
}

struct Hit {
    vec2 res;
    vec3 p;
    float len;
};

Hit march(vec3 origin, vec3 rayDir, float invert, float maxDist) {
    vec3 p;
    float len = 0.;
    float dist = 0.;
    vec2 res = vec2(0.);

    for (float i = 0.; i < 200.; i++) {
        len += dist * .9;
        p = origin + len * rayDir;
        res = map(p);
        dist = res.x * invert;
        if (dist < .0000001) {
            break;
        }
        if (len >= maxDist) {
            res.y = 0.;
            len = maxDist;
            break;
        }
    }   

    return Hit(res, p, len);
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

void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
    initPoly();

    time = iTime / 2.;
    //time= 0.;
    time = fract(time + .4);
    
    #ifdef LIGHT_MODE
        envOrientation = sphericalMatrix(((vec2(81.5, 119) / vec2(187)) * 2. - 1.) * 2.);
    #else
        envOrientation = sphericalMatrix((vec2(0.7299465240641712,0.3048128342245989) * 2. - 1.) * 2.);
    #endif

    vec2 uv = (2. * fragCoord - iResolution.xy) / iResolution.y;

    Hit hit, firstHit;
    vec2 res;
    vec3 p, rayDir, origin, sam, ref, raf, nor;
    float invert, ior, offset, extinctionDist, maxDist, firstLen, bounceCount, wavelength;
    
    vec3 col = vec3(0);
    float focal = 3.8;
    bool refracted;

    vec3 bgCol = vec3(.22);

    invert = 1.;
    maxDist = 12.; 
    origin = eye;
    rayDir = normalize(dir);
    firstHit = march(origin, rayDir, invert, maxDist);
    firstLen = firstHit.len;
    
    for (float disperse = 0.; disperse < MAX_DISPERSE; disperse++) {
        invert = 1.;
    	sam = vec3(0);

        origin = eye;
        rayDir = normalize(dir);

        extinctionDist = 0.;
        wavelength = disperse / MAX_DISPERSE;
		float rand = texture2D(iChannel0, fragCoord / iChannel0Size.xy).r;
        wavelength += (rand * 2. - 1.) * (.5 / MAX_DISPERSE);
        
		bounceCount = 0.;

        for (float bounce = 0.; bounce < MAX_BOUNCE; bounce++) {

            if (bounce == 0.) {
                hit = firstHit;
            } else {
                hit = march(origin, rayDir, invert, maxDist / 2.);
            }
            
            res = hit.res;
            p = hit.p;
            
            if (invert < 0.) {
	            extinctionDist += hit.len;
            }

            // hit background
            if ( res.y == 0.) {
                break;
            }

            //sam += shade(p, origin, rayDir, wavelength, invert)

            vec3 nor = normal(p) * invert;
            
            ref = reflect(rayDir, nor);
            
            float ior = mix(1.3, 1.6, wavelength);
            ior = invert < 0. ? ior : 1. / ior;
            raf = refract(rayDir, nor, ior);
            sam += light(p, ref) * .5;
            sam += pow(1. - abs(dot(rayDir, nor)), 5.) * .1;
            sam *= vec3(.85,.85,.98);

            // Refract
            bool tif = raf == vec3(0); // total internal reflection
            rayDir = tif ? ref : raf;
            offset = .01 / abs(dot(rayDir, nor));
            origin = p + offset * rayDir;
            //invert = tif ? invert : invert * -1.;
            invert *= -1.; // not correct but gives more interesting results


            bounceCount = bounce;
        }

        #ifdef LIGHT_MODE
            sam += bounceCount == 0. ? bgCol : env(p, rayDir);	
        #endif

        if (bounceCount == 0.) {
            // didn't bounce, so don't bother calculating dispersion
            col += sam * MAX_DISPERSE / 2.;
            break;
        } else {
            vec3 extinction = vec3(.3,.3,1.) * .5;
            extinction = 1. / (1. + (extinction * extinctionDist));	
            col += sam * extinction * spectrum(-wavelength+.2);
        }
	}
    
    col /= MAX_DISPERSE;
    
    fragColor = vec4(col, range(4., 12., firstLen));
}
