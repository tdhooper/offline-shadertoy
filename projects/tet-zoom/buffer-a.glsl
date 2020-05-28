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

float time;


#define PI 3.14159265359
#define TAU 6.28318530718

#define saturate(x) clamp(x, 0., 1.)

void pR(inout vec2 p, float a) {
    p = cos(a)*p + sin(a)*vec2(p.y, -p.x);
}

vec3 pMirror(vec3 p, vec3 n) {
    return p - (2. * dot(p, n)) * n;    
}



float smin(float a, float b, float r) {
    vec2 u = max(vec2(r - a,r - b), vec2(0));
    return max(r, min (a, b)) - length(u);
}

float smax(float a, float b, float r) {
    vec2 u = max(vec2(r + a,r + b), vec2(0));
    return min(-r, max (a, b)) + length(u);
}

float smin2(float a, float b, float k){
    float f = clamp(0.5 + 0.5 * ((a - b) / k), 0., 1.);
    return (1. - f) * a + f  * b - f * (1. - f) * k;
}

float smax2(float a, float b, float k) {
    return -smin2(-a, -b, k);
}

float smin3(float a, float b, float k){
    return min(
        smin(a, b, k),
        smin2(a, b, k)
    );
}

float smax3(float a, float b, float k){
    return max(
        smax(a, b, k),
        smax2(a, b, k)
    );
}



float vmax(vec2 v) {
	return max(v.x, v.y);
}

float vmax(vec3 v) {
	return max(v.x, max(v.y, v.z));
}

float fBox(vec2 p, vec2 b) {
	vec2 d = abs(p) - b;
	return length(max(d, vec2(0))) + vmax(min(d, vec2(0)));
}


float fBox(vec3 p, vec3 b) {
	vec3 d = abs(p) - b;
	return length(max(d, vec3(0))) + vmax(min(d, vec3(0)));
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

float range(float vmin, float vmax, float value) {
  return clamp((value - vmin) / (vmax - vmin), 0., 1.);
}


float tween(float t, float start, float duration) {
    t = range(start, start + duration, t);
//    t = pow(t, 3.);

    return smoothstep(0., 1., t);
}


float tweenBlend(float t, float start, float duration) {
    t = range(start, start + duration, t);
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



    float blendDuration = .5;
    float offsetDuration = 1.;
    float step2Start = .025;

    float t = time * (step2Start + blendDuration + offsetDuration);
    //t *= .75;

    offsetDuration *= 1.5;

    float offsetDistance = .3;

    // animation
    float rtween = tweenBlend(t, .0, .5);
    float b1 = tweenBlend(t, .0, blendDuration);
    float ot1 = tween(t, blendDuration, offsetDuration);
    float ot2 = tween(t, step2Start + blendDuration, offsetDuration);
    float o1 = ot1 * offsetDistance;
    float o2 = ot2 * offsetDistance;

    if (ot2 > 1.) {
        return 1e12;
    }

    //o1 = 0.;
    //o2 = 0.;

    vec3 n1 = pca;
    vec3 n2 = normalize(pca * vec3(-1,-1,1));
    vec3 n3 = normalize(pbc * vec3(1,-1,-1));
    vec3 n4 = normalize(pbc * vec3(-1,-1,-1));

    float sz = .3;

    vec3 pp = p;

    float rbase = .04;
    float rInner = rbase * STEP_SCALE;
    float rOuter = rbase * STEP_SCALE * (1. + ot1 * 2.);
    float sep = .001 * (1. - o2);

    float scale1 = 1. - ot1;
    float scale2 = 1. - ot2;

    // base tet
    float base = tetBase(p, sz, rbase);

    // inner tet
    float inner = -(dot(p, n4) + .1 - sep);
    inner = smax(inner, -(dot(p, n3) + .1 - sep), rInner);
    inner = smax(inner, -(dot(p, n2) + .1 - sep), rInner);

    // octahedrons
    p = pp + n4 * o2;
    p /= scale2;
    float oct = tetBase(p, sz, rbase);
    oct = smax(oct, -(dot(p, n4) + .5 - sep), rOuter);
    oct = smax(oct, -(dot(p, n3) + .1 - sep), rOuter);
    oct = smax(oct, (dot(p, n4) + .1 + sep), rOuter);
    oct = smax(oct, -(dot(p, n2) + .1 - sep), rOuter);
    oct *= scale2;

    // edge tets
    p = pp + (n4 + n3) * o2;
    p /= scale2;
    float edge = tetBase(p, sz, rbase);
    edge = smax(edge, (dot(p, n3) + .1 + sep), rOuter);
    edge = smax(edge, (dot(p, n4) + .1 + sep), rOuter);
    edge *= scale2;

    // vertex tets
    p = pp + n4 * (o1 + o2);
    p /= scale1;
    float vert = tetBase(p, sz, rbase);
    vert = smax(vert, (dot(p, n4) + .5 + sep), rOuter);
    vert *= scale1;

    float fractured = min(min(oct, edge), vert);

    if (time < 1.) {
        fractured = min(fractured, inner);
    }

    // surface: 0 - .5 -  1 - 1
    // center:  0 -  0 - .5 - 1



    float surface = 1. - saturate(-base / sz); // 1 at surface, 0 at center
    //float bl = saturate(b1 * 2.);
    float ss = 2.;
    float bl = saturate(b1 * (1. + ss) - surface * ss);
    float d = mix(base, fractured, bl);

    return d;
}

float tetLoop(vec3 p) {
    float t = time;
    //t = smoothstep(.9, 1., t);
    float scale = pow(STEP_SCALE, t);
    //scale = 1.;
    pR(p.xy, PI/2. * time);
    float d = tetAnim(p * scale, time) / scale;
    //d = min(d, length(p.xy) - .05);
    scale *= STEP_SCALE;
    pR(p.xy, PI/2. * -1.);
    d = min(d, tetAnim(p * scale, time + 1.) / scale);
    return d;
}

vec2 map(vec3 p) {
    float back = -p.z + 10.;
    float d = tetLoop(p);

    //d = fBox(p, vec3(.2)) - .05;
    return vec2(d, 1.);

    if (iMouse.x > 0.) {
    //	pR(p.yz, ((iMouse.y / iResolution.y) * 2. - 1.) * 2.);
   // 	pR(p.xz, ((iMouse.x / iResolution.x) * 2. - 1.) * 3.);
    }

    return back < d ? vec2(back, 0.) : vec2(d, 1.);
}



float hitDebugPlane = 0.;

vec2 mapDebug(vec3 p) {
    vec2 res = map(p);

    p = (debugPlaneMatrix * vec4(p, 1)).xyz;
    float plane = abs(p.y);

    hitDebugPlane = plane < abs(res.x) ? 1. : 0.;
    res.x = min(res.x, plane);

    return res;
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

vec3 light(vec3 origin, vec3 rayDir) {
    origin = -(cameraMatrix * vec4(origin, 1)).xyz;
    rayDir = -(cameraMatrix * vec4(rayDir, 0)).xyz;

    vec2 uv;
    float hit = intersectPlane(origin, rayDir, vec3(5,-2,-8), normalize(vec3(1,-.5,-.1)), normalize(vec3(0,1,0)), uv);
    float l = smoothstep(.75, .0, fBox(uv, vec2(.4,1.2) * 2.));
	return vec3(l) * hit;
}

vec3 env(vec3 origin, vec3 rayDir) {
    origin = -(cameraMatrix * vec4(origin, 1)).xyz;
    rayDir = -(cameraMatrix * vec4(rayDir, 0)).xyz;

    float l = smoothstep(.0, 1.7, dot(rayDir, vec3(.5,-.3,1))) * .4;
   	return vec3(l) * vec3(1,1,1);
}



//========================================================
// Marching
//========================================================

const float MAX_DISPERSE = 5.;
const float MAX_BOUNCE = 10.;

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
        len += dist;// * .2;
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

#pragma glslify: distanceMeter = require(../clifford-torus/distance-meter.glsl)


void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
    initPoly();

    time = mod(iTime / 2., 1.);
    
    vec2 uv = (2. * fragCoord - iResolution.xy) / iResolution.y;

    Hit hit;
    vec2 res;
    vec3 p, rayDir, origin, sam, ref, raf, nor;
    float invert, ior, offset, extinctionDist, maxDist, firstLen, bounceCount, wavelength;
    
    vec3 col = vec3(0);
    float focal = 3.8;
    bool refracted;

    vec3 bgCol = vec3(.22);
    
    for (float disperse = 0.; disperse < MAX_DISPERSE; disperse++) {
        invert = 1.;
    	sam = vec3(0);

        origin = eye;
        rayDir = normalize(dir);

	    //origin = vec3(0, 0, -focal * .7);
    	//rayDir = normalize(vec3(uv, focal));

        extinctionDist = 0.;
        wavelength = disperse / MAX_DISPERSE;
		float rand = texture2D(iChannel0, iTime + fragCoord / iChannel0Size.xy).r;
        wavelength += (rand * 2. - 1.) * (.5 / MAX_DISPERSE);
        
		bounceCount = 0.;

        for (float bounce = 0.; bounce < MAX_BOUNCE; bounce++) {
        //while (bounce < MAX_BOUNCE) {
            maxDist = bounce == 0. ? 8. : 4.; 
            hit = march(origin, rayDir, invert, maxDist);
            res = hit.res;
            p = hit.p;
			
            if (bounce == 0. && disperse == 0.) {
            	firstLen = hit.len;
            }
            
            if (invert < 0.) {
	            extinctionDist += hit.len;
            }

            if ( res.y == 0. || bounce == MAX_BOUNCE - 1.) {
                if (bounce == 0.) {
                	sam += bgCol; break;	
                }
                sam += env(origin, rayDir);
                break;
            } else {
                vec3 nor = normal(p) * invert;
                
                
                //sam += (nor * .5 + .5) / 3.; break;

                // if (hitDebugPlane == 1.) {
                //     float d = map(rayPosition).x;
                //     sam += distanceMeter(d * 2., rayLength, rayDirection, camPos);
                //     break;
                // }
                
                
                ref = reflect(rayDir, nor);
                
                float ior = mix(1.3, 1.6, wavelength); // vs 1.6 with first bounce reflective (or 1.8 wihout)
                ior = invert < 0. ? ior : 1. / ior;
                raf = refract(rayDir, nor, ior);
                sam += light(origin, ref) * .2;
                sam += pow(1. - abs(dot(rayDir, nor)), 5.) * .1;
                sam *= vec3(.85,.85,.98);

                // Refract
                rayDir = raf == vec3(0) ? ref : raf;
                if (bounce == 1.) {
                    // make first inside bounce reflective
                    //rayDir = ref;
                }
                offset = .01 / abs(dot(rayDir, nor));
                origin = p + offset * rayDir;
                invert *= -1.;
            }
            
            bounceCount = bounce;
        }
        
        if (bounceCount == 0.) {
            // didn't bounce, so don't bother calculating dispersion
            col += sam * MAX_DISPERSE / 2.;
            break;
        } else {
            vec3 extinction = vec3(.3,.3,1.) * .78;
            extinction = 1. / (1. + (extinction * extinctionDist));	
            col += sam * extinction * spectrum(-wavelength+.2);
        }
	}
    
    col /= MAX_DISPERSE;
    
    float fog = 1. - exp((firstLen - 6.) * -.5);
   // col = mix(col, bgCol, clamp(fog, 0., 1.));
    //fragColor = vec4(range(3., 8., firstLen)); return;

    fragColor = vec4(col, range(3., 8., firstLen));
}
