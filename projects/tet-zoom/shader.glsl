precision highp float;

uniform vec2 iResolution;
uniform float iTime;

varying vec3 eye;
varying vec3 dir;

uniform vec3 debugPlanePosition;
uniform mat4 debugPlaneMatrix;


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
    return smoothstep(0., 1., smoothstep(0., 1., t));
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

float tet4(vec3 p) {
    
    p = fold(p);



    float bt = .5;
    float ot = 1.;
    float step2Start = .0;

    float t = time * (step2Start + bt + ot);

    float b1 = tweenBlend(t, .0, bt);
    float b2 = tweenBlend(t, step2Start, bt);

    float o1 = tween(t, bt, ot) * .6;
    float o2 = tween(t, step2Start + bt, ot) * .6;

    vec3 n1 = pca;
    vec3 n2 = normalize(pca * vec3(-1,-1,1));
    vec3 n3 = normalize(pbc * vec3(1,-1,-1));
    vec3 n4 = normalize(pbc * vec3(-1,-1,-1));

    float sz = .3;

    vec3 pp = p;

    float rbase = .1;
    float r1 = rbase * STEP_SCALE * b1;
    float r2 = rbase * STEP_SCALE * b2;

    // base tet
    float base = tetBase(p, sz, rbase);

    // inner tet
    float inner = -(dot(p, n4) + .1);
    inner = smax(inner, -(dot(p, n3) + .1), r2);
    inner = smax(inner, -(dot(p, n2) + .1), r2);

    // octahedrons
    p = pp + n4 * o2;
    float oct = tetBase(p, sz, rbase);
    oct = smax(oct, -(dot(p, n4) + .5), r2);
    oct = smax(oct, -(dot(p, n3) + .1), r2);
    oct = smax(oct, (dot(p, n4) + .1), r2);
    oct = smax(oct, -(dot(p, n2) + .1), r2);
    
    // edge tets
    p = pp + (n4 + n3) * o2;
    float edge = tetBase(p, sz, rbase);
    edge = smax(edge, (dot(p, n3) + .1), r2);
    edge = smax(edge, (dot(p, n4) + .1), r2);
    
    // vertex tets
    p = pp + n4 * (o1 + o2);
    float vert = tetBase(p, sz, rbase);
    vert = smax(vert, (dot(p, n4) + .5), r1);

    // base minus vertex tets
    p = pp;
    float base2 = tetBase(p, sz, rbase);
    base2 = smax(base2, -(dot(p, n4) + .5), r1);
 
    float stage1 = base;
    float stage2 = min(vert, base2);
    float stage3 = min(min(min(inner, oct), edge), vert);

    float d = mix(mix(stage1, stage2, b1), stage3, b2);

    return d;
}

float map(vec3 p) {
    float scale = pow(STEP_SCALE, time);
    p *= scale;
    pR(p.xy, PI/2. * time);
    float d = tet4(p);
    return d / scale;
}

float hitDebugPlane = 0.;

float mapDebug(vec3 p) {
    float d = map(p);

    p = (debugPlaneMatrix * vec4(p, 1)).xyz;
    float plane = abs(p.y);

    hitDebugPlane = plane < abs(d) ? 1. : 0.;
    d = min(d, plane);

    return d;
}

const int NORMAL_STEPS = 6;
vec3 calcNormal(vec3 pos){
    vec3 eps = vec3(.0005,0,0);
    vec3 nor = vec3(0);
    float invert = 1.;
    vec3 npos;
    for (int i = 0; i < NORMAL_STEPS; i++){
        npos = pos + eps * invert;
        nor += map(npos) * eps * invert;
        eps = eps.zxy;
        invert *= -1.;
    }
    return normalize(nor);
}

mat3 calcLookAtMatrix( in vec3 ro, in vec3 ta, in float roll )
{
    vec3 ww = normalize( ta - ro );
    vec3 uu = normalize( cross(ww,vec3(sin(roll),cos(roll),0.0) ) );
    vec3 vv = normalize( cross(uu,ww));
    return mat3( uu, vv, ww );
}

#pragma glslify: distanceMeter = require(../clifford-torus/distance-meter.glsl)

void mainImage(out vec4 fragColor, in vec2 fragCoord) {
    initPoly();

    time = mod(iTime / 2., 1.);

    vec2 p = (-iResolution.xy + 2.0*(fragCoord))/iResolution.y;

    vec3 camPos = eye;
    vec3 rayDirection = normalize(dir);

    // mat3 camMat = calcLookAtMatrix( camPos, vec3(0,.23,-.35), -1.68);
    // rayDirection = normalize( camMat * vec3(p.xy,2.8) );

    vec3 rayPosition = camPos;
    float rayLength = 0.;
    float dist = 0.;
    bool bg = false;
    vec3 res;

    for (int i = 0; i < 300; i++) {
        rayLength += dist;
        rayPosition = camPos + rayDirection * rayLength;
        dist = map(rayPosition) * .5;

        if (abs(dist) < .0001) {
            break;
        }
        
        if (rayLength > 30.) {
            bg = true;
            break;
        }
    }

    vec3 col =  vec3(.19,.19,.22) * .2;
    
    if ( ! bg) {
        vec3 nor = calcNormal(rayPosition);
        col = nor * .5 + .5;
    }

    if (hitDebugPlane == 1.) {
        float d = map(rayPosition);
        col = distanceMeter(d * 2., rayLength, rayDirection, camPos);
    }

    col = pow( col, vec3(0.4545) );
    fragColor = vec4(col,1.0);
}
