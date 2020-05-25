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

struct MoveData {
    vec3 axis;
    float shift;
    float moveSide;
    float dist;

    float side;
    float bound;
};

MoveData separate(vec3 p, vec3 axis, float shift, float moveSide, float dist, inout vec3 offset) {
    vec3 o = -axis * dist * moveSide;
    p -= o;
    p += axis * shift;
    float bound = max(dist * 2., .01);
    float d = dot(p - offset, axis);
    float side = sign(d);
    o += axis * dist * sign(d);
    offset += o;
    return MoveData(axis, shift, moveSide, dist, side, bound);
}

void postSeparate(vec3 p, MoveData m, inout float d) {
    d = smax(d, dot(p + m.axis * m.shift, m.axis) * -m.side, .01);
}

void postSeparateB(vec3 p, MoveData m, inout float d) {
    float bound = dot(p + m.axis * m.shift, m.axis) * m.side + m.bound;
    if (bound > .001) d = min(d, bound);
}


float tet(vec3 p, float s, float r) {
    float d = dot(p, normalize(vec3(1))) - s;
    d = smax(d, dot(p, normalize(vec3(-1,-1,1))) - s, r);
    d = smax(d, dot(p, normalize(vec3(-1,1,-1))) - s, r);
    d = smax(d, dot(p, normalize(vec3(1,-1,-1))) - s, r);
    //d = max(d, length(p) - .1);
    //d = max(d, -abs(p.x));
    return d;
}

float range(float vmin, float vmax, float value) {
  return clamp((value - vmin) / (vmax - vmin), 0., 1.);
}


float tween(float t, float start, float duration) {
    t = range(start, start + duration, t);
//    t = pow(t, 3.);
    return smoothstep(0., 1., smoothstep(0., 1., t));
}

vec3 pMirror(vec3 p, vec3 n) {
    return p - (2. * dot(p, n)) * n;    
}

bool ddbg = false;

float tetBase(vec3 p, float sz, float r) {
    vec3 n1 = pca;
    vec3 n2 = normalize(pca * vec3(-1,-1,1));
    vec3 n3 = normalize(pbc * vec3(1,-1,-1));
    float d = dot(p, n1) - sz;
    d = smax(d, (dot(p, n2) - sz), r);
    d = smax(d, (dot(p, n3) - sz), r);
    return d;
}

float tet4(vec3 p) {
    
   p = fold(p);


    float t = time * 4.;
    float o1 = tween(t, .25, 1.) * .3;
    float o2 = tween(t, .5, 1.) * .3;

    vec3 n1 = pca;
    vec3 n2 = normalize(pca * vec3(-1,-1,1));
    vec3 n3 = normalize(pbc * vec3(1,-1,-1));
    vec3 n4 = normalize(pbc * vec3(-1,-1,-1));

    float sz = .3;

    vec3 pp = p;

    float r = .02;
    float rbase = .1;

    float d4 = -(dot(p, n4) + .1);
    d4 = smax(d4, -(dot(p, n3) + .1), r);
    d4 = smax(d4, -(dot(p, n2) + .1), r);

    p = pp + n4 * o2;

    float d2 = tetBase(p, sz, rbase);
    d2 = smax(d2, -(dot(p, n4) + .5), r);
    d2 = smax(d2, -(dot(p, n3) + .1), r);
    d2 = smax(d2, (dot(p, n4) + .1), r);
    d2 = smax(d2, -(dot(p, n2) + .1), r);
    
    p = pp + (n4 + n3) * o2;

    float d3 = tetBase(p, sz, rbase);
    d3 = smax(d3, (dot(p, n3) + .1), r);
    d3 = smax(d3, (dot(p, n4) + .1), r);
    
    p = pp + n4 * (o1 + o2);

    float d1 = tetBase(p, sz, rbase);
    d1 = smax(d1, (dot(p, n4) + .5), r);


    float d = min(min(min(d1, d2), d3), d4);

    return d;
}

float tet3(vec3 p) {
    p = fold(p);
    float d = dot(p, pca) - .3;
    return d;
}


float tet2(vec3 p) {
    
    //p = fold(p);

    //return length(p) - .5;
    //return dot(p, pca) - .5;

    float sz = .3;

    //pR(p.yz, max(time - .4, 0.) * 10.);

    float t = time * 4.;
    float o1 = tween(t, .25, 1.) * .3;
    float o2 = tween(t, .5, 1.) * .3;
    
    float bound = tet(p, sz + (o1 + o2) * 2./3., .0);

    if (bound > .1) {
    //    return bound;
    }
    


    vec3 mirror = normalize(cross(pca, pab));
    vec3 pbc2 = pbc - (2. * dot(pbc, mirror)) * mirror;    

    vec3 offset = vec3(0);
        
    MoveData m2 = separate(p, pbc2, -.1, -1., o2, offset);
    MoveData m3 = separate(p, pbc, -.1, -1., o2, offset);
    MoveData m1 = separate(p, pbc, -.5, -1., o1, offset);

    p -= offset;
    float d = dot(p, pca) - sz;



    //d = smax(d, -abs(dot(p, pbc) - .1) + .01, .03);
    //d = smax(d, -abs(dot(p, pbc2) - .1) + .01, .03);
    //d = smax(d, -abs(dot(p, pbc) - .5) + .01, .03);
            
    //d = min(d, length(p - pab * .5) - .05);
    //d = min(d, length(p - pbc * 1.) - .1);
    //d = min(d, length(p - pca * .3) - .025);

        
    postSeparate(p, m2, d);
    postSeparate(p, m3, d);
    postSeparate(p, m1, d);
    
    postSeparateB(p, m2, d);
    postSeparateB(p, m3, d);
    postSeparateB(p, m1, d);

    

    return d;
}

float tet2_old(vec3 p) {

    // TODO: use knighty poly mirroring so we can cut down on some work and
    // maybe focus on the side that doesn't get glitches!

    float sz = .3;

    pR(p.yz, max(time - .4, 0.) * 10.);

    float t = time * 4.;
    float o1 = tween(t, .25, 1.) * .3;
    float o2 = tween(t, .5, 1.) * .3;
    
    float bound = tet(p, sz + (o1 + o2) * 2./3., .0);

    if (bound > .1) {
        return bound;
    }
    
    vec3 n1 = normalize(vec3(1,1,1));
    vec3 n2 = normalize(vec3(-1,-1,1));
    vec3 n3 = normalize(vec3(-1,1,-1));
    vec3 n4 = normalize(vec3(1,-1,-1));

    vec3 offset = vec3(0);
    
    MoveData m5 = separate(p, n1, .1, 0., o2, offset);
    MoveData m6 = separate(p, n2, .1, 0., o2, offset);
    MoveData m7 = separate(p, n3, .1, 0., o2, offset);
    MoveData m8 = separate(p, n4, .1, 0., o2, offset);
    
    MoveData m1 = separate(p, n1, .5, 0., o1, offset);
    MoveData m2 = separate(p, n2, .5, 0., o1, offset);
    MoveData m3 = separate(p, n3, .5, 0., o1, offset);
    MoveData m4 = separate(p, n4, .5, 0., o1, offset);

    p -= offset;
    float d = tet(p, sz, .01);

    postSeparate(p, m5, d);
    postSeparate(p, m6, d);
    postSeparate(p, m7, d);
    postSeparate(p, m8, d);
    
    postSeparate(p, m1, d);
    postSeparate(p, m2, d);
    postSeparate(p, m3, d);
    postSeparate(p, m4, d);
    
    
    postSeparateB(p, m5, d);
    postSeparateB(p, m6, d);
    postSeparateB(p, m7, d);
    postSeparateB(p, m8, d);
    
    postSeparateB(p, m1, d);
    postSeparateB(p, m2, d);
    postSeparateB(p, m3, d);
    postSeparateB(p, m4, d);

    return d;
}


float map(vec3 p) {
    float d = length(p) - .5;
    d = tet4(p);
    return d;
}

float hitDebugPlane = 0.;

float mapDebug(vec3 p) {
    float d = map(p);

    float t = tet3(p);

    p = (debugPlaneMatrix * vec4(p, 1)).xyz;
    float plane = abs(p.y);
    //plane = min(plane, t);

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

    time = mod(iTime / 5., 1.);

    vec2 p = (-iResolution.xy + 2.0*(fragCoord))/iResolution.y;

    ddbg = p.x > 0.;

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
