precision highp float;

uniform vec2 iResolution;
uniform vec2 iOffset;
uniform float iGlobalTime;
uniform vec4 iMouse;
uniform sampler2D iChannel0;


void mainImage(out vec4 a, in vec2 b);

void main() {
    mainImage(gl_FragColor, gl_FragCoord.xy + iOffset.xy);
}

#ifdef GL_ES
precision mediump float;
#endif




// Author: 
// Title: 


#define PI 3.14159265359

uniform float u_time;
uniform vec2 u_resolution;
uniform vec2 u_mouse;

float t;
//vec2 iResolution = u_resolution;
//vec2 iMouse = u_mouse;

// #define t u_time
//#define iResolution u_resolution
//#define iMouse u_mouse


// HG_SDF


float vmax(vec3 v) {
    return max(max(v.x, v.y), v.z);
}

float fPlane(vec3 p, vec3 n, float distanceFromOrigin) {
    return dot(p, n) + distanceFromOrigin;
}

// Box: correct distance to corners
float fBox(vec3 p, vec3 b) {
    vec3 d = abs(p) - b;
    return length(max(d, vec3(0))) + vmax(min(d, vec3(0)));
}

// Rotate around a coordinate axis (i.e. in a plane perpendicular to that axis) by angle <a>.
// Read like this: R(p.xz, a) rotates "x towards z".
// This is fast if <a> is a compile-time constant and slower (but still practical) if not.
void pR(inout vec2 p, float a) {
    p = cos(a)*p + sin(a)*vec2(p.y, -p.x);
}

// Reflect space at a plane
float pReflect(inout vec3 p, vec3 planeNormal, float offset) {
    float t = dot(p, planeNormal)+offset;
    if (t < 0.) {
        p = p - (2.*t)*planeNormal;
    }
    return sign(t);
}


// The "Round" variant uses a quarter-circle to join the two objects smoothly:
float fOpUnionRound(float a, float b, float r) {
    float m = min(a, b);
    if ((a < r) && (b < r) ) {
        return min(m, r - sqrt((r-a)*(r-a) + (r-b)*(r-b)));
    } else {
     return m;
    }
}

float fOpIntersectionRound(float a, float b, float r) {
    float m = max(a, b);
    if ((-a < r) && (-b < r)) {
        return max(m, -(r - sqrt((r+a)*(r+a) + (r+b)*(r+b))));
    } else {
        return m;
    }
}

float fOpDifferenceRound (float a, float b, float r) {
    return fOpIntersectionRound(a, -b, r);
}


// Knighty https://www.shadertoy.com/view/XlX3zB

int Type=5;

vec3 nc,pab,pbc,pca;
void initIcosahedron() {//setup folding planes and vertex
    float cospin=cos(PI/float(Type)), scospin=sqrt(0.75-cospin*cospin);
    nc=vec3(-0.5,-cospin,scospin);//3rd folding plane. The two others are xz and yz planes
    pab=vec3(0.,0.,1.);
    pbc=vec3(scospin,0.,0.5);//No normalization in order to have 'barycentric' coordinates work evenly
    pca=vec3(0.,scospin,cospin);
    pbc=normalize(pbc); pca=normalize(pca);//for slightly better DE. In reality it's not necesary to apply normalization :) 
}

// Barycentric to Cartesian 
vec3 bToC(vec3 A, vec3 B, vec3 C, vec3 barycentric) {
    return barycentric.x * A + barycentric.y * B + barycentric.z * C;
}

vec3 pModIcosahedron(inout vec3 p, int subdivisions) {
    p = abs(p);
    pReflect(p, nc, 0.);
    p.xy = abs(p.xy);
    pReflect(p, nc, 0.);
    p.xy = abs(p.xy);
    pReflect(p, nc, 0.);
    
    if (subdivisions > 0) {

        vec3 A = pbc;
        vec3 C = reflect(A, normalize(cross(pab, pca)));
        vec3 B = reflect(C, normalize(cross(pbc, pca)));
       
        vec3 n;

        // Fold in corner A 
        
        vec3 p1 = bToC(A, B, C, vec3(.5, .0, .5));
        vec3 p2 = bToC(A, B, C, vec3(.5, .5, .0));
        n = normalize(cross(p1, p2));
        pReflect(p, n, 0.);
        
        if (subdivisions > 1) {

            // Get corners of triangle created by fold

            A = reflect(A, n);
            B = p1;
            C = p2;
            
            // Fold in corner A

            p1 = bToC(A, B, C, vec3(.5, .0, .5));
            p2 = bToC(A, B, C, vec3(.5, .5, .0));
            n = normalize(cross(p1, p2));
            pReflect(p, n, 0.);
            

            // Fold in corner B
            
            p2 = bToC(A, B, C, vec3(.0, .5, .5));
            p1 = bToC(A, B, C, vec3(.5, .5, .0));
            n = normalize(cross(p1, p2));
            pReflect(p, n, 0.);
        }
    }

    return p;
}

vec3 pRoll(inout vec3 p) {
    //return p;
    float s = 5.;
    float d = 0.01;
    float a = sin(t * s) * d;
    float b = cos(t * s) * d;
    pR(p.xy, a);
    pR(p.xz, a + b);
    pR(p.yz, b);
    return p;
}

vec3 lerp(vec3 a, vec3 b, float s) {
    return a + (b - a) * s;
}

float face(vec3 p) {
    // Align face with the xy plane
    vec3 rn = normalize(lerp(pca, vec3(0,0,1), 0.5));
    p = reflect(p, rn);
    return min(
        fPlane(p, vec3(0,0,-1), -1.4),
        length(p + vec3(0,0,1.4)) - 0.02
    );
}

vec3 planeNormal(vec3 p) {
    // Align face with the xy plane
    vec3 rn = normalize(lerp(pca, vec3(0,0,1), 0.5));
    return reflect(p, rn);  
}
vec3 pSpin(vec3 p) {
    pR(p.xz, t/2.);
    pR(p.yz, t/4. + 10.);
    pR(p.xy, t);
    return p;
}

float spinningBox(vec3 p) {
    p = pSpin(p);
    return fBox(p, vec3(1., 1., 1.));
}

// float inner(vec3 p) {
//     //float t = 1.;
//     int i = int(mod(t/4., 2.));
//  pModIcosahedron(p, i+1);
//     p = planeNormal(p);
//     p.z += 1.;
//     //p.z += sin(t*PI/2. + .2) * 0.5;
//     //pR(p.xy, t*1.5);
//     //pR(p.zy, t/2.);
    
//     pR(p.zy, PI*t/4.);
//     pR(p.zy, PI*.5);
    
//     //return fBox(p, vec3(9.,.05,(float(i)/3.)+.1));
//     return fBox(p, vec3(9., .05, .2));
// }

float inner(vec3 p) {
    // float t = 0.;
    pModIcosahedron(p, 2);
    p = planeNormal(p);
    p.z += 2.;
    pR(p.zy, PI*t/4.);
    pR(p.zy, PI*.5);
    return fBox(p, vec3(9., .1, .1));
}

float other(vec3 p) {
    //pR(p.xz, t*.3);
    //pR(p.zy, t*.3);
    pModIcosahedron(p, 1);
    p = planeNormal(p);
    p += vec3(0.,0.,2.);
    pR(p.xz, t*1.5 * 1.);
    pR(p.zy, t/2. + 2.);
    return fBox(p, vec3(.5,.1,.2));
}

float exampleModelC(vec3 p) {
    pR(p.xy, 2.832);
    
    // pR(p.xz, t/3.);

    // pR(p.yz, t*PI/2.);
    // pR(p.xy, t*PI/4.);
     //pModIcosahedron(p, 2);
     //pR(p.xy, t/8.);
     // pR(p.yz, t/16.);
    //pModIcosahedron(p, 1);
    //p = planeNormal(p);
    float b = inner(p);
    float a = other(p);
    return b;
    return fOpDifferenceRound(a, b, 0.3);
}

float exampleModel(vec3 p) {
    //pRoll(p);
    return exampleModelC(p);
}

vec3 doBackground(vec3 rayVec) {
    return vec3(.13);
}

// The MINIMIZED version of https://www.shadertoy.com/view/Xl2XWt


const float MAX_TRACE_DISTANCE = 20.0;           // max trace distance
const float INTERSECTION_PRECISION = 0.0001;        // precision of the intersection
const int NUM_OF_TRACE_STEPS = 200;


// checks to see which intersection is closer
// and makes the y of the vec2 be the proper id
vec2 opU( vec2 d1, vec2 d2 ){
    
    return (d1.x<d2.x) ? d1 : d2;
    
}


//--------------------------------
// Modelling 
//--------------------------------
vec2 map( vec3 p ){  
    
    vec2 res = vec2(exampleModel(p) ,1.); 
    // vec2 res2 = vec2(core(p) ,2.); 
    return res;
    // return opU(res, res2);
}



vec2 calcIntersection( in vec3 ro, in vec3 rd ){

    
    float h =  INTERSECTION_PRECISION*2.0;
    float t = 0.0;
    float res = -1.0;
    float id = -1.;
    
    for( int i=0; i< NUM_OF_TRACE_STEPS ; i++ ){
        
        if( h < INTERSECTION_PRECISION || t > MAX_TRACE_DISTANCE ) break;
        vec2 m = map( ro+rd*t );
        h = m.x;
        t += h;
        id = m.y;
        
    }

    if( t < MAX_TRACE_DISTANCE ) res = t;
    if( t > MAX_TRACE_DISTANCE ) id =-1.0;
    
    return vec2( res , id );
    
}


//----
// Camera Stuffs
//----
mat3 calcLookAtMatrix( in vec3 ro, in vec3 ta, in float roll )
{
    vec3 ww = normalize( ta - ro );
    vec3 uu = normalize( cross(ww,vec3(sin(roll),cos(roll),0.0) ) );
    vec3 vv = normalize( cross(uu,ww));
    return mat3( uu, vv, ww );
}

void doCamera(out vec3 camPos, out vec3 camTar, in float time, in vec2 mouse) {
    
    float x = .366 + (mouse.x * 0.5);
    float y = .4 + (mouse.y * 0.33);
    
    float an = 10.0 * x + PI / 2.;
    //an = 10.;
    float roll = .6;
    //roll = 0.;
    
    //float d = 2. + sin(an) * 1.6;
    float d = 2. + (1. - y) * 10.;
    camPos = vec3(
        sin(an),
        sin(y * PI / 2.) - roll,
        cos(an) - roll
    ) * d;
    
    camPos = vec3(1.,0.,0.)*5.5;

    camTar = vec3(0);
}


// Calculates the normal by taking a very small distance,
// remapping the function, and getting normal for that
vec3 calcNormal( in vec3 pos ){
    
    vec3 eps = vec3( 0.0001, 0.0, 0.0 );
    vec3 nor = vec3(
        map(pos+eps.xyy).x - map(pos-eps.xyy).x,
        map(pos+eps.yxy).x - map(pos-eps.yxy).x,
        map(pos+eps.yyx).x - map(pos-eps.yyx).x );
    return normalize(nor);
}




vec3 render( vec2 res , vec3 ro , vec3 rd ){
   

  vec3 color = doBackground(rd);
  
    if (res.y == 2.) {
        return vec3(0.987,0.257,1.000);
    }
    
  if( res.y > -.5 ){
      
    float mult = (sin(PI*t/2. + 4.408) + .5) / 2.;
    // mult = pow(mult, 2.);
    vec3 pos = ro + rd * res.x;
    vec3 norm = calcNormal( pos );
    vec3 ref = reflect(rd, norm);
    color = norm * 0.5 + 0.5;
    float split = .7 - (dot(pos, norm) / PI);
    // split = 1. - split;
    // split = clamp(split + mult, 0., 1.);
    split = pow(split, 4.);
    float light = dot(ref, normalize(vec3(0,1,1)));
    light *= 1. - split;
    // float mult = (sin(PI*t/2. + -2.966) + 1.) / 2.;
    color *= split;
    color += light * 0.3;
    // color = vec3(split);
    
  }
   
  return color;
}



void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
    t = iGlobalTime;
    t = iMouse.x / iResolution.x * 2.;
    t = 0.68;
    initIcosahedron();
    
    vec2 p = (-iResolution.xy + 2.0*fragCoord.xy)/iResolution.y;
    vec2 m = iMouse.xy / iResolution.xy;

    p *= 1.1;

    vec3 ro = vec3( 0., 0., 2.);
    vec3 ta = vec3( 0. , 0. , 0. );
    
    // camera movement
    doCamera(ro, ta, t, m);
    
    // camera matrix
    mat3 camMat = calcLookAtMatrix( ro, ta, 0.0 );  // 0.0 is the camera roll
    
    // create view ray
    vec3 rd = normalize( camMat * vec3(p.xy,2.0) ); // 2.0 is the lens length
    
    vec2 res = calcIntersection( ro , rd  );

    
    vec3 color = render( res , ro , rd );
    
    fragColor = vec4(color,1.0);
}
