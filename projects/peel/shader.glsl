#extension GL_EXT_frag_depth : enable

precision mediump float;

uniform vec2 iResolution;
uniform vec2 iOffset;
uniform float iTime;

uniform sampler2D uSource;
uniform sampler2D uDepth;


uniform mat4 projection;
varying vec3 eye;
varying vec3 dir;
varying vec3 cameraForward;

uniform bool guiBlend;
uniform bool guiNeck;

/* SHADERTOY FROM HERE */


const float EDGE_THICKNESS = .2;
const float WIDTH = 1.;
const float RADIUS = 3.;
const float CHANNEL_DEPTH_RATIO = 1.;
const float BALL_COUNT = 19.;
const float BALL_SIZE_RATIO = 1.;
const float BALL_SPEED = -5.;
const float TWISTS = .5;
const float TWIST_SPEED = 1.;


// --------------------------------------------------------
// IQ
// https://www.shadertoy.com/view/ll2GD3
// --------------------------------------------------------

vec3 pal( in float t, in vec3 a, in vec3 b, in vec3 c, in vec3 d ) {
    return a + b*cos( 6.28318*(c*t+d) );
}

vec3 spectrum(float n) {
    return pal( n, vec3(0.5,0.5,0.5),vec3(0.5,0.5,0.5),vec3(1.0,1.0,1.0),vec3(0.0,0.33,0.67) );
}


// --------------------------------------------------------
// Modelling utilities
// hg_sdf https://www.shadertoy.com/view/Xs3GRB
// --------------------------------------------------------

#define PI 3.14159265359

void pR(inout vec2 p, float a) {
    p = cos(a)*p + sin(a)*vec2(p.y, -p.x);
}

float vmax(vec2 v) {
    return max(v.x, v.y);
}

float vmax(vec3 v) {
    return max(max(v.x, v.y), v.z);
}

float vmin(vec3 v) {
    return min(min(v.x, v.y), v.z);
}

float fBox2(vec2 p, vec2 b) {
    vec2 d = abs(p) - b;
    return length(max(d, vec2(0))) + vmax(min(d, vec2(0)));
}

float fBox(vec3 p, vec3 b) {
    vec3 d = abs(p) - b;
    return length(max(d, vec3(0))) + vmax(min(d, vec3(0)));
}

// Capsule: A Cylinder with round caps on both sides
float fCapsule(vec3 p, float r, float c) {
    return mix(length(p.xz) - r, length(vec3(p.x, abs(p.y) - c, p.z)) - r, step(c, abs(p.y)));
}

float fHalfCapsule(vec3 p, float r) {
    return mix(length(p.xz) - r, length(p) - r, step(0., p.y));
}

float fHalfCapsule(vec2 p, float r) {
    return mix(length(p.x) - r, length(p) - r, step(0., p.y));
}

// float smin(float a, float b, float r) {
//     vec2 u = max(vec2(r - a,r - b), vec2(0));
//     return max(r, min (a, b)) - length(u);
// }

// float smax(float a, float b, float r) {
//     vec2 u = max(vec2(r + a,r + b), vec2(0));
//     return min(-r, max (a, b)) + length(u);
// }

float smin(float a, float b, float k){
    float f = clamp(0.5 + 0.5 * ((a - b) / k), 0., 1.);
    return (1. - f) * a + f  * b - f * (1. - f) * k;
}

float smax(float a, float b, float k) {
    return -smin(-a, -b, k);
}

float smin(float a, float b) {
    return smin(a, b, .0);
}

float smax(float a, float b) {
    return smax(a, b, 0.);
}


vec3 cartToPolar(vec3 p) {
    float x = p.x; // distance from the plane it lies on
    float r = length(p.zy); // distance from center
    float a = atan(p.y, p.z); // angle around center
    return vec3(x, r, a);
}

vec3 polarToCart(vec3 p) {
    return vec3(
        p.x,
        sin(p.z) * p.y,
        cos(p.z) * p.y
    );
}


// --------------------------------------------------------
// Model
// --------------------------------------------------------

struct Model {
    float dist;
    vec3 material;
};

Model mapA(vec3 p) {
    float d;
    float c = length(p.xy) - .5;

    float lead = 20.;
    float strands = 2.;

    d = abs(
        sin((atan(p.y,p.x)-p.z * lead) / strands)
        * min(1., length(p.xy))
    ) / (lead / strands) - .02;
    d = max(d, c);

    return Model(d, vec3(.8));
}

float helix(vec3 p, float lead, float thick) {
    p.z += iTime * .1;
    float d = (mod(atan(p.y, p.x) - p.z * lead, PI * 2.) - PI) / lead;
    d = abs(d) - thick;
    return d;
}

float ellip(vec3 p, vec3 s) {
    float r = vmin(s);
    p *= r / s;
    return length(p) - r;
}

vec3 modelAlbedo;

float map(vec3 p) {
    vec3 pp = p;

    p += vec3(0,.25,.1);
    pR(p.xy, -.05);
    pR(p.yz, -.05);
    p.x *= .95;
    float neck = fHalfCapsule(p, .235);
    p = pp;

    p.z -= .01;
    p.y -= .08;

    vec3 pa = p;
    p.x = abs(p.x);
    pp = p;

    modelAlbedo = vec3(.8);

    float d = 1e12;

    // skull back
    p += vec3(0,-.135,.09);
    d = ellip(p, vec3(.39, .38, .39));

    // skull base
    p += vec3(0,.1,.07);
    d = smin(d, ellip(p, vec3(.38, .36, .35)), .05);

    // p = pp;
    // p += vec3(-.12,.14,.2);
    // d = smin(d, length(p) - .2, .0);

    // forehead
    p = pp;
    p += vec3(0,-.145,-.175);
    d = smin(d, ellip(p, vec3(.315, .3, .33)), .18);

    // face base
    p = pp;
    p += vec3(0,.25,-.15);
    d = smin(d, length(p) - .28, .1);

    // behind ear
    p = pp;
    p += vec3(-.15,.13,.06);
    d = smin(d, ellip(p, vec3(.15,.15,.15)), .15);

    // cheek base
    p = pp;
    p += vec3(-.2,.14,-.14);
    d = smin(d, ellip(p, vec3(.15,.22,.2) * .8), .15);

    // // jaw
    // p = pp;
    // pR(p.yz, .14);
    // float jaw = p.z - .48;
    // pR(p.xz, .5);
    // jaw = smax(jaw, p.x - .35, .08);
    // p = pp;
    // pR(p.yz, .5);
    // jaw = smax(jaw, -p.y - .438, .2);
    // p = pp;
    // pR(p.yz, -.0);
    // jaw = smax(jaw, -p.z - .05, .15);
    // p = pp;
    // jaw = smax(jaw, p.y + .1, .25);
    // p = pp;
    // p += vec3(0,.35,-.2);
    // jaw = smin(jaw, length(p) - .23, .1);
    // d = smin(d, jaw, .1);

    // jaw base
    p = pp;
    p += vec3(0,.49,-.2);
    pR(p.yz, .6);
    d = smin(d, ellip(p, vec3(.2,.1,.2)), .1);

    // chin
    p = pp;
    p += vec3(0,.56,-.38);
    p.x *= .8;
    d = smin(d, length(p) - .04, .15);



    // brow
    p = pp;
    p += vec3(0,-.0,-.215);
    float brow = fHalfCapsule(p * vec3(.65,1,.9), .27);
    brow = smax(brow, p.x - .4, .26);
    float sb = length(p + vec3(0,-.02,-.25));
    pR(p.yz, -.5);
    brow = smax(brow, -p.y - .145, mix(.05, .3, smoothstep(.3, .6, sb)));
    d = smin(d, brow, .06);

    // cheekbone
    p = pp;
    p += vec3(-.15,.15,-.1);
    // d = smin(d, length(p) - .25, .0);


    // d = square;

    // d = min(d, sb - .59);

    
    

    // p = pp;
    // d = min(d, abs(p.x) - .001);
    // d = max(d, length(p) - .7);

    // d = min(d, square);

    // eye socket
    p = pp;
    p += vec3(-.1,.08,-.5);
    // pR(p.xy, -.2);
    // pR(p.xz, -.2);
    p.x *= .5;
    // d = smax(d, -length(p.yz) + .001, .2);
    // d = smin(d, length(p.yz) - .05, .0);

    // brow
    p = pp;
    p += vec3(-.15,.0,-.38);
    pR(p.yz, -.3);
    pR(p.xz, -1.5);
    pR(p.yz, -.1);
    p.x *= .5;
    p.z *= .5;
    // d = smin(d, length(p) - .005, .15);
    // d = smin(d, length(p) - .1, .0);

    // brow
    p = pp;
    p += vec3(0,.0,-.38);
    pR(p.yz, -.3);
    // pR(p.xz, 1.5);
    // pR(p.yz, -.1);
    p.x *= .15;
    p.z *= .3;
    // d = smin(d, length(p) - .0001, .12);
    // d = smin(d, length(p) - .05, .0);

    // chin
    p = pp;
    p += vec3(0,.5,-.25);
    // d = smin(d, length(p) - .06, .3);

    p = pp;
    p += vec3(0,.59,-.38);
    p.x *= .8;
    // d = smin(d, length(p) - .02, .25);

    // jaw line
    p = pp;
    p += vec3(-.15,.45,-.15);
    pR(p.yz, .7);
    pR(p.xz, .4);
    p.z *= .5;
    // d = smin(d, length(p) - .005, .2);

    // jaw point
    p = pp;
    p += vec3(-.15,.33,-.1);
    // d = smin(d, length(p) - .06, .28);

    // cheek
    p = pp;
    p += vec3(0,.3,-.21);
    // d = smin(d, length(p) - .2, .2);

    // cheekbone
    p = pp;
    p += vec3(-.18,.21,-.26);
    // p.xz *= .9;
    // d = smin(d, length(p) - .06, .2);
    // d = smin(d, length(p) - .12, .0);

    // lips
    p = pp;
    p.x *= .7; 
    p += vec3(0,.39,-.42);
    // d = smin(d, length(p) - .08, .05);

    // nose
    p = pp;
    p += vec3(0,.2,-.48);
    p.y *= .3; 
    // d = smin(d, length(p) - .03, .1);

    p = pp;
    p += vec3(0,.26,-.52);
    pR(p.yz, .6);
    p.z *= .7;
    // d = smin(d, length(p) - .05, .06);



/*

    float r = .515;

    p.yz *= .9;

    d = length(p) - r * .9;

    p = pp;

    pR(p.xz, .35);
    float o = sqrt(r * r - (2./9.) * r);
    float sideR = r * 6.;
    p -= vec3(o - sideR + .07, 0, r * 1.1);
    float side = length(p) - sideR;
    d = smax(d, side, .1);

    p = pp;
    p -= vec3(o - sideR + .2, r * 1.3, r * 1.1);
    side = length(p) - sideR;
    d = smax(d, side, .15);

    p = pp;
    p -= vec3(o + .1, .01, .26);
    float temple = length(p) - .01;
    d = smax(d, -temple, .25);
*/

    // p = pp;
    // d = min(d, abs(p.y) - .002);

    // p.y += 2./3. * r;
    // d = min(d, abs(p.y) - .002);

    // p.y += 2./3. * r;
    // d = min(d, abs(p.y) - .002);

    // p = pp;
    // d = max(d, length(p.xz) - r * 1.5);

    if (guiNeck) {
        p = pa;
        p += vec3(.18,.57,-.1);
        float nb = length(p);
        d = smin(d, neck, mix(.13, .2, smoothstep(.1, .3, nb)));
        // d = min(d, nb - .05);
    }

    return d;

    // d = min(d, length(p) - .2);

    // p.y += .07;
    // p.z -= .53;
    // p.x -= .16;
    // pR(p.xy, -.2);
    // pR(p.xz, -.5);
    // p.x *= .5;
    // d = smax(d, -(length(p) - .1), .05);

    // d = min(d, fBox(p, vec3(.4)));

    // d = min(d, max(length(p) - .7, abs(p.x) - .001));
    // d = min(d, max(length(p) - .7, abs(p.y) - .001));
    // d = min(d, max(length(p) - .7, abs(p.z) - .001));

    // p.y -= .5;

    // d = min(d, length(p) - .2);

    return d;

    // float h = helix(p, 30., .05);

    // p.z /= 1.2;
    // float d = length(p) - .5;

    // p.x -= .55;
    // d = smin(d, length(p) - .1, .1);

    // p = pp;
    // p.z -= .7;
    // p.z /= 2.;
    // d = smin(d, length(p) - .25, .1);

    // p = pp;
    // p.z -= 2.2;
    // d = smin(d, length(p) - 1.5, .1);

    // d = abs(d + .01) - .01;
    
    // d = max(d, h);

    // return d;
}


// --------------------------------------------------------
// Rendering
// --------------------------------------------------------

struct Hit {
    Model model;
    vec3 pos;
    bool isBackground;
    vec3 normal;
    vec3 rayOrigin;
    float rayLength;
    vec3 rayDirection;
};

float calcAO( in vec3 pos, in vec3 nor )
{
    float occ = 0.0;
    float sca = 1.0;
    for( int i=0; i<5; i++ )
    {
        float hr = 0.01 + 0.12*float(i)/4.0;
        vec3 aopos =  nor * hr + pos;
        float dd = map( aopos );
        occ += -(dd-hr)*sca;
        sca *= 0.95;
    }
    return clamp( 1.0 - 3.0*occ, 0.0, 1.0 );
}

vec3 render(Hit hit, vec3 col) {
    if ( ! hit.isBackground) {
        // The simple ambient occlusion method results in hot spots
        // at the base and sides of the balls. This is a result of
        // the limited samples we do across the normal. In reality
        // there would be a more evenly distributed darkness along
        // the base of the channell; so here it's faked with the uv
        // coordinates and blended in.
        float ao = calcAO(hit.pos, hit.normal);
        float light = dot(normalize(vec3(1,1,0)), hit.normal) * .5 + .5;
        float diff = light * ao;
        vec3 diffuse = mix(vec3(.5,.5,.6) * .7, vec3(1), diff);
        col = hit.model.material * diffuse;
        col = hit.normal * .5 + .5;

        // col = vec3(1) * pow(clamp(dot(vec3(0,1.5,.5), hit.normal) * .5 + .5, 0., 1.), 1./2.2);
        // col = vec3(1,0,0);

    }
    return col;
}


// --------------------------------------------------------
// Ray Marching
// Adapted from: https://www.shadertoy.com/view/Xl2XWt
// --------------------------------------------------------

const float MAX_TRACE_DISTANCE = 10.;
const float INTERSECTION_PRECISION = .00001;
const int NUM_OF_TRACE_STEPS = 1500;

const int NORMAL_STEPS = 6;
vec3 calcNormal(vec3 pos){
    vec3 eps = vec3(.0001,0,0);
    vec3 nor = vec3(0);
    float invert = 1.;
    for (int i = 0; i < NORMAL_STEPS; i++){
        nor += map(pos + eps * invert) * eps * invert;
        eps = eps.zxy;
        invert *= -1.;
    }
    return normalize(nor);
}

Hit raymarch(vec3 rayOrigin, vec3 rayDirection){

    float currentDist = INTERSECTION_PRECISION * 2.0;
    float rayLength = 0.;

    for(int i = 0; i < NUM_OF_TRACE_STEPS; i++){
        if (currentDist < INTERSECTION_PRECISION || rayLength > MAX_TRACE_DISTANCE) {
            break;
        }
        currentDist = map(rayOrigin + rayDirection * rayLength);
        rayLength += currentDist * (1. - .5);
    }

    bool isBackground = false;
    vec3 pos = vec3(0);
    vec3 normal = vec3(0);

    if (rayLength > MAX_TRACE_DISTANCE) {
        isBackground = true;
    } else {
        pos = rayOrigin + rayDirection * rayLength;
        normal = calcNormal(pos);
    }

    Model model = Model(currentDist, modelAlbedo);

    return Hit(
        model,
        pos,
        isBackground,
        normal,
        rayOrigin,
        rayLength,
        rayDirection
    );
}

mat3 calcLookAtMatrix(vec3 ro, vec3 ta, vec3 up) {
    vec3 ww = normalize(ta - ro);
    vec3 uu = normalize(cross(ww,up));
    vec3 vv = normalize(cross(uu,ww));
    return mat3(uu, vv, ww);
}

float getDepth(float depth) {
    depth = projection[3].z / (depth * -2. + 1. - projection[2].z);
    return depth;
}

void main() {

    float time = iTime;
    // time *= .333;
    time = mod(time, 1.);


    vec3 rayOrigin = eye;
    vec3 rayDirection = normalize(dir);
    vec3 rayPosition = rayOrigin;

    vec3 bg = vec3(.7,.8,.9) * 1.1;

    Hit hit = raymarch(rayOrigin, rayDirection);
    vec3 color = render(hit, bg);

    // color = vec3(0,0,1);
    // color = pow(color, vec3(1. / 2.2)); // Gamma

    float eyeHitZ = -hit.rayLength * dot(rayDirection, cameraForward);

    vec3 eyeSpace = vec3(0, 0, eyeHitZ);
    float zc = ( projection * vec4(eyeSpace, 1)).z;
    float wc = ( projection * vec4(eyeSpace, 1)).w;
    float depth = (zc/wc + 1.) / 2.;


    float polyD = getDepth(texture2D(uDepth, gl_FragCoord.xy / iResolution.xy).r);
    float rayD = getDepth(depth);

    if (guiBlend && ! hit.isBackground) {
        color = spectrum(smoothstep(.03, -.03, polyD - rayD));
    }

    float alpha = smoothstep(.06, -.06, polyD - rayD);

    // alpha = .5;

    if (polyD > rayD) {
        alpha = max(0., alpha - .1);
    }

    // alpha = .5;

    if ( ! guiBlend) {
        alpha = 1.;
    }

    // alpha = 0.;
    if (hit.pos.x < 0.) {
        // alpha = 1.;
    }

    vec3 polyColor = texture2D(uSource, gl_FragCoord.xy / iResolution.xy).rgb;
    color = mix(polyColor, color, alpha);

    if (abs(polyD - rayD) < .001) {
        // color = vec3(1);
    }



    gl_FragColor = vec4(color, 1);
    gl_FragDepthEXT = depth;
}
