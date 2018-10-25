precision highp float;

uniform vec2 iResolution;
uniform vec2 iOffset;
uniform float iTime;

uniform mat4 cameraMatrix;
uniform vec3 cameraPosition;

void mainImage(out vec4 a, in vec2 b);

void main() {
    mainImage(gl_FragColor, gl_FragCoord.xy + iOffset.xy);
}

#ifdef GL_ES
precision mediump float;
#endif


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

float fBox2(vec2 p, vec2 b) {
    vec2 d = abs(p) - b;
    return length(max(d, vec2(0))) + vmax(min(d, vec2(0)));
}

float smax(float a, float b, float r) {
    vec2 u = max(vec2(r + a,r + b), vec2(0));
    return min(-r, max (a, b)) + length(u);
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

Model map(vec3 p) {
    float d = length(p) - .5;
    p = mod(p, 1.);
    d = min(d, length(p) - .2);
    return Model(d, vec3(.8));
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
        float dd = map( aopos ).dist;
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
        nor += map(pos + eps * invert).dist * eps * invert;
        eps = eps.zxy;
        invert *= -1.;
    }
    return normalize(nor);
}

Hit raymarch(vec3 rayOrigin, vec3 rayDirection){

    float currentDist = INTERSECTION_PRECISION * 2.0;
    float rayLength = 0.;
    Model model;

    for(int i = 0; i < NUM_OF_TRACE_STEPS; i++){
        if (currentDist < INTERSECTION_PRECISION || rayLength > MAX_TRACE_DISTANCE) {
            break;
        }
        model = map(rayOrigin + rayDirection * rayLength);
        currentDist = model.dist;
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

void mainImage(out vec4 fragColor, in vec2 fragCoord) {

    float time = iTime;
    // time *= .333;
    time = mod(time, 1.);

    vec2 p = (-iResolution.xy + 2.0*fragCoord.xy)/iResolution.y;


    vec3 camPos = cameraPosition;
    mat4 camMat = cameraMatrix;
    float focalLength = 3.;
    vec3 rd = normalize(
        (vec4(p, -focalLength, 1) * camMat).xyz
    );
    vec3 rayDirection = rd;

    vec3 bg = vec3(.7,.8,.9) * 1.1;

    Hit hit = raymarch(camPos, rayDirection);
    vec3 color = render(hit, bg);

    color = pow(color, vec3(1. / 2.2)); // Gamma

    fragColor = vec4(color,1);
}
