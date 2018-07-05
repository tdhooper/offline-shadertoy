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
    vec2 uv;
    float underStep;
    int id;
};

// Imagine a plane with a channel cut in each side, if the plane were
// thin, the channels would cut through to the other side, forming
// a hole:
// ___     ___
// ___|   |___
//
// If the plane were thick, the channels would have depth, and
// if it were thick enough, they'd never intersect:
// ___     ___
//    \___/
//     ___
// ___/   \___
//
// We want to create a thin plane, but with channels that have depth,
// don't intersect, and don't create a hole. I've achieved this by
// only cutting the channel when we get close to where it would be.

// The threshold is the surface covering the channel:
// ___ ___ ___
//    \___/
//
// This gets set to true when the ray passes through that surface,
// into the channel
bool pastThreshold = false;

// This is the side of the plane we were on when we crossed
// the threshold
float thresholdSide = 0.;

float lastZ = 0.; // Last torus 'z' position or the ray
bool AO_PASS = false;
float time;

Model fModel(vec3 p) {

    vec3 pp = p;
    float twist = time * TWIST_SPEED;

    // Transform space into a torus knot
    p = cartToPolar(p);
    p.y -= RADIUS;
    p.z /= PI * 2.;
    pR(p.xy, (TWISTS * p.z + twist) * PI * 2.);

    // When the ray shoots past the 'join' in the torus,
    // flip the side so it matches up
    if (length(lastZ - p.z) > .5) {
        thresholdSide *= -1.;
    }
    lastZ = p.z;

    float round = EDGE_THICKNESS;

    // The base plane that we carve into
    float d = fBox2(
        p.xy,
        vec2(WIDTH, EDGE_THICKNESS - round)
    ) - round;

    float channelWidth = WIDTH - EDGE_THICKNESS / 2.;
    float channelDepth = channelWidth * CHANNEL_DEPTH_RATIO;
    float channelOffset = channelWidth - channelDepth;

    // Surface covering the channel, when the ray passes into this,
    // we know we're inside the channel
    float threshold = fBox2(
        p.xy,
        vec2(channelWidth + round, EDGE_THICKNESS + .002)
    );

    // When the ray passes into the channel for the first time,
    // record which side of the plane we were on.
    // Always assume we're past the threshold when calculating ambient
    // occlusion, as the channel never actually gets cut when
    // we're not inside it.
    if ((AO_PASS || threshold <= 0.) && ! pastThreshold) {
        pastThreshold = true;
        thresholdSide = sign(p.y);
    }

    float side = mix(sign(p.y), thresholdSide, abs(thresholdSide));

    // Cut the channel when we're past the threshold. This actually
    // constructs entirely new geometry, so we don't cut through to
    // the other side.
    if (pastThreshold) {
        float cut = length(
            p.xy - vec2(0, channelOffset) * side
        ) - channelWidth;
        d = fBox2(
            p.xy + vec2(0, thresholdSide * (channelDepth * 2. - EDGE_THICKNESS)),
            vec2(WIDTH, channelDepth * 2. - round)
        ) - round;
        d = smax(-cut, d, round);
    }

    // Rough uv mapping, used to correct the ambient occlusion
    vec2 uv = side * p.xy / (channelDepth * 2.) + vec2(0, .5);
    
    // A Möbius strip has a surface length of 2x it's diamater,
    // so increment our position when corssing over to the other
    // side of the plane
    if (side > 0.) {
        p.z += 1.;
    }
    p.z /= 2.;

    float repeat = BALL_COUNT;

    float ballOffset = (time / repeat) * BALL_SPEED;
    p.z += ballOffset;

    // Divide the strip up into cells
    float cell = floor((p.z + .5 / repeat) * repeat);

    // Ball position
    vec3 bp = vec3(0);
    bp.y = -channelOffset;
    bp.z = 2. * (cell / repeat - ballOffset);

    vec3 col = spectrum(bp.z / 2.);

    // Transform ball's torus position into cartesian space
    pR(bp.xy, -(TWISTS * p.z + twist) * PI * 2.);
    bp.y += RADIUS;
    bp.z *= PI * 2.;
    bp = polarToCart(bp);

    // Add the ball sdf, and colour it
    p = pp;
    float ballSize = channelWidth * BALL_SIZE_RATIO;
    float balls = length(p - bp) - ballSize;
    col = d < balls ? vec3(1) : col;
    d = min(d, balls);
    
    Model model = Model(d, col, uv, 0., 10);
    return model;
}

float focalLength;

Model map(vec3 p) {
    float scale = focalLength;
    p *= scale;
    pR(p.yz, 1.25);
    Model model = fModel(p);
    model.dist /= scale;
    return model;
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
    AO_PASS = true;
    if ( ! hit.isBackground) {
        // The simple ambient occlusion method results in hot spots
        // at the base and sides of the balls. This is a result of
        // the limited samples we do across the normal. In reality
        // there would be a more evenly distributed darkness along
        // the base of the channell; so here it's faked with the uv
        // coordinates and blended in.
        float ao = calcAO(hit.pos, hit.normal);
        float fakeAo = min(hit.model.uv.y * 3., 1.);
        ao = mix(ao, fakeAo, .7);
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
const float INTERSECTION_PRECISION = .0001;
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

    time = iTime;
    time *= .333;
    time = mod(time, 1.);

    vec2 p = (-iResolution.xy + 2.0*fragCoord.xy)/iResolution.y;

    vec3 camPos = vec3(2.5,0,3.5) * 1.25;
    vec3 camTar = vec3(-.5,0,0);
    vec3 camUp = vec3(1,0,0);
    mat3 camMat = calcLookAtMatrix(camPos, camTar, camUp);
    focalLength = 2.;
    vec3 rayDirection = normalize(camMat * vec3(p, focalLength));

    vec3 bg = vec3(.7,.8,.9) * 1.1;

    Hit hit = raymarch(camPos, rayDirection);
    vec3 color = render(hit, bg);

    color = pow(color, vec3(1. / 2.2)); // Gamma

    fragColor = vec4(color,1);
}
