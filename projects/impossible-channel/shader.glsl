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

float time;


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
// Utilities
// hg_sdf https://www.shadertoy.com/view/Xs3GRB
// --------------------------------------------------------

#define PI 3.14159265359
#define TAU 6.28318530718

#define saturate(x) clamp(x, 0., 1.)

void pR(inout vec2 p, float a) {
    p = cos(a)*p + sin(a)*vec2(p.y, -p.x);
}

void pR45(inout vec2 p) {
    p = (p + vec2(p.y, -p.x))*sqrt(0.5);
}

float pMod1(inout float p, float size) {
    float halfsize = size*0.5;
    float c = floor((p + halfsize)/size);
    p = mod(p + halfsize, size) - halfsize;
    return c;
}

float pModPolar(inout vec2 p, float repetitions) {
    float angle = 2.*PI/repetitions;
    float a = atan(p.y, p.x) + angle/2.;
    float r = length(p);
    float c = floor(a/angle);
    a = mod(a,angle) - angle/2.;
    p = vec2(cos(a), sin(a))*r;
    // For an odd number of repetitions, fix cell index of the cell in -x direction
    // (cell index would be e.g. -5 and 5 in the two halves of the cell):
    if (abs(c) >= (repetitions/2.)) c = abs(c);
    return c;
}

float vmax(vec2 v) {
    return max(v.x, v.y);
}

float vmax(vec3 v) {
    return max(max(v.x, v.y), v.z);
}

float fBox(vec3 p, vec3 b) {
    vec3 d = abs(p) - b;
    return length(max(d, vec3(0))) + vmax(min(d, vec3(0)));
}

float fBox2(vec2 p, vec2 b) {
    vec2 d = abs(p) - b;
    return length(max(d, vec2(0))) + vmax(min(d, vec2(0)));
}

// Don't round corners when distance is reduced
float fBoxy(vec2 p, vec2 s) {
    return vmax(abs(p) - s);
}

float smin(float a, float b, float r) {
    vec2 u = max(vec2(r - a,r - b), vec2(0));
    return max(r, min (a, b)) - length(u);
}

float smax(float a, float b, float r) {
    vec2 u = max(vec2(r + a,r + b), vec2(0));
    return min(-r, max (a, b)) + length(u);
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
    vec2 uv;
    float underStep;
    int id;
};

bool pastThreshold = false;
float thresholdSide = 0.;
float lastZ = 0.;



bool AO_PASS = false;

Model fModel(vec3 p) {

    vec3 pp = p;
    float curveLen = 14.;

    p = cartToPolar(p);
    p.y -= 3.;
    p.z /= PI * 2.;

    float rotate = -time;
    // rotate = 0.;

    pR(p.xy, p.z * PI + rotate * PI * 2.);

    float thick = .2;
    float width = 1.;
    float channelWidth = width - thick / 2.;
    float channelDepth = channelWidth / 1.;
    float channelOffset = channelWidth - channelDepth;
    float round = thick;

    float repeat = 19.;
    float ballSize = channelWidth * 1.;

    // round = 0.;

    // p.y += channelDepth/2.;

    // Impossible Channel
    // Carves through beyond the other side of a thi turface,
    // as if it had depth.

    if (length(lastZ - p.z) > .5) {
        thresholdSide *= -1.;
    }
    lastZ = p.z;

    p.x = abs(p.x);
    float d = fBox2(p.xy, vec2(width, thick - round)) - round;

    float threshold = fBox2(
        p.xy,
        vec2(channelWidth + round, thick + .002)
    );
    // threshold = max(threshold, -d);
    if (threshold <= 0. && ! pastThreshold) {
        pastThreshold = true;
        thresholdSide = sign(p.y);
    }

    float side = mix(sign(p.y), thresholdSide, abs(thresholdSide));
    float cut = length(p.xy - vec2(0, channelOffset) * side) - channelWidth;

    if (pastThreshold) {
        d = fBox2(p.xy + vec2(0, thresholdSide * (channelDepth * 2. - thick)), vec2(width, channelDepth * 2. - round)) - round;
        d = smax(-cut, d, round);
    }

    // d = max(d, p.x - width);

    float zScale = 36.;
    float bounceSpeed = 0.;
    float bounceRatio = 4.;
    // if (p.y > 0. || thresholdSide > 0. && ! (sign(p.y) < thresholdSide)) {
    //     p.z += .5 / repeat;
    // }
    // if (sign(p.y) != thresholdSide) {
    //     p.z += .5 / repeat;
    // }

    if (side > 0.) {
        p.z += 1.;
    }
    p.z /= 2.;

    // float bounce = abs(sin((p.z + time * 2.) * 2. * PI));

    vec3 col = spectrum(p.z);


    float tt = (time / repeat) * bounceSpeed;

    p.z += tt;

    float cell = pMod1(p.z, 1./repeat);


    vec3 bp = vec3(0,0,0);
    bp.z -= tt * 2.;
    bp.z += 2. * cell / repeat;

    cell /= repeat;
    col = spectrum(cell);

    bp.y -= channelOffset;
    pR(bp.xy, -(bp.z * PI + rotate * PI * 2.));
    bp.y += 3.;


    bp.z *= PI * 2.;
    bp = polarToCart(bp);

    p = pp;
    float balls = length(p - bp)- ballSize;

    col = mix(col, vec3(.8), step(d - balls, 0.));
    d = min(d, balls);


    Model model = Model(d, col, vec2(0), 0., 10);

    return model;
}

float focalLength;

Model map(vec3 p) {
    float scale = focalLength;
    p *= scale;
    Model model = fModel(p);
    // model.dist = min(model.dist, length(p) - .2);
    // model.dist = max(model.dist, -p.y);
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
    AO_PASS = true;
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
        float ao = calcAO(hit.pos, hit.normal);
        col = hit.model.material;
        vec3 diffuse = mix(vec3(.5,.5,.6) * 1., vec3(1), ao);
        diffuse = vec3(dot(normalize(vec3(1,1,0)), hit.normal) * .5 + .5);
        // diffuse *= mix(.7, 1., ao);
        col *= diffuse;
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

    vec3 camPos = cameraPosition;
    mat4 camMat = cameraMatrix;
    focalLength = 2.;
    
    // vec3 rayDirection = normalize(camMat * vec3(p, focalLength));
    vec3 rayDirection = normalize(
        (vec4(p, -focalLength, 1) * camMat).xyz
    );

    vec3 bg = vec3(1);

    Hit hit = raymarch(camPos, rayDirection);
    vec3 color = render(hit, bg);

    color = pow(color, vec3(1.,.9,.8)); // Brighten and tint a little blue
    color = pow(color, vec3(1. / 2.2)); // Gamma

    fragColor = vec4(color,1);
}
