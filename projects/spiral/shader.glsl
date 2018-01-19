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


/* SHADERTOY FROM HERE */


vec2 mousee;

#define MODEL_ROTATION vec2(.5, .5)
#define CAMERA_ROTATION vec2(.5, .5)

// 0: Defaults
// 1: Model
// 2: Camera
#define MOUSE_CONTROL 2

//#define DEBUG

float time;

#define PI 3.14159265359
#define HALF_PI 1.5707963267948966
#define TAU 6.28318530718
#define PHI 1.618033988749895


// --------------------------------------------------------
// Rotation controls
// --------------------------------------------------------

mat3 sphericalMatrix(float theta, float phi) {
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

mat3 mouseRotation(bool enable, vec2 xy) {
    if (enable) {
        vec2 mouse = mousee.xy / iResolution.xy;

        if (mouse.x != 0. && mouse.y != 0.) {
            xy.x = mouse.x;
            xy.y = mouse.y;
        }
    }
    float rx, ry;

    xy.x -= .5;
    //xy *= 2.;

    rx = (xy.y + .5) * PI;
    ry = (-xy.x) * 2. * PI;

    return sphericalMatrix(rx, ry);
}

mat3 modelRotation() {
    mat3 m = mouseRotation(MOUSE_CONTROL==1, MODEL_ROTATION);
    return m;
}

mat3 cameraRotation() {
    mat3 m = mouseRotation(MOUSE_CONTROL==2, CAMERA_ROTATION);
    return m;
}


// --------------------------------------------------------
// Modelling
// --------------------------------------------------------

// Rotate around a coordinate axis (i.e. in a plane perpendicular to that axis) by angle <a>.
// Read like this: R(p.xz, a) rotates "x towards z".
// This is fast if <a> is a compile-time constant and slower (but still practical) if not.
void pR(inout vec2 p, float a) {
    p = cos(a)*p + sin(a)*vec2(p.y, -p.x);
}

float vmax(vec2 v) {
    return max(v.x, v.y);
}

float vmax(vec3 v) {
    return max(max(v.x, v.y), v.z);
}

// Box: correct distance to corners
float fBox(vec3 p, vec3 b) {
    vec3 d = abs(p) - b;
    return length(max(d, vec3(0))) + vmax(min(d, vec3(0)));
}

float fBox2(vec2 p, vec2 b) {
    vec2 d = abs(p) - b;
    return length(max(d, vec2(0))) + vmax(min(d, vec2(0)));
}

// Repeat space along one axis. Use like this to repeat along the x axis:
// <float cell = pMod1(p.x,5);> - using the return value is optional.
float pMod1(inout float p, float size) {
    float halfsize = size*0.5;
    float c = floor((p + halfsize)/size);
    p = mod(p + halfsize, size) - halfsize;
    return c;
}

// Cylindrical coordinates
vec3 cartToPolar(vec3 p) {
    p = p.zxy;
    float r = length(p.xy); // distance from center
    float z = p.z; // distance from the plane it lies on
    float a = atan(p.y, p.x); // angle around center
    // r = pow(r, .7);
    return vec3(a, z, r);
}

float globalScale;
bool debug = false;

vec3 pModSpiral(inout vec3 p, float flip) {
    float scale = .25;
    globalScale *= scale;

    float spacing = mix(.5, 3., sin(time * 2.) * .5 + .5);
    spacing = 2. * time;
    float a = atan(spacing / PI) * -1.;

    // p.x *= spacing;

    // pMod1(p.y, PI * .5);


    p /= scale;
    p = p.yxz;

    p = cartToPolar(p);


    // a = PI * -.25;
    
    // p.x *= 3.;
    // p.y /= spacing;
    pR(p.xy, a * flip);

    

    float repeat = cos(a) * spacing * 2.;
    float c = pMod1(p.y, repeat);
    // float halfsize = repeat * .5;
    // float c = floor((p.y + halfsize) / repeat);
    // p.y = mod(p.y + halfsize, repeat) - halfsize;

    // float len = sqrt(pow(spacing, 2.) - pow(repeat, 2.));
    float len = sqrt(pow(spacing, 2.) + pow(PI, 2.));
    // float len = sqrt(pow(spacing, 2.) - pow(repeat, 2.));
    // float len = sqrt(pow(repeat, 2.) + pow(PI, 2.));

    float offset = repeat / tan(PI * .5 - a);
    p.x -= (offset + len * 2.) * c * flip;
    // p.x += len;
    

    // p.z -= mix(2., 10., sin(p.x * .1 + time * 3.) * .5 + .5);
    p.z -= 1.5;

    // p.x += time;
    // p.x -= 13.1 * c - (spacing * c * .75);
    // p.x -= PI * scale * .5;
    // float len = 

    return vec3(0., 0., 0.);
}


struct Model {
    float dist;
    vec3 albedo;
};



Model map( vec3 p ){
    mat3 m = modelRotation();

    float slice = dot(p, vec3(0,0,1));

    globalScale = 1.;
    vec3 mm = pModSpiral(p, 1.);
    pModSpiral(p, -1.);
    // pModSpiral(p, 1.);
    // pModSpiral(p, -1.);
    // pModSpiral(p, 1.);
    vec3 color = sign(p) * .5 + .5;
    // color = vec3(smoothstep(.25, .3, abs(mod(p.x, .5) - .25) * 4.), 0., 1.);
    // color = vec3(
    //     smoothstep(0., .1, sin(p.x * 7.)),
    //     smoothstep(0., .1, sin(p.x * 3.)),
    //     smoothstep(0., .1, sin(p.x * 4.))
    // );
    color = vec3(
        smoothstep(0., .1, sin(p.x * 20.)),
        sin(p.x / 1.5),
        sin(p.x / 3.)
    );
    // color = mm;

    float d = fBox2(p.yz, vec2(.5));
    // float d = fBox(p, vec3(1., .5, .5));
    d = length(p.yz) - .5;
    d *= globalScale;

    // d = max(d, slice);

    Model model = Model(d, color);
    return model;
}


// --------------------------------------------------------
// Camera
// --------------------------------------------------------

vec3 camPos;
vec3 camTar;
vec3 camUp;


void doCamera() {
    camUp = vec3(0,-1,0);
    camTar = vec3(0.);
    camPos = vec3(0,0,-5.);
    camPos *= cameraRotation();
}



// --------------------------------------------------------
// Ray Marching
// Adapted from: https://www.shadertoy.com/view/Xl2XWt
// --------------------------------------------------------

const float MAX_TRACE_DISTANCE = 30.; // max trace distance
const float INTERSECTION_PRECISION = .00001; // precision of the intersection
const int NUM_OF_TRACE_STEPS = 1000;
const float FUDGE_FACTOR = .8; // Default is 1, reduce to fix overshoots

struct CastRay {
    vec3 origin;
    vec3 direction;
};

struct Ray {
    vec3 origin;
    vec3 direction;
    float len;
};

struct Hit {
    Ray ray;
    Model model;
    vec3 pos;
    bool isBackground;
    vec3 normal;
    vec3 color;
};

vec3 calcNormal( in vec3 pos ){
    vec3 eps = vec3( 0.001, 0.0, 0.0 );
    vec3 nor = vec3(
        map(pos+eps.xyy).dist - map(pos-eps.xyy).dist,
        map(pos+eps.yxy).dist - map(pos-eps.yxy).dist,
        map(pos+eps.yyx).dist - map(pos-eps.yyx).dist );
    return normalize(nor);
}

Hit raymarch(CastRay castRay){

    float currentDist = INTERSECTION_PRECISION * 2.0;
    Model model;

    Ray ray = Ray(castRay.origin, castRay.direction, 0.);

    for( int i=0; i< NUM_OF_TRACE_STEPS ; i++ ){
        if (currentDist < INTERSECTION_PRECISION || ray.len > MAX_TRACE_DISTANCE) {
            break;
        }
        model = map(ray.origin + ray.direction * ray.len);
        currentDist = model.dist;
        ray.len += currentDist * FUDGE_FACTOR;
    }

    bool isBackground = false;
    vec3 pos = vec3(0);
    vec3 normal = vec3(0);

    if (ray.len > MAX_TRACE_DISTANCE) {
        isBackground = true;
    } else {
        pos = ray.origin + ray.direction * ray.len;
        normal = calcNormal(pos);
    }

    return Hit(ray, model, pos, isBackground, normal, vec3(0));
}


// --------------------------------------------------------
// Rendering
// --------------------------------------------------------

void shadeSurface(inout Hit hit){

    vec3 background = vec3(.1)* vec3(.5,0,1);
    background = vec3(.2,.8,1.) * .9;
    if (hit.isBackground) {
        hit.color = background;
        return;
    }
    pR(hit.normal.xz, 2.75);
    hit.color = hit.normal * .5 + .5;
    hit.color = hit.model.albedo;
}


vec3 render(Hit hit){

    shadeSurface(hit);

    return hit.color;
}


// --------------------------------------------------------
// Camera
// https://www.shadertoy.com/view/Xl2XWt
// --------------------------------------------------------

mat3 calcLookAtMatrix( in vec3 ro, in vec3 ta, in vec3 up )
{
    vec3 ww = normalize( ta - ro );
    vec3 uu = normalize( cross(ww,up));
    vec3 vv = normalize( cross(uu,ww));
    return mat3( uu, vv, ww );
}



// --------------------------------------------------------
// Gamma
// https://www.shadertoy.com/view/Xds3zN
// --------------------------------------------------------

const float GAMMA = 1.;

vec3 gamma(vec3 color, float g) {
    return pow(color, vec3(g));
}

vec3 linearToScreen(vec3 linearRGB) {
    return gamma(linearRGB, 1.0 / GAMMA);
}




void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
    mousee = iMouse.xy;

    vec2 p = (-iResolution.xy + 2.0*fragCoord.xy)/iResolution.y;
    vec2 m = mousee.xy / iResolution.xy;

// debug = true;
    if (p.y < 0.) {
        debug = true;
    }

    time = iGlobalTime;

    doCamera();

    mat3 camMat = calcLookAtMatrix(camPos, camTar, camUp);
    float focalLength = 3.;
    vec3 rd = normalize(camMat * vec3(p, focalLength));
    Hit hit = raymarch(CastRay(camPos, rd));

    vec3 color = render(hit);
    color = linearToScreen(color);
    fragColor = vec4(color,1.0);
}
