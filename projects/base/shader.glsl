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


float vmax(vec3 v) {
    return max(max(v.x, v.y), v.z);
}

// Box: correct distance to corners
float fBox(vec3 p, vec3 b) {
    vec3 d = abs(p) - b;
    return length(max(d, vec3(0))) + vmax(min(d, vec3(0)));
}

// Same, but mirror every second cell so all boundaries match
vec2 pModMirror2(inout vec2 p, vec2 size) {
    vec2 halfsize = size*0.5;
    vec2 c = floor((p + halfsize)/size);
    p = mod(p + halfsize, size) - halfsize;
    p *= mod(c,vec2(2))*2. - vec2(1);
    return c;
}

// Same, but mirror every second cell so they match at the boundaries
float pModMirror1(inout float p, float size) {
    float halfsize = size*0.5;
    float c = floor((p + halfsize)/size);
    p = mod(p + halfsize,size) - halfsize;
    p *= mod(c, 2.0)*2. - 1.;
    return c;
}


struct Model {
    float dist;
};

// checks to see which intersection is closer
Model opU( Model m1, Model m2 ){
    if (m1.dist < m2.dist) {
        return m1;
    } else {
        return m2;
    }
}

Model map( vec3 p ){
    mat3 m = modelRotation();

    pModMirror2(p.xy, vec2(4.));
    pModMirror1(p.z, 4.);
    float d = length(p + vec3(.25)) - .75;
    d = min(d, fBox(p - vec3(.25), vec3(.5)));
    Model model = Model(d);
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
    camPos = vec3(0,0,-3.);
    camPos *= cameraRotation();
}



// --------------------------------------------------------
// Ray Marching
// Adapted from: https://www.shadertoy.com/view/Xl2XWt
// --------------------------------------------------------

const float ANTI_ALIAS = 0.1;
const float MAX_TRACE_DISTANCE = 30.; // max trace distance
const float INTERSECTION_PRECISION = .001; // precision of the intersection
const int NUM_OF_TRACE_STEPS = 100;
const float FUDGE_FACTOR = 1.; // Default is 1, reduce to fix overshoots

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
    bool isNone;
};

vec3 calcNormal( in vec3 pos ){
    vec3 eps = vec3( 0.001, 0.0, 0.0 );
    vec3 nor = vec3(
        map(pos+eps.xyy).dist - map(pos-eps.xyy).dist,
        map(pos+eps.yxy).dist - map(pos-eps.yxy).dist,
        map(pos+eps.yyx).dist - map(pos-eps.yyx).dist );
    return normalize(nor);
}

struct Hits {
    Hit a;
    Hit b;
};

Hits raymarch(CastRay castRay){

    float currentDist = INTERSECTION_PRECISION * 2.0;
    float lastDist = currentDist;
    float adjustModel = ANTI_ALIAS;

    Model model;

    Ray ray = Ray(castRay.origin, castRay.direction, 0.);
    float safeRayLen = ray.len;

    vec3 pos = vec3(0);

    Hit hitA = Hit(
        ray,
        model,
        pos,
        true
    );

    int step = 0;
    // 0 first pass shrunk model
    // 1 find background hit
    // 2 find foreground hit

    for (int i = 0; i < NUM_OF_TRACE_STEPS; i++) {
        if (ray.len > MAX_TRACE_DISTANCE) {
            break;
        }
        if (currentDist < INTERSECTION_PRECISION) {
            if (step == 0) {
                adjustModel = 0.;
                ray.len = safeRayLen;
                step = 2;
            }
            if (step == 1) {
                hitA = Hit(ray, model, pos, false);
                ray.len = safeRayLen;
                step = 2;
            }
            if (step == 2) {
                break;
            }
        }
        if (step == 0 && lastDist < currentDist && currentDist < ANTI_ALIAS) {
            step = 1;
        }
        if (step == 1 && lastDist < currentDist && currentDist > ANTI_ALIAS) {
            adjustModel = 0.;
        }
        if (step == 0 && currentDist > ANTI_ALIAS) {
            safeRayLen = ray.len;
        }
        pos = ray.origin + ray.direction * ray.len;
        model = map(pos);
        lastDist = currentDist;
        currentDist = model.dist + adjustModel;
        ray.len += currentDist * FUDGE_FACTOR;
    }

    Hit hitB = Hit(
        ray,
        model,
        ray.origin + ray.direction * ray.len,
        false
    );

    return Hits(hitA, hitB);
}



// --------------------------------------------------------
// Rendering
// --------------------------------------------------------

vec3 shadeHit(inout Hit hit){

    vec3 background = vec3(1,1,0);

    if (hit.ray.len > MAX_TRACE_DISTANCE) {
        return background;
    } else {
        return calcNormal(hit.pos) * .5 + .5;
    }
}

vec3 render(Hits hits){

    Hit hitA = hits.a;
    Hit hitB = hits.b;

    if (hitA.isNone) {
        return shadeHit(hitB);
    } else {
        vec3 colorA = shadeHit(hitA);
        vec3 colorB = shadeHit(hitB);
        return mix(
            colorA,
            colorB,
            0.
        );
    }

    // if (hit.isOutline) {
    //     hit.color = mix(
    //         hit.color,
    //         vec3(0),
    //         smoothstep(
    //             outlineSize,
    //             outlineSize * .5,
    //             hit.outlineDist
    //         )
    //     );
    // }


    // return color;
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

    time = iGlobalTime;

    doCamera();

    mat3 camMat = calcLookAtMatrix(camPos, camTar, camUp);
    float focalLength = 2.;
    vec3 rd = normalize(camMat * vec3(p, focalLength));
    Hits hits = raymarch(CastRay(camPos, rd));
    vec3 color = render(hits);
    color = linearToScreen(color);
    fragColor = vec4(color,1.0);
}
