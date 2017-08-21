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
// HG_SDF
// https://www.shadertoy.com/view/Xs3GRB
// --------------------------------------------------------

float vmax(vec3 v) {
    return max(max(v.x, v.y), v.z);
}

// Rotate around a coordinate axis (i.e. in a plane perpendicular to that axis) by angle <a>.
// Read like this: R(p.xz, a) rotates "x towards z".
// This is fast if <a> is a compile-time constant and slower (but still practical) if not.
void pR(inout vec2 p, float a) {
    p = cos(a)*p + sin(a)*vec2(p.y, -p.x);
}

float smax(float a, float b, float r) {
    float m = max(a, b);
    if ((-a < r) && (-b < r)) {
        return max(m, -(r - sqrt((r+a)*(r+a) + (r+b)*(r+b))));
    } else {
        return m;
    }
}

float smin(float a, float b, float r) {
    float m = min(a, b);
    if ((a < r) && (b < r) ) {
        return min(m, r - sqrt((r-a)*(r-a) + (r-b)*(r-b)));
    } else {
     return m;
    }
}


// Plane with normal n (n is normalized) at some distance from the origin
float fPlane(vec3 p, vec3 n, float distanceFromOrigin) {
    return dot(p, n) + distanceFromOrigin;
}

// Box: correct distance to corners
float fBox(vec3 p, vec3 b) {
    vec3 d = abs(p) - b;
    return length(max(d, vec3(0))) + vmax(min(d, vec3(0)));
}

// Cylindrical coordinates
vec3 cartToPolar(vec3 p) {
    float r = length(p.xy); // distance from center
    float z = p.z; // distance from the plane it lies on
    float a = atan(p.y, p.x); // angle around center
    return vec3(r, z, a);
}

float pReflect(inout vec3 p, vec3 planeNormal, float offset) {
    float t = dot(p, planeNormal)+offset;
    if (t < 0.) {
        p = p - (2.*t)*planeNormal;
    }
    return sign(t);
}



// --------------------------------------------------------
// Spectrum colour palette
// IQ https://www.shadertoy.com/view/ll2GD3
// --------------------------------------------------------

vec3 pal( in float t, in vec3 a, in vec3 b, in vec3 c, in vec3 d ) {
    return a + b*cos( 6.28318*(c*t+d) );
}

vec3 spectrum(float n) {
    return pal( n, vec3(0.5,0.5,0.5),vec3(0.5,0.5,0.5),vec3(1.0,1.0,1.0),vec3(0.0,0.33,0.67) );
}




// --------------------------------------------------------
// Materials
// --------------------------------------------------------

struct Material {
    vec3 albedo;
    bool specular;
    float transparency;
    float refractiveIndex;
    float reflection;
};


Material ceramicMaterial = Material(
    vec3(.5),
    false,
    0.,
    0.,
    0.
);  
Material waterMaterial = Material(
    vec3(0.),
    true,
    1.,
    1. / 1.333,
    // 1. / 1.1,
    0.
);
Material mirrorMaterial = Material(
    vec3(.7, .3, .0),
    false,
    0.,
    0.,
    1.
);


// --------------------------------------------------------
// Modelling
// --------------------------------------------------------

bool insideTransparency = false;
bool enableTransparency = true;

struct Model {
    float dist;
    Material material;
};

Model newModel() {
    return Model(
        1e12,
        ceramicMaterial
    );
}

// checks to see which intersection is closer
Model opU( Model m1, Model m2 ){
    if (m1.dist < m2.dist) {
        return m1;
    } else {
        return m2;
    }
}


Model mainModel(vec3 p) {
    float d = 1e12;
    float part;

    Model model = newModel();
    if ( ! enableTransparency) return model;

    pR(p.zx, -.5);
    pR(p.yz, -time);
    // pR(p.yx, sin(time*2.)* .5);
    
    // pR(p.xz, time);
    
    d = fBox(p, vec3(.2)) - .05;
    
    p.z += .07;

    float squish = 1.;
    p.z /= squish;
    part = length(p) - .3;
    d = part;

    p.z += .1;
    part = length(p) - .3;
    d = smax(d, -part, .02);
    
    // part = dot(p, vec3(1,0,0));
    // d = max(d, -part);
    

    d *= squish;
    
    if (insideTransparency) d *= -1.;
    model.dist = d;
    model.material = waterMaterial;
    return model;
}

Model map( vec3 p ){
    p *= modelRotation();
    Model model;
    model = mainModel(p);
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
    camPos = vec3(0,0,-1.);
    camPos *= cameraRotation();
}



// --------------------------------------------------------
// Ray Marching
// Adapted from: https://www.shadertoy.com/view/Xl2XWt
// --------------------------------------------------------

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
// Refraction from https://www.shadertoy.com/view/lsXGzH
// --------------------------------------------------------

void shadeSurface(inout Hit hit){
    
    vec3 color = vec3(.04,.045,.05);
    
    if (hit.isBackground) {
        hit.color = color;
        hit.color = hit.ray.direction * .5 + .5;
        // vec2 p = cartToPolar(hit.ray.direction).yz;
        vec3 pp = hit.ray.direction;
        
        pReflect(pp, normalize(-vec3(0,.5,.5)), 0.);
        pReflect(pp, normalize(-vec3(0,-.5,.5)), 0.);
        pReflect(pp, normalize(-vec3(.5,0,.5)), 0.);
        pReflect(pp, normalize(-vec3(-.5,0,.5)), 0.);
        
        vec2 p = pp.xy;

        float size = .01;
        float space = .1;

        p = mod(p, space);
        p -= vec2(space / 2.);
        float d = length(p);
        hit.color = vec3(smoothstep(size + .005, size, d));

        hit.color *= hit.ray.direction *.5 + .5;
        hit.color *= 2.;
        return;
    }

    hit.color = hit.model.material.albedo;
}

CastRay transparencyCastRay(Hit hit, float refractiveIndex) {
    float separation = 0.05;    
    vec3 rayDirection, rayOrigin;
    float startDistance;    
    rayDirection = refract(hit.ray.direction, hit.normal, refractiveIndex);
    if (rayDirection == vec3(0)) {
        rayDirection = reflect(hit.ray.direction, hit.normal);
    }
    startDistance = separation / abs(dot(rayDirection, hit.normal));
    rayOrigin = hit.pos + startDistance * rayDirection;
    return CastRay(rayOrigin, rayDirection);
}

bool isInsideTransparentModel(vec3 pos) {
    Model model = map(pos);
    return model.dist < 0. && model.material.transparency < 1.;
}

const float REFRACT_SAMPLES = 40.; // max trace distance


Hit renderTransparency(Hit hit) {

    float riMax = hit.model.material.refractiveIndex;
    float riMin = riMax * .7;
    float refractiveIndex;

    float wl;
    Hit hitOutside;
    vec3 color = vec3(0);

    CastRay castRay;

    for(float i = 0.; i < REFRACT_SAMPLES; i++){

        wl = i / REFRACT_SAMPLES;
        refractiveIndex = mix(riMin, riMax, wl);

        // First march ray through to the other side

        castRay = transparencyCastRay(hit, refractiveIndex);
        insideTransparency = true;
        enableTransparency = true;
        Hit hitInside = raymarch(castRay);
        float thickness = hitInside.ray.len;
        
        // Then march back out into the scene
        
        castRay = transparencyCastRay(hitInside, 1. / refractiveIndex);
        insideTransparency = false;
        enableTransparency = ! isInsideTransparentModel(castRay.origin);
        hitOutside = raymarch(castRay);
        enableTransparency = true;
        
        // Shade the final model
        
        shadeSurface(hitOutside);

        hitOutside.color *= spectrum(wl) * 4.;
        color += hitOutside.color;
    }

    color /= REFRACT_SAMPLES;
    hitOutside.color = color;

    // float core = thickness;
    // hitOutside.color = mix(hitOutside.color, vec3(1), core);
    
    return hitOutside;
}


void mixTransparency(inout Hit hit, Hit hit2) {
    hit.color = mix(hit.color, hit2.color, hit.model.material.transparency);
}

vec3 render(Hit hit){
    
    shadeSurface(hit);
    
    if (hit.isBackground) {
        return hit.color;
    }
    
    if (hit.model.material.transparency > 0.) {
        Hit hit2 = renderTransparency(hit);
        if ( ! hit2.isBackground && hit2.model.material.transparency > 0.) {
            Hit hit3 = renderTransparency(hit2);
            if ( ! hit3.isBackground && hit3.model.material.transparency > 0.) {
                Hit hit4 = renderTransparency(hit3);
                mixTransparency(hit3, hit4);
            }
            mixTransparency(hit2, hit3);
        }
        mixTransparency(hit, hit2);
    }

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

    time = iGlobalTime;

    doCamera();

    mat3 camMat = calcLookAtMatrix(camPos, camTar, camUp);
    float focalLength = 2.;
    vec3 rd = normalize(camMat * vec3(p, focalLength));
    Hit hit = raymarch(CastRay(camPos, rd));

    vec3 color = render(hit);
    color = linearToScreen(color);
    fragColor = vec4(color,1.0);
}
