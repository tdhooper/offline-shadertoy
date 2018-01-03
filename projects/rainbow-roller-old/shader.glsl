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



#define MODEL_ROTATION vec2(.5, .5)
#define LIGHT_ROTATION vec2(.4, .8)
#define CAMERA_ROTATION vec2(.5, .5)

// 0: Defaults
// 1: Model
// 2: Lighting
// 3: Camera
#define MOUSE_CONTROL 0

//#define DEBUG

float time;

// --------------------------------------------------------
// HG_SDF
// https://www.shadertoy.com/view/Xs3GRB
// --------------------------------------------------------

#define PI 3.14159265359
#define PHI (1.618033988749895)
#define TAU 6.283185307179586

#define GDFVector0 vec3(1, 0, 0)
#define GDFVector1 vec3(0, 1, 0)
#define GDFVector2 vec3(0, 0, 1)

#define GDFVector3 normalize(vec3(1, 1, 1 ))
#define GDFVector4 normalize(vec3(-1, 1, 1))
#define GDFVector5 normalize(vec3(1, -1, 1))
#define GDFVector6 normalize(vec3(1, 1, -1))

#define GDFVector7 normalize(vec3(0, 1, PHI+1.))
#define GDFVector8 normalize(vec3(0, -1, PHI+1.))
#define GDFVector9 normalize(vec3(PHI+1., 0, 1))
#define GDFVector10 normalize(vec3(-PHI-1., 0, 1))
#define GDFVector11 normalize(vec3(1, PHI+1., 0))
#define GDFVector12 normalize(vec3(-1, PHI+1., 0))

#define GDFVector13 normalize(vec3(0, PHI, 1))
#define GDFVector14 normalize(vec3(0, -PHI, 1))
#define GDFVector15 normalize(vec3(1, 0, PHI))
#define GDFVector16 normalize(vec3(-1, 0, PHI))
#define GDFVector17 normalize(vec3(PHI, 1, 0))
#define GDFVector18 normalize(vec3(-PHI, 1, 0))

float vmax(vec3 v) {
    return max(max(v.x, v.y), v.z);
}

// Rotate around a coordinate axis (i.e. in a plane perpendicular to that axis) by angle <a>.
// Read like this: R(p.xz, a) rotates "x towards z".
// This is fast if <a> is a compile-time constant and slower (but still practical) if not.
void pR(inout vec2 p, float a) {
    p = cos(a)*p + sin(a)*vec2(p.y, -p.x);
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


// --------------------------------------------------------
// https://github.com/stackgl/glsl-inverse
// --------------------------------------------------------

mat3 inverse(mat3 m) {
  float a00 = m[0][0], a01 = m[0][1], a02 = m[0][2];
  float a10 = m[1][0], a11 = m[1][1], a12 = m[1][2];
  float a20 = m[2][0], a21 = m[2][1], a22 = m[2][2];

  float b01 = a22 * a11 - a12 * a21;
  float b11 = -a22 * a10 + a12 * a20;
  float b21 = a21 * a10 - a11 * a20;

  float det = a00 * b01 + a01 * b11 + a02 * b21;

  return mat3(b01, (-a22 * a01 + a02 * a21), (a12 * a01 - a02 * a11),
              b11, (a22 * a00 - a02 * a20), (-a12 * a00 + a02 * a10),
              b21, (-a21 * a00 + a01 * a20), (a11 * a00 - a01 * a10)) / det;
}


// --------------------------------------------------------
// http://math.stackexchange.com/a/897677
// --------------------------------------------------------

mat3 orientMatrix(vec3 A, vec3 B) {
    mat3 Fi = mat3(
        A,
        (B - dot(A, B) * A) / length(B - dot(A, B) * A),
        cross(B, A)
    );
    mat3 G = mat3(
        dot(A, B),              -length(cross(A, B)),   0,
        length(cross(A, B)),    dot(A, B),              0,
        0,                      0,                      1
    );
    return Fi * G * inverse(Fi);
}



// --------------------------------------------------------
// IQ
// https://www.shadertoy.com/view/ll2GD3
// --------------------------------------------------------

vec3 pal( in float t, in vec3 a, in vec3 b, in vec3 c, in vec3 d ) {
    return a + b*cos( 6.28318*(c*t+d) );
}

vec3 spectrum(float n) {
    return pal(
        n,
        vec3(0.5,0.5,0.5),
        vec3(0.5,0.5,0.5),
        vec3(1.0,1.0,1.0),
        vec3(0.0,0.33,0.67)
    );
}


// --------------------------------------------------------
// Rotation
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
        vec2 mouse = iMouse.xy / iResolution.xy;

        if (mouse.x != 0. && mouse.y != 0.) {
            xy.x = mouse.x;
            xy.y = mouse.y;
        }
    }
    float rx, ry;
    
    rx = (xy.y + .5) * PI;
    ry = (-xy.x) * 2. * PI;
    
    return sphericalMatrix(rx, ry);
}

mat3 modelRotation() {
    mat3 m = mouseRotation(MOUSE_CONTROL==1, MODEL_ROTATION);
    return m;
}

mat3 lightRotation() {
    mat3 m = mouseRotation(MOUSE_CONTROL==2, LIGHT_ROTATION);
    return m;
}

mat3 cameraRotation() {
    mat3 m = mouseRotation(MOUSE_CONTROL==3, CAMERA_ROTATION);
    return m;
}


// --------------------------------------------------------
// Modelling 
// --------------------------------------------------------

struct Material {
    vec3 albedo;
    bool specular;
    float transparency;
    float refractiveIndex;
    float reflection;
};

struct Model {
    float dist;
    Material material;
};

Material ceramicMaterial = Material(
    vec3(.5),
    false,
    0.,
    0.,
    0.
);  
Material waterMaterial = Material(
    vec3(.3, .7, .9),
    true,
    1.,
    1. / 1.222,
    0.
);
Material mirrorMaterial = Material(
    vec3(.7, .3, .0),
    false,
    0.,
    0.,
    1.
);

Model newModel() {
    return Model(
        10000.,
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

bool insideTransparency = false;
bool enableTransparency = true;


float waveSlow(vec3 p, vec3 v) {
    float t = time;
    float time = time / 5.;

    time = mod(time, 3.);
    
    if (time > 1.) {
        time = 1. + (time - 1.) / 2.;
    }
    
    float len = sin(time * PI) * .5 + .5;
    len = abs(mod(time - .5, 2.) - 1.);
    
    len = len * 8. + 8.;
    
    float offset = (-len * PI);
    
    if (time < 1.5 && time > .5) {
        offset = 0.;
    }      
    
    float angle = (acos(dot(normalize(p), v)));

    float waveA = cos(angle * len + offset);
    return waveA;
}


float wave1(vec3 p, vec3 v) {    
    float angle = acos(dot(normalize(p), v));
    float waveA = 0.;
    //waveA += cos(angle * 6. * 1.)*2.;
    waveA += cos(angle * 6. * 2.)*4.;
    //waveA += cos(angle * 6. * 3.)*2.;
    //waveA += cos(angle * 6. * 4.)*2.;
    //waveA += cos(angle * 6. * 12.)*.3;
    return waveA;
}

float wave2(vec3 p, vec3 v) {    
    float angle = acos(dot(normalize(p), v));
    float waveA = 0.;
    waveA += cos(angle * 6. * 1.)*6.;
    //waveA += cos(angle * 6. * 2.)*3.;
    //waveA += cos(angle * 6. * 3.)*2.;
    //waveA += cos(angle * 6. * 4.)*2.;
    waveA += cos(angle * 6. * 30.)*.2;
    return waveA;
}

float wave(vec3 p, vec3 v) {    
    return wave1(p, v);
}

Model modelC(vec3 p) {
    //pR(p.xz, time * PI*2.);
    //pR(p.xz, PI/5.7);

    Model model = newModel();
    vec3 a = vec3(1,0,0);
    float w = 0.;

    w += wave(p, GDFVector13);
    w += wave(p, GDFVector14);
    w += wave(p, GDFVector15);
    w += wave(p, GDFVector16);
    w += wave(p, GDFVector17);
    w += wave(p, GDFVector18);
    
    //w += wave(p, normalize(vec3(-1.,0.,-1./sqrt(2.))));
    //w += wave(p, normalize(vec3(1.,0.,-1./sqrt(2.))));
    //w += wave(p, normalize(vec3(0.,1.,1./sqrt(2.))));
    //w += wave(p, normalize(vec3(0.,-1.,1./sqrt(2.))));

    
    //w += wave(p, GDFVector3);
    //w += wave(p, GDFVector4);
    //w += wave(p, GDFVector5);
    //w += wave(p, GDFVector6);
    //w += wave(p, GDFVector7);
    //w += wave(p, GDFVector8);
    //w += wave(p, GDFVector9);
    //w += wave(p, GDFVector10);
    //w += wave(p, GDFVector11);
    //w += wave(p, GDFVector12);
    
    float r = w * .005 + .6;
    //r = .5;
    model.dist = length(p) - r;
    //model.material.albedo = vec3(mod((atan(dot(normalize(p), GDFVector13))/2.)+.5, 1.));
    return model;
}

Model modelA(vec3 p) {
    float d = 10000.;
    Model model = newModel();
    if ( ! enableTransparency) return model;

    model = modelC(p);

    if (insideTransparency) model.dist *= -1.;
    model.material = waterMaterial;
    return model;
}

Model map( vec3 p ){
    mat3 m = modelRotation();
    p *= m;    
    Model model = modelA(p);
    return model;
}

// --------------------------------------------------------
// LIGHTING
// https://www.shadertoy.com/view/Xds3zN
// --------------------------------------------------------

float softshadow( in vec3 ro, in vec3 rd, in float mint, in float tmax )
{
    float res = 1.0;
    float t = mint;
    for( int i=0; i<16; i++ )
    {
        float h = map( ro + rd*t ).dist;
        res = min( res, 8.0*h/t );
        t += clamp( h, 0.02, 0.10 );
        if( h<0.001 || t>tmax ) break;
    }
    return clamp( res, 0.0, 1.0 );
}

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

vec3 doLighting(Material material, vec3 pos, vec3 nor, vec3 ref, vec3 rd) {
    //return material.albedo;
    vec3 lightPos = vec3(0,0,-1);
    vec3 backLightPos = normalize(vec3(0,-.3,1));
    vec3 ambientPos = vec3(0,1,0);

    mat3 m = lightRotation();
    lightPos *= m;
    backLightPos *= m;
    
    // lighitng        
    float occ = calcAO( pos, nor );
    //occ = 1.;
    vec3  lig = lightPos;
    float amb = clamp((dot(nor, ambientPos) + 1.) / 2., 0., 1.);
    float dif = clamp( dot( nor, lig ), 0.0, 1.0 );
    float bac = pow(clamp(dot(nor, backLightPos), 0., 1.), 1.5);
    float fre = pow( clamp(1.0+dot(nor,rd),0.0,1.0), 2.0 );
    
    dif *= softshadow( pos, lig, 0.02, 2.5 );

    vec3 lin = vec3(0.0);
    lin += 1.20*dif*vec3(.95,0.80,0.60);
    lin += 0.80*amb*vec3(0.50,0.70,.80)*occ;
    lin += 0.30*bac*vec3(0.25,0.25,0.25)*occ;
    lin += 0.20*fre*vec3(1.00,1.00,1.00)*occ;
    vec3 col = material.albedo*lin;
    
    /*
    if (material.specular) {
    float specular = clamp(dot(ref, lig), 0., 1.);
    specular = pow((sin(specular * 20. - 3.) * .5 + .5) + .1, 32.) * specular;
    specular *= .1;
    specular += pow(clamp(dot(ref, lig), 0., 1.) + .3, 8.) * .1;
    
    col += specular;
    }
    */
    return col;
}   


// --------------------------------------------------------
// Ray Marching
// Adapted from: https://www.shadertoy.com/view/Xl2XWt
// --------------------------------------------------------

const float MAX_TRACE_DISTANCE = 30.; // max trace distance
const float INTERSECTION_PRECISION = .001; // precision of the intersection
const int NUM_OF_TRACE_STEPS = 200;
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
    vec3 color = vec3(0);
    
    if (ray.len > MAX_TRACE_DISTANCE) {
        isBackground = true;
    } else {
        pos = ray.origin + ray.direction * ray.len;
        normal = calcNormal(pos);
    }

    return Hit(ray, model, pos, isBackground, normal, color);
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
        //hit.color = vec3(sign(hit.ray.direction.x) * .5 + .5);
        //hit.color = vec3(dot(vec3(0,0,1), hit.ray.direction) * .5 + .5);
        //hit.color = vec3(sin(dot(vec3(0,1,0), hit.ray.direction) * 2.) * .5 + .5);
        
        vec3 d = hit.ray.direction;
        vec3 y = vec3(0,1,0);
        vec3 z = vec3(0,0,1);
        vec3 dd = normalize(cross(d, z));
        float angle = acos(dot(dd, y));
        angle *= sign(dot(cross(dd, y), z)) / PI;
        
        float lines = 10.;
        float thick = .1;
        float start = thick * .5;
        float end = 1. - start;
        float aa = .01;

        angle = mod(angle, 1. / 10.) * lines;
        angle = smoothstep(start, start - aa, angle) + smoothstep(end, end + aa, angle);

        hit.color = vec3(angle);
        
        return;
    }

    vec3 ref = reflect(hit.ray.direction, hit.normal);


    color = doLighting(
        hit.model.material,
        hit.pos,
        hit.normal,
        ref,
        hit.ray.direction
    );

    hit.color = color;
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
    return model.dist < 0. && model.material.transparency > 0.;
}

Hit renderTransparency(Hit hit, bool allowTransparency) {

    float refractiveIndex = hit.model.material.refractiveIndex;
    CastRay castRay;
    
    // First march ray through to the other side

    castRay = transparencyCastRay(hit, refractiveIndex);
    insideTransparency = true;
    enableTransparency = true;
    Hit hitInside = raymarch(castRay);
    float thickness = hitInside.ray.len;
    
    // Then march back out into the scene
    
    castRay = transparencyCastRay(hitInside, 1. / refractiveIndex);
    insideTransparency = false;
    enableTransparency = allowTransparency && ! isInsideTransparentModel(castRay.origin);
    Hit hitOutside = raymarch(castRay);
    enableTransparency = true;
    
    // Shade the final model
    
    shadeSurface(hitOutside);
    
    if (hitOutside.isBackground || hitOutside.model.material.transparency < 1.) {
        shadeSurface(hitOutside);
    }

    //float core = thickness*.2;
    //hitOutside.color = mix(hitOutside.color, vec3(1), core);
    
    return hitOutside;
}

Hit renderReflection(Hit hit) {

    float separation = 0.05;    
    vec3 rayDirection, rayOrigin;
    float startDistance;    
    rayDirection = reflect(hit.ray.direction, hit.normal);        
    startDistance = separation / abs(dot(rayDirection, hit.normal));
    rayOrigin = hit.pos + startDistance * rayDirection;
    CastRay castRay = CastRay(rayOrigin, rayDirection);

    hit = raymarch(castRay);
    
    shadeSurface(hit);        

    return hit;
}

void mixTransparency(inout Hit hit, Hit hit2) {
    hit.color = mix(hit.color, hit2.color, hit.model.material.transparency);
}

void mixReflection(inout Hit hit, Hit hit2) {
    hit.color = mix(hit.color, hit2.color, hit.model.material.reflection);
}

vec3 render(Hit hit){
    
    #ifdef DEBUG
        return vec3(length(hit.pos));
        return hit.normal * .5 + .5;
    #endif
    
    if (hit.isBackground || hit.model.material.transparency < 1.) {
        shadeSurface(hit);
        
        if (hit.isBackground) {
            return vec3(0.01);
            return hit.color;
            return spectrum(time * 2. + .25);
        }
    }
    
    if (hit.model.material.transparency > 0.) {
        Hit hit2 = renderTransparency(hit, true);
        if ( ! hit2.isBackground && hit2.model.material.transparency > 0.) {
            Hit hit3 = renderTransparency(hit2, false);
            if ( ! hit3.isBackground && hit3.model.material.transparency > 0.) {
                Hit hit4 = renderTransparency(hit2, false);
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

mat3 calcLookAtMatrix( in vec3 ro, in vec3 ta, in float roll )
{
    vec3 ww = normalize( ta - ro );
    vec3 uu = normalize( cross(ww,vec3(sin(roll),cos(roll),0.0) ) );
    vec3 vv = normalize( cross(uu,ww));
    return mat3( uu, vv, ww );
}

void doCamera(out vec3 camPos, out vec3 camTar, out float camRoll, in vec2 mouse) {
    float dist = 1.5;
    camRoll = 0.;
    camTar = vec3(0,0,0);
    camPos = vec3(0,0,-dist);
    camPos *= cameraRotation();
    //time = .99;
    camPos = vec3(
        sin(time * PI*2.) * 1.9,
        cos(time * PI*2.) * 1.9,
        0.
    );
    camRoll = floor(time + .5)*PI;
    camPos += camTar;
}


// --------------------------------------------------------
// Gamma
// https://www.shadertoy.com/view/Xds3zN
// --------------------------------------------------------

const float GAMMA = 2.2;

vec3 gamma(vec3 color, float g) {
    return pow(color, vec3(g));
}

vec3 linearToScreen(vec3 linearRGB) {
    return gamma(linearRGB, 1.0 / GAMMA);
}

void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
    time = iGlobalTime;
    time = mod(time, 3.);
    time /= 6.;
time += .01;
    //time = 0.35;
    
    vec2 p = (-iResolution.xy + 2.0*fragCoord.xy)/iResolution.y;
    p.y += .05;
    vec2 m = iMouse.xy / iResolution.xy;

    vec3 camPos = vec3( 0., 0., 2.);
    vec3 camTar = vec3( 0. , 0. , 0. );
    float camRoll = 0.;
    
    // camera movement
    doCamera(camPos, camTar, camRoll, m);
    
    // camera matrix
    mat3 camMat = calcLookAtMatrix( camPos, camTar, camRoll );  // 0.0 is the camera roll
    
    // create view ray
    vec3 rd = normalize( camMat * vec3(p.xy,2.0) ); // 2.0 is the lens length
    
    Hit hit = raymarch(CastRay(camPos, rd));

    vec3 color = render(hit);
    
    #ifndef DEBUG
       color = linearToScreen(color);
    #endif

    fragColor = vec4(color,1.0);
}