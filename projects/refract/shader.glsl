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


// #define DEBUG

float time;

#define PI 3.14159265359
#define HALF_PI 1.5707963267948966
#define TAU 6.28318530718
#define PHI 1.618033988749895


// --------------------------------------------------------
// HG_SDF
// https://www.shadertoy.com/view/Xs3GRB
// --------------------------------------------------------

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

mat3 mouseRotation(vec2 xy) {
    vec2 mouse = iMouse.xy / iResolution.xy;

    if (mouse.x != 0. && mouse.y != 0.) {
        xy = mouse;
    }

    float rx, ry;

    rx = (xy.y + .5) * PI;
    ry = (xy.x - .5) * 2. * PI;

    return sphericalMatrix(rx, ry);
}


mat3 modelRotation() {
    vec2 defaultRotation = vec2(sin(time * PI * 4.) * .1 + .5);
    pR(defaultRotation, sin(time * PI * 2.) * .1);
    mat3 m = mouseRotation(defaultRotation);
    return m;
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
    int id;
    bool transparent;
    float refractiveIndex;
    float dispersion;
};

Material transparentMaterial = Material(
    0,
    true,
    1. / 1.333,
    .15
);

Material backMaterial = Material(
    1,
    false,
    0.,
    0.
);  


// --------------------------------------------------------
// Modelling
// --------------------------------------------------------

bool insideTransparency = false;
bool enableTransparency = true;

struct Model {
    float dist;
    vec2 uv;
    Material material;
};

Model newModel() {
    return Model(
        1e12,
        vec2(0),
        backMaterial
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

Model backModel(vec3 p) {
    Model model = newModel();
    p.z += 2.;
    model.dist = dot(p, vec3(0,0,1)) - .7;
    model.uv = vec2(p.x, p.y);
    return model;
}

Model mainModel(vec3 p) {
    Model model = newModel();

    float part, d;
    float indentOffset = .1;
    float indentSize = .2;
    float indentSmooth = .2;

    // Sphere
    d = length(p) - .3;

    // Mirror
    p.z *= sign(p.z);

    // Indent
    p.z -= indentSize + indentOffset;
    part = length(p) - indentSize;
    d = smax(d, -part, indentSmooth);

    model.dist = d;
    return model;
}

Model transparentModel(vec3 p) {
    if ( ! enableTransparency) return newModel();

    Model model = mainModel(p);
    
    if (insideTransparency) model.dist *= -1.;

    model.material = transparentMaterial;
    return model;
}

Model map( vec3 p ){
    Model model = backModel(p);
    p *= modelRotation();
    model = opU(model, transparentModel(p));
    return model;
}


// --------------------------------------------------------
// Ray Marching
// Adapted from: https://www.shadertoy.com/view/Xl2XWt
// --------------------------------------------------------

const float MAX_TRACE_DISTANCE = 5.; // max trace distance
const float INTERSECTION_PRECISION = .001; // precision of the intersection
const int NUM_OF_TRACE_STEPS = 50;

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
        ray.len += currentDist;
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
// Shading
// --------------------------------------------------------

float makeDots(vec2 uv, float repeat, float size) {
    uv = mod(uv, 1. / repeat) * repeat;
    uv -= .5;
    return smoothstep(size, size * .8, length(uv));
}

float makeLine(float x, float thick) {
    float start = .5 - thick * .5;
    float end = .5 + thick * .5;
    float aa = .01;
    return smoothstep(start, start + aa, x) - smoothstep(end -aa, end, x);
}

float makeLines(float x, float repeat, float thick) {
    x = mod(x, 1. / repeat) * repeat;
    return makeLine(x, thick);
}

vec3 shadeSurface(Hit hit) {
    vec3 color = vec3(0);

    if (hit.model.material.id == 1) {
        vec2 uv = hit.model.uv;
        float repeat = 5.;
        color += makeDots(uv + .5 / repeat, repeat, .1);
        color += makeLines(uv.x, repeat, .025);
        color += makeLines(uv.y, repeat, .025);
    }

    return color;
}


// --------------------------------------------------------
// Refraction & Dispersion
// Some refraction logic from https://www.shadertoy.com/view/lsXGzH
// --------------------------------------------------------

const float REFRACTION_BOUNCES = 4.;
const float DISPERSION_SAMPLES = 20.;
const float WAVELENGTH_BLEND_MULTIPLIER = 5.;

Hit marchTransparent(Hit hit, float wavelength) {
    enableTransparency = true;
    insideTransparency = false;

    for (float i = 0.; i < REFRACTION_BOUNCES; i++) {
        if (hit.isBackground || ! hit.model.material.transparent) {
            return hit;
        } else {

            // Adjust refractive index for wavelength and dispersion amount
            float refractiveIndex = hit.model.material.refractiveIndex;
            float riMin = refractiveIndex;
            float riMax = refractiveIndex * (1. + hit.model.material.dispersion);
            refractiveIndex = mix(riMin, riMax, wavelength);

            // Invert when moving from the transparent object to air
            if (insideTransparency) {
                refractiveIndex = 1. / refractiveIndex;
            }

            vec3 rayDirection = refract(hit.ray.direction, hit.normal, refractiveIndex);
            if (rayDirection == vec3(0)) {
                // Total internal reflection
                rayDirection = reflect(hit.ray.direction, hit.normal);
            } else {
                insideTransparency = ! insideTransparency;
            }

            // Don't let the ray get trapped inside a transparent object
            if (i == REFRACTION_BOUNCES - 1.) {
                enableTransparency = false;
                insideTransparency = false;
            }

            // Move away from the surface before marching
            float separation = 0.01;
            float startDistance = separation / abs(dot(rayDirection, hit.normal));

            vec3 rayOrigin = hit.pos + startDistance * rayDirection;
            CastRay castRay = CastRay(rayOrigin, rayDirection);
            hit = raymarch(castRay);
        }
    }
    return hit;
}

vec3 shadeTransparentSurface(Hit hit) {
    float wavelength;
    vec3 sampleColor;
    vec3 color = vec3(0);

    // March for each wavelength and blend together
    for(float r = 0.; r < DISPERSION_SAMPLES; r++){
        wavelength = r / DISPERSION_SAMPLES;
        Hit hit2 = marchTransparent(hit, wavelength);
        sampleColor = shadeSurface(hit2) * spectrum(wavelength);
        // I don't have a model for correctly blending wavelengths together
        // so there's a fudge multiplier to stop the result going grey
        sampleColor /= DISPERSION_SAMPLES / WAVELENGTH_BLEND_MULTIPLIER;
        color += sampleColor;
    }

    return color;
}


// --------------------------------------------------------
// Main
// --------------------------------------------------------

// https://www.shadertoy.com/view/Xl2XWt
mat3 calcLookAtMatrix( in vec3 ro, in vec3 ta, in vec3 up )
{
    vec3 ww = normalize( ta - ro );
    vec3 uu = normalize( cross(ww,up));
    vec3 vv = normalize( cross(uu,ww));
    return mat3( uu, vv, ww );
}

vec3 getColor(vec2 p) {

    vec3 camUp = vec3(0,-1,0);
    vec3 camTar = vec3(0.);
    vec3 camPos = vec3(0,0,1.);

    mat3 camMat = calcLookAtMatrix(camPos, camTar, camUp);
    float focalLength = 2.;
    vec3 rayDirection = normalize(camMat * vec3(p, focalLength));

    CastRay castRay = CastRay(camPos, rayDirection);
    Hit hit = raymarch(castRay);

    #ifdef DEBUG
        return hit.normal * .5 + .5;
    #endif

    if ( ! hit.model.material.transparent) {
        return shadeSurface(hit);
    }

    return shadeTransparentSurface(hit);    
}

void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
    vec2 p = (-iResolution.xy + 2.0*fragCoord.xy)/iResolution.x;
    vec2 m = iMouse.xy / iResolution.xy;

    time = iGlobalTime;
    time /= 4.;
    time = mod(time, 1.);

    vec3 color = getColor(p);

    fragColor = vec4(color,1.0);
}


