precision highp float;

uniform vec2 iResolution;
uniform float iTime;
uniform vec4 iMouse;

uniform float guiLead;
uniform float guiRadius;

void mainImage(out vec4 a, in vec2 b);

void main() {
    mainImage(gl_FragColor, gl_FragCoord.xy);
}

#ifdef GL_ES
precision mediump float;
#endif


/* SHADERTOY FROM HERE */

/*

Helix Distance
--------------

I've been wanting to create nice even helix shapes, most of the methods
I've seen usually twist some geometry around an axis, but this leads to
a squashed cross-section with large radiuses.

Instead, here I'm finding an approximate closest point on the helix and
using that to calculate the distance. To see what I mean by approximate,
enable VISUALISE_CLOSEST below and click around the canvas.

I'm also constructing 'helix coordinates' which you can see visualised
as the uv texture. These can also be used for twisting regular geometry
around the helix.

There's a seam that appears for small radiuses, I think I'd need a more
accurate closest point to fix that.

These Wikipedia pages were helpful when thinking about the geometry
of helices:

* https://en.wikipedia.org/wiki/Helix_angle
* https://en.wikipedia.org/wiki/Lead_(engineering)

*/

#define VISUALISE_CLOSEST

// --------------------------------------------------------
// Structs
// --------------------------------------------------------

struct Model {
    float dist;
    vec2 uv;
    int id;
};

struct Hit {
    Model model;
    vec3 pos;
    bool isBackground;
    vec3 normal;
    vec3 rayOrigin;
    vec3 rayDirection;
};


// --------------------------------------------------------
// Utilities
// --------------------------------------------------------

#define PI 3.14159265359

#define saturate(x) clamp(x, 0., 1.)

float vmax(vec2 v) {
    return max(v.x, v.y);
}

void pR(inout vec2 p, float a) {
    p = cos(a)*p + sin(a)*vec2(p.y, -p.x);
}

// Repeat space along one axis
float pMod1(inout float p, float size) {
    float halfsize = size*0.5;
    float c = floor((p + halfsize)/size);
    p = mod(p + halfsize, size) - halfsize;
    return c;
}

// Distance to line segment between <a> and <b>, used for fCapsule() version 2below
float fLineSegment(vec3 p, vec3 a, vec3 b) {
    vec3 ab = b - a;
    float t = saturate(dot(p - a, ab) / dot(ab, ab));
    return length((ab*t + a) - p);
}

// Capsule version 2: between two end points <a> and <b> with radius r 
float fCapsule(vec3 p, vec3 a, vec3 b, float r) {
    return fLineSegment(p, a, b) - r;
}

// A circular disc with no thickness (i.e. a cylinder with no height).
float fDisc(vec3 p, float r) {
 float l = length(p.xz) - r;
    return l < 0. ? abs(p.y) : length(vec2(p.y, l));
}

float fBox2(vec2 p, vec2 b) {
    vec2 d = abs(p) - b;
    return length(max(d, vec2(0))) + vmax(min(d, vec2(0)));
}

vec3 intersectPlane(vec3 rayOrigin, vec3 rayDirection, vec3 normal, float offset) {
    float dist = dot(normal, normal * offset - rayOrigin) / dot(normal, rayDirection);
    return rayOrigin + rayDirection * dist;
}

// Cartesian to polar coordinates
vec3 cartToPolar(vec3 p) {
    float x = p.x; // distance from the plane it lies on
    float a = atan(p.y, p.z); // angle around center
    float r = length(p.zy); // distance from center
    return vec3(x, a, r);
}

// Polar to cartesian coordinates
vec3 polarToCart(vec3 p) {
    return vec3(
        p.x,
        sin(p.y) * p.z,
        cos(p.y) * p.z
    );
}

// closest point on line/curve
// distance for point
// length of line/curve (for stacking)

vec2 closestPointOnLine(vec2 line, vec2 point){

    float a = atan(line.x, line.y);
    pR(point, -a);
    float rep = sin(a) * line.y;
    float c = pMod1(point.x, rep);
    pR(point, a);

    line = normalize(line);
    float d = dot(point, line);
    vec2 closest = line * d;

    line = vec2(line.y, -line.x);
    closest += c * rep * line;

    return closest;
}

// Closest of two points
vec3 closestPoint(vec3 pos, vec3 p1, vec3 p2) {
    if (length(pos - p1) < length(pos - p2)) {
        return p1;
    } else {
        return p2;
    }
}

// http://www.neilmendoza.com/glsl-rotation-about-an-arbitrary-axis/
mat3 rotationMatrix(vec3 axis, float angle)
{
    axis = normalize(axis);
    float s = sin(angle);
    float c = cos(angle);
    float oc = 1.0 - c;
    
    return mat3(
        oc * axis.x * axis.x + c,           oc * axis.x * axis.y - axis.z * s,  oc * axis.z * axis.x + axis.y * s,
        oc * axis.x * axis.y + axis.z * s,  oc * axis.y * axis.y + c,           oc * axis.y * axis.z - axis.x * s,
        oc * axis.z * axis.x - axis.y * s,  oc * axis.y * axis.z + axis.x * s,  oc * axis.z * axis.z + c
    );
}


// --------------------------------------------------------
// Helix
// --------------------------------------------------------

vec2 debugP;
vec2 debugClosest;
vec2 debugLine;
float debugLead;
float debugRadius;

// Closest point on a helix for one revolution
vec3 closestHelixSection(vec3 p, float lead, float radius) {

    p = cartToPolar(p);

    p.y *= radius;

    vec2 line = vec2(lead, radius * PI * 2.);
    // line = vec2(0, radius * PI * 2.);

    vec2 closest = closestPointOnLine(line, p.xy);

    debugLead = lead;
    debugRadius = radius;
    debugP = p.xy;
    debugLine = line;
    debugClosest = closest;

    closest.y /= radius;

    vec3 closestCart = polarToCart(vec3(closest, radius));
    //closestCart = vec3(closest, 0.);
    return closestCart;
}

// Closest point on a helix for infinite revolutions
vec3 closestHelix(vec3 p, float lead, float radius) {
    float c = pMod1(p.x, lead);
    vec3 offset = vec3(lead, 0, 0);
    vec3 A = closestHelixSection(p, lead, radius);
    vec3 B = closestHelixSection(p + offset, lead, radius) - offset;
    vec3 C = closestHelixSection(p - offset, lead, radius) + offset;
    vec3 closest = closestPoint(p, A, closestPoint(p, B, C));
    closest += offset * c;
    return closest;
}

// Cartesian to helix coordinates
vec3 helixCoordinates(vec3 p, vec3 closest, float lead, float radius) {
    float helixAngle = atan((2. * PI * radius) / lead);
    vec3 normal = normalize(closest - vec3(closest.x,0,0));
    vec3 tangent = vec3(1,0,0) * rotationMatrix(normal, helixAngle);
    float x = (closest.x / lead) * radius * PI * 2.;
    float y = dot(p - closest, cross(tangent, normal));
    float z = dot(p - closest, normal);
    return vec3(x,y,z);
}

vec3 mousePos;
bool mouseDown;

Model visualiseClosest(vec3 p) {
    float lead = guiLead;
    float radius = guiRadius;
    
    vec3 helix = closestHelixSection(p, lead, radius);
    float d = length(p - helix) - .1;
    
    vec3 testPoint = vec3(sin(iTime * .75) * 3., cos(iTime * .75) * .8, 0.);
    if (mouseDown) {
        testPoint = mousePos;
    }

    vec3 testHelix = closestHelixSection(testPoint, lead, radius);

    d = min(d, length(p - testHelix) - .2);
    d = min(d, length(p - testPoint) - .1);
    d = min(d, fCapsule(p, testPoint, testHelix, .05));

    return Model(d, vec2(0), 0);
}



Model map(vec3 p) {
    //pR(p.xz, iTime * .5);

    #ifdef VISUALISE_CLOSEST
        return visualiseClosest(p);
    #endif
    
    float phase = iTime * PI * 2. / 8. - .5;
    float lead = mix(1., 8., sin(phase) * .5 + .5);
    float radius = mix(0.001, 1.8, cos(phase) * .5 + .5);

    vec3 helix = closestHelix(p, lead, radius);
    float d = length(p - helix) - .5;
    
    vec3 hp = helixCoordinates(p, helix, lead, radius);
    vec2 uv = vec2(hp.x, atan(hp.y, hp.z) / PI / 2.);
    
    return Model(d, uv, 0);
}        


// --------------------------------------------------------
// Rendering
// --------------------------------------------------------

vec3 render(Hit hit){
    vec3 col;
    if (hit.isBackground) {
        col = vec3(.1);
    } else {
        vec2 uv = hit.model.uv;
        uv *= vec2(4., 8.);
        uv = cos(uv * PI * 2.);
        uv = smoothstep(.5, .55, uv);
        col = vec3(1.-uv.yx, 1.);
        #ifdef VISUALISE_CLOSEST
        col = vec3(1);
        #endif
        vec3 light = normalize(vec3(.5,1,0));
        vec3 diffuse = vec3(dot(hit.normal, light) * .5 + .5);
        col *= diffuse;
    }
    #ifndef VISUALISE_CLOSEST
    if (hit.isBackground || hit.pos.z > 0.) {
        vec3 debugPlanePos = intersectPlane(
            hit.rayOrigin, hit.rayDirection, vec3(0,0,1), 0.
        );
        float dist = map(debugPlanePos).dist;
        vec3 meter = vec3(mod(dist, 1./4.));
        col = mix(col, meter, .5);
    }
    #endif
    return col;
}


// --------------------------------------------------------
// Ray Marching
// Adapted from: https://www.shadertoy.com/view/Xl2XWt
// --------------------------------------------------------

const float MAX_TRACE_DISTANCE = 30.;
const float INTERSECTION_PRECISION = .001;
const int NUM_OF_TRACE_STEPS = 100;
const float FUDGE_FACTOR = .1;


vec3 calcNormal(vec3 pos){
    vec3 eps = vec3( 0.001, 0.0, 0.0 );
    vec3 nor = vec3(
        map(pos+eps.xyy).dist - map(pos-eps.xyy).dist,
        map(pos+eps.yxy).dist - map(pos-eps.yxy).dist,
        map(pos+eps.yyx).dist - map(pos-eps.yyx).dist );
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
        rayLength += currentDist * (1. - FUDGE_FACTOR);
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

    return Hit(model, pos, isBackground, normal, rayOrigin, rayDirection);
}


mat3 calcLookAtMatrix(vec3 ro, vec3 ta, vec3 up) {
    vec3 ww = normalize(ta - ro);
    vec3 uu = normalize(cross(ww,up));
    vec3 vv = normalize(cross(uu,ww));
    return mat3(uu, vv, ww);
}

float sc = PI * 2.;

float sGrid(float p, float spacing, float thick) {
    p = mod(p + spacing / 2., spacing) - spacing / 2.;
    float d = dot(p, 1.) / sc;
    d = smoothstep(thick * 1.25, thick, abs(d));
    return d;
}

vec3 plot(vec2 p) {
    p *= sc;

    vec3 col = vec3(.2);

    float box = fBox2(p, vec2(guiLead * .5, guiRadius * PI));
    col = mix(col, vec3(0), smoothstep(.001, .0, box / sc));

    col = mix(col, vec3(1,0,.5), sGrid(p.x, 10., .008) * .5);
    col = mix(col, vec3(.5,1,0), sGrid(p.y, 10., .008) * .5);
    col = mix(col, vec3(1,0,.5), sGrid(p.x, 1., .002) * .5);
    col = mix(col, vec3(.5,1,0), sGrid(p.y, 1., .002) * .5);

    col = mix(col, vec3(1), smoothstep(.025, .02, length(p - debugP) / sc));
    col = mix(col, vec3(0,.5,1), smoothstep(.025, .02, length(p - debugClosest) / sc));

    vec2 line;

    line = closestPointOnLine(debugLine, p);
    col = mix(col, vec3(0,.5,1), smoothstep(.006, .005, length(p - line) / sc));

    return col;
}

void mainImage(out vec4 fragColor, in vec2 fragCoord) {

    vec2 p = (-iResolution.xy + 2.0*fragCoord.xy)/iResolution.y;
    vec2 m = (-iResolution.xy + 2.0*iMouse.xy)/iResolution.y;

    vec3 camPos = vec3(0,0,-6);
    vec3 camTar = vec3(0);
    vec3 camUp = vec3(0,1,0);
    mat3 camMat = calcLookAtMatrix(camPos, camTar, camUp);

    float focalLength = 2.;
    vec3 rayDirection = normalize(camMat * vec3(p, focalLength));

    vec3 mouseRayDirection = normalize(camMat * vec3(m, focalLength));
    mousePos = intersectPlane(camPos, mouseRayDirection, vec3(0,0,1), 0.);
    mouseDown = iMouse.z > 0.;
    
    Hit hit = raymarch(camPos, rayDirection);

    vec3 color = render(hit);
    color = pow(color, vec3(1. / 2.2)); // Gamma

    float width = iResolution.x / iResolution.y;
    p += vec2(width * .5, .5);
    vec3 graph = plot(p);
    color = mix(color, graph, smoothstep(.001, .0, fBox2(p, vec2(width * .5, .5))));
    fragColor = vec4(color, 1);
}


