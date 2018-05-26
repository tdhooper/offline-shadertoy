precision highp float;

uniform vec2 iResolution;
uniform vec2 iOffset;
uniform float iTime;
uniform sampler2D iChannel0;

uniform mat4 cameraMatrix;
uniform vec3 cameraPosition;

uniform float guiZoom;
uniform float guiOffset;

uniform bool guiLevel1Enabled;
uniform bool guiLevel2Enabled;
uniform bool guiLevel3Enabled;

uniform float guiLevel1Lead;
uniform float guiLevel2Lead;
uniform float guiLevel3Lead;

uniform float guiLevel1Radius;
uniform float guiLevel2Radius;
uniform float guiLevel3Radius;

uniform float guiThickness;
uniform bool guiNormals;


void mainImage(out vec4 a, in vec2 b);

void main() {
    mainImage(gl_FragColor, gl_FragCoord.xy + iOffset.xy);
}

#ifdef GL_ES
precision mediump float;
#endif


/* SHADERTOY FROM HERE */

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

// Repeat in three dimensions
vec3 pMod3(inout vec3 p, vec3 size) {
    vec3 c = floor((p + size*0.5)/size);
    p = mod(p + size*0.5, size) - size*0.5;
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

float vmax(vec2 v) {
    return max(v.x, v.y);
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

vec2 closestPointOnLine(vec2 line, vec2 point){
    line = normalize(line);
    float d = dot(point, line);
    return line * d;
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

vec2 closestPointOnRepeatedLine(vec2 line, vec2 point){

    // Angle of the line
    float a = atan(line.x, line.y);

    // Rotate space so we can easily repeat along
    // one dimension
    pR(point, -a);

    // Repeat to create parallel lines at the corners
    // of the vec2(lead, radius) polar bounding area
    float repeatSize = sin(a) * line.y;
    float cell = pMod1(point.x, repeatSize);

    // Rotate space back to where it was
    pR(point, a);

    // Closest point on a line
    line = normalize(line);
    float d = dot(point, line);
    vec2 closest = line * d;

    // Part 2 of the repeat, move the line along it's
    // perpendicular by the repeat cell
    vec2 perpendicular = vec2(line.y, -line.x);
    closest += cell * repeatSize * perpendicular;

    return closest;
}

// Closest point on a helix
vec3 closestHelix(vec3 p, float lead, float radius) {

    p = cartToPolar(p);
    p.y *= radius;

    vec2 line = vec2(lead, radius * PI * 2.);
    vec2 closest = closestPointOnRepeatedLine(line, p.xy);

    closest.y /= radius;
    vec3 closestCart = polarToCart(vec3(closest, radius));

    return closestCart;
}

// Cartesian to helix coordinates
void pModHelix(inout vec3 p, float lead, float radius) {
    vec3 closest = closestHelix(p, lead, radius);
    float helixAngle = atan((2. * PI * radius) / lead);
    vec3 normal = normalize(closest - vec3(closest.x,0,0));
    vec3 tangent = vec3(1,0,0) * rotationMatrix(normal, helixAngle);
    float x = (closest.x / lead) * radius * PI * 2.;
    float y = dot(p - closest, cross(tangent, normal));
    float z = dot(p - closest, normal);
    p = vec3(x, y, z);
}

float pModHelixScale(inout vec3 p, float lead, float innerRatio) {
    float radius = mix(.25, .5, innerRatio);
    pModHelix(p, lead, radius);
    float scale = mix(.5, 0., innerRatio);
    p /= scale;
    return 1. / scale;
}


Model map(vec3 p) {
    float d;

    float s = dot(p,p);
    // s = .1;
    s *= guiZoom * 10.;
    p /= s;


    p.y += sin(2800./1000.);
    p.y += guiOffset;
    p.x += .5;
    p.x += iTime * .1;

    // d = length(p.yz) - .5;
    // pMod3(p, vec3(.5,.5,.5));
    // d = max(d, length(p) - .2);
    // d *= s;

    // return Model(d, vec2(0), 0);



    // pR(p.yz, guiRotate * PI * 2.);

    float lead1 = mix(.001, 8., pow(guiLevel1Lead, 2.));
    float radius1 = mix(0., .99, guiLevel1Radius);

    float lead2 = mix(.001, 8., pow(guiLevel2Lead, 2.));
    float radius2 = mix(0., .99, guiLevel2Radius);

    float lead3 = mix(.001, 8., pow(guiLevel3Lead, 2.));
    float radius3 = mix(0., .99, guiLevel3Radius);

    float scale = 1.;

    if (guiLevel1Enabled) {
        scale *= pModHelixScale(p, lead1, radius1);
        p.x += iTime * .2;
    }
    if (guiLevel2Enabled) {
        scale *= pModHelixScale(p, lead2, radius2);
        p.x += iTime * .4;
    }
    if (guiLevel3Enabled) {
        scale *= pModHelixScale(p, lead3, radius3);
    }


    d = length(p.yz) - .5;
    d = fBox2(p.yz, vec2(.5));
    d /= scale;

    d = abs(d) - .001;


    d *= s;


    vec2 uv = cartToPolar(p).xy;
    return Model(d, uv, 0);
}


// --------------------------------------------------------
// Rendering
// --------------------------------------------------------

vec3 camPos;

vec3 render(Hit hit){
    vec3 col;
    vec3 bg = vec3(.9,.3, .9) * .2;
    col = bg;
    if ( ! hit.isBackground) {
        vec2 uv = hit.model.uv;
        uv *= vec2(4., 8.);
        uv = cos(uv * PI * 2.);
        uv = smoothstep(.5, .55, uv);
        col = vec3(1.-uv.yx, 1.);
        vec3 light = normalize(vec3(.5,1,0));
        vec3 diffuse = vec3(dot(hit.normal, light) * .5 + .5);
        col *= diffuse;
        vec3 ref = reflect(hit.rayDirection, hit.normal);
        col = normalize(hit.normal) * .5 + .5;
        // col = vec3(col.b);
        col = mix(col, bg, clamp(0., 1., smoothstep(length(camPos * .25), length(camPos * 2.5), length(camPos - hit.pos))));
    }
    // if (hit.isBackground || hit.pos.z > 0.) {
    //     vec3 debugPlanePos = intersectPlane(
    //         hit.rayOrigin, hit.rayDirection, vec3(0,0,1), 0.
    //     );
    //     float dist = map(debugPlanePos).dist;
    //     vec3 meter = vec3(mod(dist, 1./4.));
    //     col = mix(col, meter, .5);
    // }
    return col;
}


// --------------------------------------------------------
// Ray Marching
// Adapted from: https://www.shadertoy.com/view/Xl2XWt
// --------------------------------------------------------

const float MAX_TRACE_DISTANCE = 10.;
const float INTERSECTION_PRECISION = .0001;
const int NUM_OF_TRACE_STEPS = 300;
const float FUDGE_FACTOR = .75;


vec3 calcNormal(vec3 pos){
    vec3 eps = vec3( 0.0001, 0.0, 0.0 );
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

void mainImage(out vec4 fragColor, in vec2 fragCoord) {

    vec2 p = (-iResolution.xy + 2.0*fragCoord.xy)/iResolution.y;

    // vec3 camPos = vec3(0,0,guiZoom);
    // pR(camPos.xz, iTime);
    // vec3 camTar = vec3(0);
    // vec3 camUp = vec3(0,1,0);
    // mat3 camMat = calcLookAtMatrix(camPos, camTar, camUp);

    camPos = cameraPosition;
    mat4 camMat = cameraMatrix;

    float focalLength = 2.;
    vec3 rayDirection = normalize(
        (vec4(p, -focalLength, 1) * camMat).xyz
    );

    Hit hit = raymarch(camPos, rayDirection);

    vec3 color = render(hit);
    // color = pow(color, vec3(1. / 2.2)); // Gamma
    fragColor = vec4(color,1);
}



