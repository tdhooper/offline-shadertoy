precision highp float;

#extension GL_OES_standard_derivatives : enable

uniform vec2 iResolution;
uniform vec2 iOffset;
uniform float iTime;
uniform sampler2D iChannel0;

uniform mat4 cameraViewMatrix;
uniform vec3 cameraPosition;

uniform float guiFocalLength;
uniform float guiSmallRadius;
uniform float guiLargeRadius;
uniform float guiOffsetX;
uniform float guiOffsetY;
uniform float guiOffsetZ;
// uniform float guiRotateX;
// uniform float guiRotateY;
// uniform float guiRotateZ;
uniform bool guiAnimation2;
uniform bool guiTrefoil;

void mainImage(out vec4 a, in vec2 b);

void main() {
    mainImage(gl_FragColor, gl_FragCoord.xy + iOffset.xy);
}

#ifdef GL_ES
precision mediump float;
#endif


/* SHADERTOY FROM HERE */

#pragma glslify: distanceMeter = require(./distance-meter.glsl)
#pragma glslify: fNova = require(../nova-graff/nova.glsl)

// --------------------------------------------------------
// Structs
// --------------------------------------------------------

struct Model {
    float dist;
    vec3 material;
    vec2 uv;
    float underStep;
    int id;
};

struct Hit {
    Model model;
    vec3 pos;
    bool isBackground;
    vec3 normal;
    vec3 rayOrigin;
    float rayLength;
    vec3 rayDirection;
};

float time;

// --------------------------------------------------------
// Utilities
// --------------------------------------------------------

#define PI 3.14159265359
#define TAU 6.28318530718

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

float vmax(vec3 v) {
    return max(max(v.x, v.y), v.z);
}

float range(float vmin, float vmax, float value) {
  return (value - vmin) / (vmax - vmin);
}

float rangec(float a, float b, float t) {
    return clamp(range(a, b, t), 0., 1.);
}

float fBox(vec3 p, vec3 b) {
    vec3 d = abs(p) - b;
    return length(max(d, vec3(0))) + vmax(min(d, vec3(0)));
}


float fBox2(vec2 p, vec2 b) {
    vec2 d = abs(p) - b;
    return length(max(d, vec2(0))) + vmax(min(d, vec2(0)));
}

vec3 intersectPlane(vec3 rayOrigin, vec3 rayDirection, vec3 normal, float offset) {
    float dist = dot(normal, normal * offset - rayOrigin) / dot(normal, rayDirection);
    return rayOrigin + rayDirection * dist;
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

// The "Chamfer" flavour makes a 45-degree chamfered edge (the diagonal of a square of size <r>):
float cmin(float a, float b, float r) {
    return min(min(a, b), (a - r + b)*sqrt(0.5));
}

// Intersection has to deal with what is normally the inside of the resulting object
// when using union, which we normally don't care about too much. Thus, intersection
// implementations sometimes differ from union implementations.
float cmax(float a, float b, float r) {
    return max(max(a, b), (a + r + b)*sqrt(0.5));
}



// The "Round" variant uses a quarter-circle to join the two objects smoothly:
float smin(float a, float b, float r) {
    vec2 u = max(vec2(r - a,r - b), vec2(0));
    return max(r, min (a, b)) - length(u);
}

float smax(float a, float b, float r) {
    vec2 u = max(vec2(r + a,r + b), vec2(0));
    return min(-r, max (a, b)) + length(u);
}

// float smax(float a, float b, float r) {
//     float m = max(a, b);
//     if ((-a < r) && (-b < r)) {
//         return max(m, -(r - sqrt((r+a)*(r+a) + (r+b)*(r+b))));
//     } else {
//         return m;
//     }
// }

// float smin(float a, float b, float r) {
//     float m = min(a, b);
//     if ((a < r) && (b < r) ) {
//         return min(m, r - sqrt((r-a)*(r-a) + (r-b)*(r-b)));
//     } else {
//      return m;
//     }
// }


float smin(float a, float b) {
    return smin(a, b, .0);
}

float smax(float a, float b) {
    return smax(a, b, .0);
}



// --------------------------------------------------------
// Bezier
// IQ
// Modified to return closest point
// --------------------------------------------------------

vec4 sdBezier(vec3 A, vec3 B, vec3 C, vec3 pos)
{    
    vec3 a = B - A;
    vec3 b = A - 2.0*B + C;
    vec3 c = a * 2.0;
    vec3 d = A - pos;

    float kk = 1.0 / dot(b,b);
    float kx = kk * dot(a,b);
    float ky = kk * (2.0*dot(a,a)+dot(d,b)) / 3.0;
    float kz = kk * dot(d,a);      

    vec4 res;

    float p = ky - kx*kx;
    float p3 = p*p*p;
    float q = kx*(2.0*kx*kx - 3.0*ky) + kz;
    float h = q*q + 4.0*p3;

    if(h >= 0.0) 
    { 
        h = sqrt(h);
        vec2 x = (vec2(h, -h) - q) / 2.0;
        vec2 uv = sign(x)*pow(abs(x), vec2(1.0/3.0));
        float t = uv.x + uv.y - kx;
        t = clamp( t, 0.0, 1.0 );

        // 1 root
        vec3 qos = A + (c + b*t)*t;
        res = vec4(qos, t);
    }
    else
    {
        float z = sqrt(-p);
        float v = acos( q/(p*z*2.0) ) / 3.0;
        float m = cos(v);
        float n = sin(v)*1.732050808;
        vec3 t = vec3(m + m, -n - m, n - m) * z - kx;
        t = clamp( t, 0.0, 1.0 );

        // 3 roots
        vec3 qos = A + (c + b * t.x) * t.x;
        float dis = dot(qos - pos, qos);
        res = vec4(qos, t.x);

        qos = A + (c + b * t.y) * t.y;
        float dis2 = dot(qos - pos, qos);
        if( dis2 < dis ) {
            res = vec4(qos, t.y);
            dis = dis2;
        }

        qos = A + (c + b * t.z) * t.z;
        dis2 = dot(qos - pos,qos);
        if (dis2 < dis) {
            res = vec4(qos, t.z);
        }
    }
    
    return res;
}


Model opU(Model a, Model b) {
    if (a.dist < b.dist) {
        return a;
    }
    return b;
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
// Trefoil
// --------------------------------------------------------

vec4 unionBezier(vec3 p, vec4 a, vec4 b) {
    if (length(p - a.xyz) < length(p - b.xyz)) {
        return a;
    }
    return b;
}

float switchBezier(vec3 p, vec4 a, vec4 b) {
    return length(p - a.xyz) - length(p - b.xyz);
}

struct Curve {
    vec3 position;
    vec3 tangent;
    vec3 normal;
    vec3 binormal;
    float t;
};

struct CurvePart {
    vec4 bez;
    vec3 tangent;
    vec3 normal;
    float offset;
};


// 14
vec3 a0 = vec3(-0.5669088543444947, -0.23671635305770958, 0.3195550470325173);
vec3 b0 = vec3(-0.21879767215820162, -0.40033356387104035, 0.281874024845266);
vec3 c0 = vec3(0, -0.40033356387104035, 0);
vec3 aTan0 = vec3(0.9022064407419169, -0.38757768574492396, -0.1892275767334432);
vec3 cTan0 = vec3(0.613494495153403, 0.0002619034153996805, -0.7896989526541569);
vec3 aNor0 = vec3(-0.3077045475610546, -0.8858362163188649, 0.34729254133673104);
vec3 cNor0 = vec3(0.00016067630912024614, -0.9999999657032999, -0.0002068248599310844);
// 0
vec3 a1 = vec3(0, -0.40033356387104035, 0);
vec3 b1 = vec3(0.21879767215820162, -0.40033356387104035, -0.281874024845266);
vec3 c1 = vec3(0.5669088543444947, -0.23671635305770958, -0.3195550470325173);
vec3 aTan1 = vec3(0.613494495153403, 0.0002619034153996805, -0.7896989526541569);
vec3 cTan1 = vec3(0.902206440741908, 0.38757768574494417, -0.18922757673344437);
vec3 aNor1 = vec3(0.00016067630912024614, -0.9999999657032999, -0.0002068248599310844);
vec3 cNor1 = vec3(0.307704565929649, -0.8858362318233614, -0.3472924855147808);
// 1
vec3 a2 = vec3(0.5669088543444947, -0.23671635305770958, -0.3195550470325173);
vec3 b2 = vec3(0.9150200365307877, -0.0730991422443788, -0.3572360692197686);
vec3 c2 = vec3(0.9444049957885698, 0.24455937521789314, -0.1786180346098843);
vec3 aTan2 = vec3(0.902206440741908, 0.38757768574494417, -0.18922757673344437);
vec3 cTan2 = vec3(0.14311311393143017, 0.8728591195545045, 0.4665143020652047);
vec3 aNor2 = vec3(0.307704565929649, -0.8858362318233614, -0.3472924855147808);
vec3 cNor2 = vec3(0.9651563914319037, -0.018749617671540965, -0.2610011339349261);
// 5
vec3 a3 = vec3(-0.34669903629988125, 0.20016678193552043, 0);
vec3 b3 = vec3(-0.4560978723789819, 0.010682439557619071, -0.281874024845266);
vec3 c3 = vec3(-0.48845680241143064, -0.3725992929638091, -0.3195550470325173);
vec3 aTan3 = vec3(-0.3067472913765029, -0.531301816894778, -0.7896989797368913);
vec3 cTan3 = vec3(-0.11545113752095092, -0.9751225133799538, -0.1892276902165235);
vec3 aNor3 = vec3(-0.8660198797752173, 0.500009567669747, -0.000008534097960699594);
vec3 cNor3 = vec3(-0.9210193666153568, 0.17643446721607253, -0.3472667635948305);
// 6
vec3 a4 = vec3(-0.48845680241143064, -0.3725992929638091, -0.3195550470325173);
vec3 b4 = vec3(-0.5208157324438795, -0.7558810254852374, -0.3572360692197686);
vec3 c4 = vec3(-0.2604078662219397, -0.940158405422784, -0.1786180346098843);
vec3 aTan4 = vec3(-0.11545113752095092, -0.9751225133799538, -0.1892276902165235);
vec3 cTan4 = vec3(0.6843619980972525, -0.5603684330860392, 0.4665146029451097);
vec3 aNor4 = vec3(-0.9210193666153568, 0.17643446721607253, -0.3472667635948305);
vec3 cNor4 = vec3(-0.49883011384191744, -0.8264741169516885, -0.26097710921340667);
// 7
vec3 a5 = vec3(-0.2604078662219397, -0.940158405422784, -0.1786180346098843);
vec3 b5 = vec3(1.377036685388269e-16, -1.1244357853603304, 0);
vec3 c5 = vec3(0.26040786622193884, -0.9401584054227843, 0.1786180346098843);
vec3 aTan5 = vec3(0.6843619980972525, -0.5603684330860392, 0.4665146029451097);
vec3 cTan5 = vec3(0.684361998097279, 0.5603684330859919, 0.4665146029451278);
vec3 aNor5 = vec3(-0.49883011384191744, -0.8264741169516885, -0.26097710921340667);
vec3 cNor5 = vec3(0.4988300815201652, -0.8264741202403052, 0.2609771605784677);
// 8
vec3 a6 = vec3(0.26040786622193884, -0.9401584054227843, 0.1786180346098843);
vec3 b6 = vec3(0.5208157324438776, -0.7558810254852383, 0.3572360692197686);
vec3 c6 = vec3(0.4884568024114297, -0.37259929296381017, 0.3195550470325173);
vec3 aTan6 = vec3(0.684361998097279, 0.5603684330859919, 0.4665146029451278);
vec3 cTan6 = vec3(-0.1154511375209849, 0.9751225133799435, -0.18922769021655528);
vec3 aNor6 = vec3(0.4988300815201652, -0.8264741202403052, 0.2609771605784677);
vec3 cNor6 = vec3(0.9210193440053495, 0.17643447537181506, 0.34726681941733095);
// 9
vec3 a7 = vec3(0.4884568024114297, -0.37259929296381006, 0.3195550470325173);
vec3 b7 = vec3(0.4560978723789819, 0.010682439557618061, 0.281874024845266);
vec3 c7 = vec3(0.34669903629988125, 0.2001667819355199, 0);
vec3 aTan7 = vec3(-0.1154511375209849, 0.9751225133799435, -0.18922769021655528);
vec3 cTan7 = vec3(-0.30674729137653417, 0.5313018168947605, -0.7896989797368906);
vec3 aNor7 = vec3(0.9210193440053495, 0.17643447537181506, 0.34726681941733095);
vec3 cNor7 = vec3(0.8660198557765797, 0.5000096092349068, 0.000008571384490341227);
// 10
vec3 a8 = vec3(0.34669903629988125, 0.2001667819355199, 0);
vec3 b8 = vec3(0.23730020022078058, 0.38965112431342175, -0.281874024845266);
vec3 c8 = vec3(-0.07845205193306419, 0.6093156460215194, -0.3195550470325173);
vec3 aTan8 = vec3(-0.30674729137653417, 0.5313018168947605, -0.7896989797368906);
vec3 cTan8 = vec3(-0.7867551988977671, 0.5875449230132569, -0.1892279589507877);
vec3 aNor8 = vec3(0.8660198557765797, 0.5000096092349068, 0.000008571384490341227);
vec3 cNor8 = vec3(0.6133085148535106, 0.709419711040007, -0.34724103904362313);

float sepa;
float sepR = 0.465;

CurvePart bezierInner(vec3 p) {
    vec4 bez, bezPart;
    vec3 a, b, c,
        aTan, cTan, aNor, cNor,
        tan, nor;
    float offset;

    a = a0;
    b = b0;
    c = c0;
    aTan = aTan0;
    cTan = cTan0;
    aNor = aNor0;
    cNor = cNor0;
    bezPart = sdBezier(a, b, c, p);
    bez = bezPart;
    tan = mix(aTan, cTan, bez.w);
    nor = mix(aNor, cNor, bez.w);
    offset = 0.;

    a = a1;
    b = b1;
    c = c1;
    aTan = aTan1;
    cTan = cTan1;
    aNor = aNor1;
    cNor = cNor1;
    bezPart = sdBezier(a, b, c, p);
    if (switchBezier(p, bez, bezPart) > 0.) {
        bez = bezPart;
        tan = mix(aTan, cTan, bez.w);
        nor = mix(aNor, cNor, bez.w);
        offset = 1.;
    }

    a = a2;
    b = b2;
    c = c2;
    aTan = aTan2;
    cTan = cTan2;
    aNor = aNor2;
    cNor = cNor2;
    bezPart = sdBezier(a, b, c, p);
    if (switchBezier(p, bez, bezPart) > 0.) {
        bez = bezPart;
        tan = mix(aTan, cTan, bez.w);
        nor = mix(aNor, cNor, bez.w);
        offset = 2.;
    }

    sepa = length(p - bez.xyz) - sepR;

    return CurvePart(
        bez,
        tan,
        nor,
        offset
    );
}

CurvePart bezierOuter(vec3 p) {
    vec4 bez, bezPart;
    vec3 a, b, c,
        aTan, cTan, aNor, cNor,
        tan, nor;
    float offset;

    a = a3;
    b = b3;
    c = c3;
    aTan = aTan3;
    cTan = cTan3;
    aNor = aNor3;
    cNor = cNor3;
    bezPart = sdBezier(a, b, c, p);
    bez = bezPart;
    tan = mix(aTan, cTan, bez.w);
    nor = mix(aNor, cNor, bez.w);
    offset = 0.;

    a = a4;
    b = b4;
    c = c4;
    aTan = aTan4;
    cTan = cTan4;
    aNor = aNor4;
    cNor = cNor4;
    bezPart = sdBezier(a, b, c, p);
    if (switchBezier(p, bez, bezPart) > 0.) {
        bez = bezPart;
        tan = mix(aTan, cTan, bez.w);
        nor = mix(aNor, cNor, bez.w);
        offset = 1.;
    }

    float sepd = length(p - bez.xyz) - sepR;


    a = a5;
    b = b5;
    c = c5;
    aTan = aTan5;
    cTan = cTan5;
    aNor = aNor5;
    cNor = cNor5;
    bezPart = sdBezier(a, b, c, p);
    if (switchBezier(p, bez, bezPart) > 0.) {
        bez = bezPart;
        tan = mix(aTan, cTan, bez.w);
        nor = mix(aNor, cNor, bez.w);
        offset = 2.;
    }

    a = a6;
    b = b6;
    c = c6;
    aTan = aTan6;
    cTan = cTan6;
    aNor = aNor6;
    cNor = cNor6;
    bezPart = sdBezier(a, b, c, p);
    if (switchBezier(p, bez, bezPart) > 0.) {
        bez = bezPart;
        tan = mix(aTan, cTan, bez.w);
        nor = mix(aNor, cNor, bez.w);
        offset = 3.;
    }

    float sepc = 1e12;

    a = a7;
    b = b7;
    c = c7;
    aTan = aTan7;
    cTan = cTan7;
    aNor = aNor7;
    cNor = cNor7;
    bezPart = sdBezier(a, b, c, p);

    sepc = min(sepc, length(p - bezPart.xyz) - sepR);

    if (switchBezier(p, bez, bezPart) > 0.) {
        bez = bezPart;
        tan = mix(aTan, cTan, bez.w);
        nor = mix(aNor, cNor, bez.w);
        offset = 4.;
    }

    a = a8;
    b = b8;
    c = c8;
    aTan = aTan8;
    cTan = cTan8;
    aNor = aNor8;
    cNor = cNor8;
    bezPart = sdBezier(a, b, c, p);

    sepc = min(sepc, length(p - bezPart.xyz) - sepR);

    if (switchBezier(p, bez, bezPart) > 0.) {
        bez = bezPart;
        tan = mix(aTan, cTan, bez.w);
        nor = mix(aNor, cNor, bez.w);
        offset = 5.;
    }

    float sepb = length(p - bez.xyz) - sepR;
    sepa = max(sepa, sepb);
    sepc = max(sepc, sepd);
    sepa = min(sepa, sepc);

    return CurvePart(
        bez,
        tan,
        nor,
        offset + 6.
    );
}

Curve TrefoilCurve(vec3 p) {

    vec4 bez, bezPart;
    
    float side = 0.;
    float tFlip = 1.;

    float cell = 0.;

    float repetitions = 3.;
    float angle = TAU / repetitions;


    if (p.z > 0.) {
        p.z *= -1.;
        p.y *= -1.;
        pR(p.xy, angle / -2.);
        side = 1.;
        tFlip = -1.;
    }
    pR(p.xy, angle / -2.);
    cell = pModPolar(p.xy, repetitions);
    pR(p.xy, angle / 2.);


    float outer = 0.;

    CurvePart curve = bezierInner(p);
    CurvePart outerCurve = bezierOuter(p);

    if (switchBezier(p, curve.bez, outerCurve.bez) > 0.) {
        outer = 1.;
        curve = outerCurve;
    }

    // curve = cornerCurve;

    // curve = cornerCurve;

    // float flip = sign(p.z);
    // side = flip * .5 + .5;

    // float rot = side * TAU / -6.;
    // rot = 0.;

    // vec3 pp = p;
    // cell = floor((atan(p.y, p.x) + angle / 2. - rot) / angle);

    float an = cell * angle;

    mat3 m = mat3(
        cos(an), -sin(an), 0,
        sin(an), cos(an), 0,
        0, 0, 1
    );

    mat3 m2 = mat3(
        cos(angle / -2.), -sin(angle / -2.), 0,
        sin(angle / -2.), cos(angle / -2.), 0,
        0,0,-1
    );
    mat3 m3 = mat3(1,0,0,0,-1,0,0,0,1);

    if (side > 0.) {
        m = m * m2 * m3;
    }

    // m = mat3(1,0,0,0,1,0,0,0,1);


    float parts = 24.;

    float sectionCurveOffset;

    vec3 tangent = curve.tangent;
    vec3 normal = curve.normal;
    vec3 binormal = cross(tangent, normal);

    normal *= -1.;

    if (side > 0.) {
        binormal *= -1.;
    }

    tangent = normalize(tangent);
    binormal = normalize(binormal);
    normal = normalize(normal);

    float flip = -1. + side * 2.;
    cell = mod(cell * flip, 3.);
    float offset = cell * 5. + side * 7.;
    float t = (offset - (curve.bez.w + curve.offset) * flip);
    t = mod(t / 15., 1.);

    vec3 position = curve.bez.xyz;

    return Curve(
        position * m,
        curve.tangent * m,
        normal * m,
        binormal * m,
        t
    );
}

Curve pModTrefoil(inout vec3 p, float len) {
    Curve curve = TrefoilCurve(p);
    float x = dot(p - curve.position, curve.normal);
    float y = dot(p - curve.position, curve.binormal);
    float z = curve.t;
    p = vec3(x, y, z);
    p.z -= 0.0666;
    p.z *= len;
    return curve;
}


// --------------------------------------------------------
// Materials
// --------------------------------------------------------

vec3 DEFAULT_MAT = vec3(1.9);
vec3 DISTANCE_METER_MAT = vec3(123.);

vec3 TRAIN_MAT = vec3(.9,.5,.5);
vec3 CHANNEL_MAT = vec3(.15);
vec3 SLEEPER_MAT = vec3(.2);
vec3 RAIL_MAT = vec3(.25);
vec3 PLATFORM_MAT = vec3(.5);
vec3 MIND_THE_GAP_MAT = vec3(1.,1.,0.);

vec3 TRAIN_RED = vec3(1,.025,.125);
vec3 TRAIN_GREY = vec3(.4);
vec3 TRAIN_ROOF = vec3(.6);
vec3 TRAIN_WINDOW = vec3(.5,.9,1.);
vec3 TRAIN_WINDOW_FRAME = vec3(.1);
vec3 TRAIN_WHITE = vec3(1);
vec3 TRAIN_BLUE = vec3(0,0,.7);
vec3 TRAIN_UNDERCARRIDGE = vec3(.1);

vec3 STAIR_BASE_MAT = vec3(.1);
vec3 STEP_MAT = vec3(.3);
vec3 STEP_TOP_MAT = vec3(.5);
vec3 STEP_STRIPE_MAT = vec3(.3);
vec3 STEP_YELLOW_MAT = MIND_THE_GAP_MAT;
vec3 HANDRAIL_MAT = vec3(.7);


// --------------------------------------------------------
// Model
// --------------------------------------------------------

Model mTrain(vec3 p, float width, float height, float index) {
    p.z *= -1.;

    float d = 1e12;
    float len = 1.2;

    vec3 pp = p;

    p /= height * 4.;
    p.y *= sign(p.x);
    p.y -= mix(.75, .25, sign(p.x) * .5 + .5);
    p.z += (index - 1.) * len * 4.;
    vec2 uv = p.zy;
    // uv.xy = vec2(index / 3.);

    p = pp;
    p.x = abs(p.x);
    pp = p;

    float isFront = max(0., sign(p.z));
    float frontWidth = .12;
    float backWidth = .08;
    float innerLength = (len * 2.) - backWidth * 2.;
    float frontDoorOffset = frontWidth - backWidth;

    d = p.x - width;

    // Slanted side
    p.xy -= vec2(width, height * .45);
    // d = smax(d, dot(p.xy, normalize(vec2(1.,-.175))), width * .01);
    d = max(d, dot(p.xy, normalize(vec2(1.,-.175))));
    p = pp;

    // Round top
    float topRadius = width * 1.1;
    p.y += height - topRadius;
    float top = length(p.xy) - topRadius;
    top = min(top, -p.y);
    // d = smax(d, top, width * .05);
    d = smax(d, top, .005);
    p = pp;

    // Blue
    float blueStripe = step(0., p.y - height + .03);
    vec3 color = mix(TRAIN_WHITE, TRAIN_BLUE, blueStripe);

    float form = d;
    float thinOffset = .025;
    float thin = form + thinOffset;
    d = thin;

    // Carridge
    float carridgeCrop = max(-p.z - innerLength / 2., p.z - innerLength / 2. + frontDoorOffset);
    d = smax(d, carridgeCrop, .01);

    float roofPane = p.y + height * .8;

    // pMod1(p.z, len / 2.);
    // p.y -= height;
    // thin = max(-thin, fBox2(p.yz, vec2(thinTop,.2)));
    // d = max(d, -thin);
    // p = pp;

    // Side doors

    float sideDoorWidth = .17 / 2.;
    float spacing = innerLength / 3.;
    p.z -= spacing / 2.;
    float cell = pMod1(p.z, spacing);
    float isFrontDoor = max(0., cell);
    p.z += frontDoorOffset * isFrontDoor;
    p.z = abs(p.z);
    p.z -= sideDoorWidth - .001;
    float sideDoorMask = fBox2(p.yz, vec2(1., sideDoorWidth));
    float sideDoors = smax(form, sideDoorMask, thinOffset * 1.5);
    sideDoors = smax(sideDoors, -roofPane, .01);
    sideDoors = max(sideDoors, carridgeCrop);
    float sideDoorEdge = sideDoorMask + .01;
    vec3 sideDoorColor = mix(TRAIN_RED, TRAIN_WHITE, isFrontDoor);
    sideDoorColor = mix(sideDoorColor, TRAIN_BLUE, blueStripe * isFrontDoor);
    float windowFrameOffset = .007;
    p.y += .028;
    p.z += .025;
    float sideDoorWindow = fBox2(p.yz, vec2(height * .42, sideDoorWidth * .3)) - .005;
    sideDoorColor = mix(sideDoorColor, TRAIN_WINDOW, 1. - step(0., sideDoorWindow));
    p = pp;
    color = mix(color, sideDoorColor, step(0., d - sideDoors));
    d = min(d, sideDoors);
    p = pp;


    // Side windows

    p.y = abs(p.y);
    float sideWindow = p.y - .04;
    sideWindow = smax(sideWindow, -sideDoorMask + windowFrameOffset * 2., .01);
    // color = mix(color, TRAIN_WINDOW_FRAME, 1. - step(0., sideWindow - windowFrameOffset));
    pMod1(p.z, spacing);
    p.z = abs(p.z);
    sideWindow = smax(sideWindow, -(p.z - windowFrameOffset * 2.), .01);
    color = mix(color, TRAIN_WINDOW, 1. - step(0., sideWindow));
    p = pp;

    // Roof
    float roof = smax(form, roofPane, .01);
    roof = smax(roof, carridgeCrop, .01);
    color = mix(color, TRAIN_ROOF, step(0., d - roof));
    d = min(d, roof);



    // Front

    p.z = abs(p.z);
    p.z -= len;
    float frontRadius = width * 3.;
    float endcap = length(p.xz + vec2(0,frontRadius)) - frontRadius;
    endcap *= .5;
    p = pp;

    p.z -= len;
    float front = smax(form, endcap, .03);
    front = smax(front, -(p.z + frontWidth), .01);
    color = mix(color, TRAIN_RED, step(0., d - front));
    d = min(d, front);
    p = pp;
    
    // Back

    p.z += len;
    float back = smax(form, endcap, .03);
    back = smax(back, (p.z - backWidth), .01);
    vec3 backColor = mix(TRAIN_WHITE, TRAIN_BLUE, blueStripe);
    color = mix(color, backColor, step(0., d - back));
    d = min(d, back);
    p = pp;



    // Front grey
    p.z = abs(p.z);
    p.z -= len - .05;
    float grey = form + .01;
    grey = max(grey, -p.z);
    p.y -= height * .48;
    p.x -= width * .3;
    if (isFront > 0.) {
        grey = smax(grey, dot(p.xy, normalize(vec2(.3,1))), .02);
        grey = max(grey, p.y);
    }
    color = mix(color, TRAIN_GREY, step(0., d - grey));
    p = pp;


    // Front door
    vec2 doorWH = vec2(width * .22, height * 1.7);
    vec2 doorXY = vec2(0, height);

    // - Inset
    p.z = abs(p.z);
    p.xy -= doorXY;
    p.z -= len;
    d = smax(d, -fBox(p, vec3(doorWH, .01)), .01);

    // - Template
    float door = fBox2(p.xy, doorWH);
    p = pp;

    // Window
    float windowBottom = mix(.02, -.01, isFront);
    float windowOffset = .0125;
    float window = max(door + windowOffset, p.y - windowBottom);
    p = pp;

    float window2Offset = .02;
    float window2 = max(-door + window2Offset, p.y - .02);
    p.xy -= vec2(doorWH.x + window2Offset, windowBottom + .01);
    window2 = max(window2, dot(p.xy, normalize(vec2(-.7,1))));

    p = pp;
    window2 = max(window2, -(p.y + doorWH.y - doorXY.y - windowOffset + .005));
    window2 = smax(window2, grey + .02, .01);

    p = pp;


    window = min(window, window2);
    // color = mix(color, TRAIN_WINDOW_FRAME, 1.-step(0., window - windowFrameOffset));
    color = mix(color, TRAIN_WINDOW, 1.-step(0., window));

    // Undercarridge

    float baseHeight = .025;
    p.y -= height;
    d = smax(d, p.y, 0.);

    // p.x = abs(p.x);
    // p.x -= width;
    p.y -= baseHeight / 2.;
    float undercarridge = fBox2(p.xy, vec2(width - baseHeight, baseHeight / 2.));
    p.z = abs(p.z);
    undercarridge = max(undercarridge, endcap);
    color = mix(color, TRAIN_UNDERCARRIDGE, step(0., d - undercarridge));
    d = min(d, undercarridge);
    p = pp;

    Model train = Model(d, color, uv, .2, 1);
    return train;
}

bool pastThreshold = false;
float lastSide = 1.;

Model mTrainSide(vec3 p, float curveLen, float radius) {
    
    vec3 pp = p;
    float d = 1e12;
    vec3 color = CHANNEL_MAT;

    float trainSize = .175;

    // Channel

    p.x = abs(p.x);
    d = -p.y - .001;

    float threshold = fBox2(p.xy + vec2(0,.003), vec2(radius, .002));
    float side = sign(threshold);
    if (side != lastSide) {
        pastThreshold = true;
    }
    lastSide = side;

    float gap = .01;
    float channelWidth = trainSize + gap;
    float channelDepth = .09;

    if (pastThreshold) {
        float cut = fBox2(p.xy, vec2(channelWidth, channelDepth));
        d = max(d, -cut);
    }

    // Platform

    p.x = abs(p.x);
    p.x -= radius;
    float platform = fBox2(p.xy, vec2(radius - channelWidth,.005)) - .005;
    color = mix(color, PLATFORM_MAT, step(0., d - platform));
    d = min(d, platform);
    p = pp;

    p.x = abs(p.x);
    p.x -= mix(radius, channelWidth, .5);
    pMod1(p.z, curveLen * 2./3. / 10.);
    float mindTheGap = fBox2(p.xz, vec2(0., .125)) - .015;
    color = mix(color, MIND_THE_GAP_MAT, 1. - step(0., mindTheGap));
    p = pp;


    // Sleepers

    p.x = abs(p.x);
    float sleeperSize = curveLen / 80.;
    float sleeperHeight = .02;
    pMod1(p.z, sleeperSize);
    p.y -= channelDepth;
    float sleepers = fBox(p, vec3(channelWidth * .66, sleeperHeight, sleeperSize / 4.));
    color = mix(color, SLEEPER_MAT, step(0., d - sleepers));
    d = min(d, sleepers);
    p = pp;


    // Rails
    p.x = abs(p.x);
    float railHeight = .01;
    p.x = abs(p.x);
    p.x -= channelWidth * .4;
    p.y -= channelDepth - sleeperHeight - railHeight;
    float rail = fBox2(p.xy, vec2(railHeight));
    color = mix(color, RAIL_MAT, step(0., d - rail));
    d = min(d, rail);
    p = pp;


    Model track = Model(d, color, vec2(0), 0., 2);

    // p.x -= radius;
    // Model platform = Model(
    //     fBox2(p.xy, vec2(.075)),
    //     color,
    //     0.
    // );
    // p = pp;

    p.z += curveLen * guiOffsetX;
    if (guiAnimation2) {
        p.z += time * (curveLen * 5. / 6.);
    } else {
        p.z += time * (curveLen * 5. / 6.);
    }
    float c = floor(p.z / curveLen * 2. + .5);
    c = mod(c, 2.);
    if (time > .85) {
        c += 1.;
    }
    pMod1(p.z, curveLen / 2.);
    float trainHeight = trainSize * .8;
    p.y += trainHeight;
    Model train = mTrain(p, trainSize, trainHeight, c);
    p = pp;

    Model model = opU(track, train);
    // model = track;

    if ( ! pastThreshold) {
        model.dist = max(model.dist, p.y);
    }

    // model.uv = vec2(c);

    return model;
}

float fStep(vec3 p, float stairSize) {
    pR(p.yz, (30./180.) * PI);
    float d = length(p.yz) - stairSize * .9;
    d = smax(d, p.y, .001);
    return d;
}

Model mStairSide(vec3 p, float curveLen, float radius) {
    vec3 pp = p;
    float d = p.y;

    float stairSize = curveLen / 60.;
    if (guiAnimation2) {
        p.z += time * stairSize * 2. * -15. * .5;
    } else {
        p.z += time * stairSize * 2. * -3.;
    }

    pMod1(p.z, stairSize);

    float stairRadius = stairSize * .9;
    float handrailWidth = .1;
    float stairWidth = radius - handrailWidth;

    vec3 color = STAIR_BASE_MAT;

    // Step
    p.z -= stairSize / 2.;
    pp = p;
    pR(p.yz, (30./180.) * PI);
    float steps = length(p.yz) - stairRadius;
    steps = smax(steps, p.y, .02);
    vec3 stepColor = STEP_TOP_MAT;

    float warning = -fBox2(p.xz, vec2(stairWidth - .01, stairRadius - .02));
    warning = -(abs(p.z) - stairRadius + .025);
    stepColor = mix(stepColor, STEP_YELLOW_MAT, 1.-step(0., warning));

    p.y += .01;
    vec3 edgeColor = mix(STAIR_BASE_MAT, STEP_MAT, smoothstep(stairRadius * -.6, 0., p.y));
    stepColor = mix(stepColor, edgeColor, 1.-step(0., p.y));
    p = pp;

    // Dummy
    p.z -= stairSize;
    steps = min(steps, length(p.yz) - stairRadius);

    // Limit width
    steps = max(steps, p.x - stairWidth);

    color = mix(color, stepColor, step(0., d - steps));
    d = min(d, steps);
    p = pp;

    p.x -= radius;
    float handrailHeight = .14;
    float handrail = fBox2(p.xy, vec2(handrailWidth, handrailHeight));
    vec3 handrailColor = mix(STAIR_BASE_MAT, HANDRAIL_MAT, smoothstep(0., handrailHeight * 2., p.y));
    // handrailColor = STEP_MAT;
    handrailColor = mix(handrailColor, HANDRAIL_MAT, step(0., p.x + handrailWidth * .75));
    color = mix(color, handrailColor, step(0., d - handrail));
    d = min(d, handrail);
    p = pp;

    d = max(d, -p.y);
    return Model(d, color, vec2(0), .2, 3);
}

bool AO_PASS = false;

Model fShape2(vec3 p) {
    // float s = 1.;

    // if (length(p) > 3.) {
    //     s = dot(p,p);
    //     s /= 5.;
    //     p /= s;
    //     // p.xz *= -1.;
    //     // pR(p.xz, PI);
    // }

    // bool guiTrefoil = false;

    if (guiTrefoil) {
        if (guiAnimation2) {
            pR(p.xy, time * -PI * 2. * 1./3.);
        } else {
            pR(p.xy, time * PI * 2. * 2./3.);
        }
    } else {
        // float side = sign(p.x);
        // p.x = abs(p.x);
        // p.x -= .25;
        // p.y *= side;
    }

    // p /= s;
    Curve curve;
    float curveLen = 14.;

    if (guiTrefoil) {
        curve = pModTrefoil(p, curveLen);
        // pR(p.xy, 2.5 + p.z / curveLen * PI * 2.);
        pR(p.xy, -time * PI * 2.);
    } else {
        p = p.xzy * vec3(1,-1,1);
    }


    float radius = .28;
    float outer = length(p.xy) - radius;
    float eps = .02;
    float d = -outer + eps * 2.;
    if ( ! AO_PASS) {
        if (outer > eps) {
            return Model(outer, vec3(.2), vec2(0), 0., 0);
        }
    } else {
        d = 1e12;
    }


    p.y -= .05;

    Model model = mTrainSide(p, curveLen, radius);

    p.x = abs(p.x);

    if ( ! pastThreshold) {
        Model stair = mStairSide(p, curveLen, radius);
        model = opU(model, stair);
    }

    model.dist = max(model.dist, outer);

    model.dist = min(model.dist, d);

    // model.dist = outer + .1;

    // model.dist *= s;
    

    // model.dist -= .03;

    return model;
}

float focalLength;

Model map(vec3 p) {

    pR(p.xy, guiOffsetY * PI * 2.);

    float d;
    float s = focalLength;
    p *= s;

    Model model = fShape2(p);

    // bool guiTrefoil = false;

    if ( ! guiTrefoil) {
        // model.dist = max(model.dist, fBox2(p.zy, vec2(2.)));
    }

    model.dist /= s;

    return model;
}


Model mapDebug(vec3 p) {

    vec3 n = normalize(vec3(1,0,0));
    float d = abs(dot(p, n) + .43) - .001;
    Model model = map(p);

    return model;

    if (model.dist < d) {
        return model;
    }

    return Model(d, DISTANCE_METER_MAT, vec2(0), 0., 99);
}


// --------------------------------------------------------
// Rendering
// --------------------------------------------------------

vec3 camPos;

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

vec3 drawNova(vec3 col, vec2 uv) {
    uv *= -1.;
    float s = .3;
    uv /= s;
    uv.y -= .5;
    uv.x /= 1.4;
    uv.x += 1.2;

    vec2 uvw = uv;
    pR(uvw, PI * .25);
    uv += sin((uvw + .25 + time) * 5.) * .1;
    // uv.y += cos((uv.x + time) * 2.) * .1;

    float d = fNova(uv, .133);

    uv -= vec2(.05,-.1);
    float d2 = fNova(uv, .133);
    // d2 = max(d2, d);

    d *= s;
    d2 *= s;

    col = mix(col, vec3(.3), smoothstep(.005, .0, d - .02));
    col = mix(col, vec3(.0), smoothstep(.005, .0, d2 + .01));

    d = max(d - .04, -d + .01);
    col = mix(col, TRAIN_WHITE, smoothstep(.005, .0, d));

    return col;
}

vec3 render(Hit hit, vec3 col) {
    if ( ! hit.isBackground) {
        vec3 albedo = hit.model.material;
        vec3 light = normalize(vec3(-.5,-1,0));
        float d = dot(hit.normal, light) * .5 + .5;
        float ao = calcAO(hit.pos, hit.normal);
        d = ao;
        vec3 diffuse = vec3(d);
        // d = step(.6, d);
        // d = smoothstep(.2, .8, d);
        diffuse = mix(vec3(.5,.5,.6) * 1., vec3(1), d);
        col = albedo;
        // col *= vec3(ao);
        if (hit.model.id == 1) {
            col = drawNova(col, hit.model.uv);
        }
        // col = vec3(hit.model.uv, 0.);
        // if (hit.model.material == TRAIN_WINDOW) {
        //     diffuse = vec3(1);
        // }
        col *= diffuse;

        // col = mix(col, vec3(0,0,1), d * .05);
        // vec3 ref = reflect(hit.rayDirection, hit.normal);
    }
    if (hit.model.material == DISTANCE_METER_MAT) {
        float dist = map(hit.pos).dist;
        col = distanceMeter(dist * 10., hit.rayLength, hit.rayDirection, 10.);
    }
    return col;
}


// --------------------------------------------------------
// Ray Marching
// Adapted from: https://www.shadertoy.com/view/Xl2XWt
// --------------------------------------------------------

const float MAX_TRACE_DISTANCE = 10.;
const float INTERSECTION_PRECISION = .0001;
const int NUM_OF_TRACE_STEPS = 150;


// Faster runtime
vec3 _calcNormal(vec3 pos){
    vec3 eps = vec3(.0001,0,0);
    vec3 nor = vec3(
        map(pos+eps.xyy).dist - map(pos-eps.xyy).dist,
        map(pos+eps.yxy).dist - map(pos-eps.yxy).dist,
        map(pos+eps.yyx).dist - map(pos-eps.yyx).dist );
    return normalize(nor);
}

// Faster compilation
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
        model = mapDebug(rayOrigin + rayDirection * rayLength);
        currentDist = model.dist;
        rayLength += currentDist * (1. - model.underStep);
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

float roundel(vec2 uv) {
    float radius = 215. + 90. / 2.;
    float inner = radius - 90.;
    float width = 640. / 2.;
    float height = 100. / 2.;
    uv *= 200.;
    uv = abs(uv);
    float bar = uv.x - width;
    bar = max(bar, uv.y - height);
    float circle = length(uv) - radius;
    circle = max(circle, -(length(uv) - inner));
    return min(bar, circle);
}

float backgroundMap(vec2 uv) {
    // return step(0., roundel(uv));

    uv.y *= -1.;
    // uv.y += .05;

    float ss = 1.5;
    uv /= ss;
    // uv.y -= .5;
    // uv.x /= 1.4;
    float r = 3.55;

    vec2 uvw = uv;

    uv.x += time * r;
    vec2 uv2 = uv;
    pMod1(uv.x, r);
    uv.x += 1.2;

    vec2 w = vec2(0);

    // uv2 -= time * 5.;
    pR(uv2, PI * .5);
    uv2 -= time * r;
    w += sin((uv2 / r * PI * 1.) * 2.) * .2;
    w *= sin(uvw.x * PI * 1.5 + PI / 2.) * .5 + .5;
    w += sin(uvw * PI + PI / 2.) * .1;

    uv += w;

    return step(0., fNova(uv, .06) * ss);

    float s = .78;
    uv *= s;
    uv = abs(uv);
    float d = uv.x - 1.25;
    d = max(d, uv.y - .25);
    d = min(d, length(uv) - 1.);
    d /= s;
    d = step(0., d);
    // float r = sin(d * 20. - time * PI * 10.) * .1;
    // d = min(d, abs(d - .2) - .1);
    // d = (d + 1.5) * 5.;
    // d = d * r;
    // d = step(0.5, d);
    return d;
}

void mainImage(out vec4 fragColor, in vec2 fragCoord) {

    time = iTime;
    if (guiAnimation2) {
        time *= 1.5;
    }
    time *= .3;
    time = mod(time, 1.);

    vec2 p = (-iResolution.xy + 2.0*fragCoord.xy)/iResolution.y;

    p.x += 0.5;
    p *= 2.5;
    // float d = fNova(p);
    // vec3 cc = vec3(smoothstep(0.01, .0, d));
    // cc = vec3(0,1,1) * mod(d * 5., 1.) * .5;
    vec3 cc = vec3(0);
    cc = drawNova(cc, p / vec2(-2) + vec2(.5,0));
    fragColor = vec4(cc,1.0);
    return;

    p.y *= -1.;

    camPos = vec3(-1.,0,.25) * .95;
    vec3 camTar = vec3(0,-.0025,0);
    vec3 camUp = vec3(0,0,1);
    mat3 camMat = calcLookAtMatrix(camPos, camTar, camUp);
    focalLength = pow(guiFocalLength, 3.);
    vec3 rayDirection = normalize(camMat * vec3(p, focalLength));

    // camPos = cameraPosition;
    // mat4 camMat = cameraViewMatrix;
    // focalLength = pow(guiFocalLength, 3.);
    // focalLength = 2.5;
    // vec3 rayDirection = normalize(
    //     (vec4(p, -focalLength, 1) * camMat).xyz
    // );

    vec3 bg = vec3(1.);
    bg = mix(bg, TRAIN_WINDOW, 1.-backgroundMap(p));
    // bg = vec3(backgroundMap(p));
    // fragColor = vec4(bg,1);
    // return;

    Hit hit = raymarch(camPos, rayDirection);
    vec3 color = render(hit, bg);
    // vec3 color = bg;

    vec2 uv = fragCoord/iResolution.xy;
    float vig = pow(
        16. * uv.x * uv.y * (1. - uv.x) * (1. - uv.y),
        0.4
    );
    // color *= vig;

    // color *= mix(vec3(1), vec3(.7,1.2,2.5), color.r);

    color = pow(color, vec3(1.,.9,.8));

    color = pow(color, vec3(1. / 2.2)); // Gamma

    fragColor = vec4(color,1);
}



