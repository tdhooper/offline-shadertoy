precision highp float;

uniform vec2 iResolution;
uniform vec2 iOffset;
uniform float iTime;
uniform sampler2D iChannel0;

uniform mat4 cameraMatrix;
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

// --------------------------------------------------------
// Structs
// --------------------------------------------------------

struct Model {
    float dist;
    vec3 material;
};

struct Hit {
    Model model;
    vec3 pos;
    bool isBackground;
    vec3 normal;
    vec3 rayOrigin;
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

// The "Round" variant uses a quarter-circle to join the two objects smoothly:
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

Curve pModTrefoil(inout vec3 p) {
    Curve curve = TrefoilCurve(p);
    float x = dot(p - curve.position, curve.normal);
    float y = dot(p - curve.position, curve.binormal);
    float z = curve.t;
    p = vec3(x, y, z);
    p.z -= 0.0666;
    p.z *= 10.;
    return curve;
}


// --------------------------------------------------------
// Materials
// --------------------------------------------------------

vec3 DEFAULT_MAT = vec3(.9);

vec3 TRAIN_MAT = vec3(.9,.5,.5);
vec3 PLATFORM_MAT = vec3(.5,.9,.5);
vec3 TRACK_MAT = vec3(.9,.9,.2);
vec3 TRAIN_RED = vec3(1,.0,.0);
vec3 TRAIN_GREY = vec3(.4);
vec3 TRAIN_WINDOW = vec3(.5,.9,1.);
vec3 TRAIN_WINDOW_FRAME = vec3(.1);

vec3 STEPS_MAT = vec3(.5,.5,.9);
vec3 HANDRAIL_MAT = vec3(.9,.2,.9);


// --------------------------------------------------------
// Model
// --------------------------------------------------------

Model mTrain(vec3 p, float width) {
    float d = 1e12;
    float len = 1.;
    float height = width * .9;
    p.z = abs(p.z);
    vec3 pp = p;

    d = smin(d, fBox2(p.xy, vec2(width, height)));

    // Slanted side
    p.xy -= vec2(width, height * .3);
    d = smax(d, dot(p.xy, normalize(vec2(1.,-.175))), width * .01);
    p = pp;

    // Round top
    float topRadius = width * 1.13;
    p.y += height - topRadius;
    d = smax(d, length(p.xy) - topRadius, width * .05);
    p = pp;

    // Grey
    float grey = d + .005;
    p.y -= height * .4;
    p.x -= width * .4;
    grey = smax(grey, dot(p.xy, normalize(vec2(.3,1))), .02);
    grey = max(grey, -dot(p.xy, vec2(0,-1)));
    vec3 color = mix(TRAIN_RED, TRAIN_GREY, 1.-step(0., grey));
    p = pp;

    // Carridge
    d = smax(d, dot(p, vec3(0,0,1)) - len, .005);

    // Front door
    vec2 doorWH = vec2(width * .275, height * 1.7);
    vec2 doorXY = vec2(0, height);

    // - Inset
    p.xy -= doorXY;
    p.z -= len;
    d = smax(d, -fBox(p, vec3(doorWH, .01)), .01);

    // - Template
    float door = fBox2(p.xy, doorWH);
    p = pp;

    // Window
    float windowBottom = .005;
    float windowOffset = .015;
    float window = max(door + windowOffset, dot(p.xy, vec2(0,1)) - windowBottom);
    p = pp;

    float window2Offset = .02;
    float window2 = max(-door + window2Offset, dot(p.xy, vec2(0,1)) - .02);
    window2 = max(window2, dot(p.xy, vec2(0,-1)) - doorWH.y + doorXY.y + windowOffset);
    p.xy -= vec2(doorWH.x + window2Offset, windowBottom);
    window2 = max(window2, dot(p.xy, normalize(vec2(-.7,1))));
    window2 = smax(window2, grey + .01, .01);

    window = min(window, window2);
    color = mix(color, TRAIN_WINDOW_FRAME, 1.-step(0., window - .007));
    color = mix(color, TRAIN_WINDOW, 1.-step(0., window));

    Model train = Model(d, color);
    return train;
}

Model mTrainSide(vec3 p, float curveLen, float radius) {
    vec3 pp = p;
    float d = 1e12;

    float trackSize = .001;
    Model track = Model(
        fBox2(p.xy, vec2(1.,trackSize)),
        TRACK_MAT
    );

    p.x -= radius;
    Model platform = Model(
        fBox2(p.xy, vec2(.075)),
        PLATFORM_MAT
    );
    p = pp;

    if (guiAnimation2) {
        p.z += time * (curveLen * 5. / 6.);
    } else {
        p.z += time * curveLen;
    }
    pMod1(p.z, curveLen * .5);
    float trainSize = .175;
    p.y += trainSize + trackSize;
    Model train = mTrain(p, trainSize);
    p = pp;

    Model model = opU(track, platform);
    model = opU(model, train);

    return model;
}

Model mStairSide(vec3 p, float radius) {
    vec3 pp = p;
    float d = 1e12;

    float stairSize = .1;
    float stairWidth = .2;
    if (guiAnimation2) {
        p.z += time * stairSize * 2. * -15. * .5;
    } else {
        p.z += time * stairSize * 2. * -15.;
    }
    pMod1(p.z, stairSize * 2.);
    p.z -= .02;
    pR(p.yz, PI * .2);
    p.y += .05;
    Model steps = Model(
        fBox(p, vec3(stairWidth, stairSize, stairSize)),
        STEPS_MAT
    );
    p = pp;

    p.x -= radius;
    Model handrail = Model(
        fBox2(p.xy, vec2(.12)),
        HANDRAIL_MAT
    );
    p = pp;

    Model model = opU(steps, handrail);
    return model;
}

Model fShape2(vec3 p) {
    float s = 1.;

    // if (length(p) > 2.) {
    //     s = dot(p,p);
    //     s /= 5.;
    //     // p.xz *= -1.;
    //     // pR(p.xz, PI);
    // }

    if (guiTrefoil) {
        if (guiAnimation2) {
            pR(p.xy, time * -PI * 2. * 1./3.);
        } else {
            pR(p.xy, time * PI * 2.);
        }
    }

    p /= s;
    Curve curve;

    if (guiTrefoil) {
        curve = pModTrefoil(p);
        // pR(p.xy, p.x / 10. * PI * 2.);
        pR(p.xy, -time * PI * 2.);
    }

    float curveLen = 10.;

    float radius = .28;
    float outer = length(p.xy) - radius;

    p.y -= .05;
    p.x = abs(p.x);
    vec3 pp = p;

    Model train = mTrainSide(p, curveLen, radius);
    Model stair = mStairSide(p, radius);

    float divide = dot(p, vec3(0,1,0));
    train.dist = max(train.dist, divide);
    stair.dist = max(stair.dist, -divide);

    Model model = opU(train, stair);

    model.dist = max(model.dist, outer);

    model.dist *= s;

    // model.dist -= .03;

    return model;
}

float focalLength;

Model map(vec3 p) {
    float d;
    float s = focalLength;
    p *= s;
    
    Model model = fShape2(p);

    if ( ! guiTrefoil) {
        model.dist = max(model.dist, fBox2(p.zy, vec2(2.)));
    }

    model.dist /= s;

    return model;
}


// --------------------------------------------------------
// Rendering
// --------------------------------------------------------

vec3 camPos;

vec3 render(Hit hit){
    vec3 col;
    vec3 bg = vec3(.75);
    col = bg;
    if ( ! hit.isBackground) {
        vec3 albedo = hit.model.material;
        vec3 light = normalize(vec3(-.5,-1,0));
        float d = dot(hit.normal, light) * .5 + .5;
        d = mix(.5, 1., step(.6, d));
        vec3 diffuse = vec3(d);
        col = albedo;
        col *= diffuse;
        // vec3 ref = reflect(hit.rayDirection, hit.normal);
    }
    return col;
}


// --------------------------------------------------------
// Ray Marching
// Adapted from: https://www.shadertoy.com/view/Xl2XWt
// --------------------------------------------------------

const float MAX_TRACE_DISTANCE = 10.;
const float INTERSECTION_PRECISION = .001;
const int NUM_OF_TRACE_STEPS = 150;
const float FUDGE_FACTOR = .5;


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

    time = iTime / 5.;
    if (guiAnimation2) {
        time *= 1.5;
    }
    time = mod(time, 1.);

    vec2 p = (-iResolution.xy + 2.0*fragCoord.xy)/iResolution.y;

    // vec3 camPos = vec3(0,0,guiZoom);
    // pR(camPos.xz, time);
    // vec3 camTar = vec3(0);
    // vec3 camUp = vec3(0,1,0);
    // mat3 camMat = calcLookAtMatrix(camPos, camTar, camUp);

    camPos = cameraPosition;
    mat4 camMat = cameraMatrix;

    focalLength = pow(guiFocalLength, 3.);
    focalLength = 2.5;
    vec3 rayDirection = normalize(
        (vec4(p, -focalLength, 1) * camMat).xyz
    );

    Hit hit = raymarch(camPos, rayDirection);

    vec3 color = render(hit);
    color = pow(color, vec3(1. / 2.2)); // Gamma
    fragColor = vec4(color,1);
}



