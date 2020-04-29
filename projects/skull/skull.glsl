
#define saturate(x) clamp(x, 0., 1.)

float smin(float a, float b, float k){
    float f = clamp(0.5 + 0.5 * ((a - b) / k), 0., 1.);
    return (1. - f) * a + f  * b - f * (1. - f) * k;
}

float smax(float a, float b, float k) {
    return -smin(-a, -b, k);
}

float smin2(float a, float b, float r) {
    vec2 u = max(vec2(r - a,r - b), vec2(0));
    return max(r, min (a, b)) - length(u);
}

float smax2(float a, float b, float r) {
    vec2 u = max(vec2(r + a,r + b), vec2(0));
    return min(-r, max (a, b)) + length(u);
}

float smin3(float a, float b, float k){
    return min(
        smin(a, b, k),
        smin2(a, b, k)
    );
}

float smax3(float a, float b, float k){
    return max(
        smax(a, b, k),
        smax2(a, b, k)
    );
}

void pR(inout vec2 p, float a) {
    p = cos(a)*p + sin(a)*vec2(p.y, -p.x);
}

// Shortcut for 45-degrees rotation
void pR45(inout vec2 p) {
    p = (p + vec2(p.y, -p.x))*sqrt(0.5);
}

vec3 pRx(vec3 p, float a) {
    pR(p.yz, a); return p;
}

vec3 pRy(vec3 p, float a) {
    pR(p.xz, a); return p;
}

vec3 pRz(vec3 p, float a) {
    pR(p.xy, a); return p;
}

float range(float vmin, float vmax, float value) {
    return clamp((value - vmin) / (vmax - vmin), 0., 1.);
}



// sdUberprim with precomputed constants
float sdUnterprim(vec3 p, vec4 s, vec3 r, vec2 ba, float sz2) {
    vec3 d = abs(p) - s.xyz;
    float q = length(max(d.xy, 0.0)) + min(0.0,max(d.x,d.y)) - r.x;
    // hole support: without this line, all results are convex
#ifndef CONVEX    
    q = abs(q) - s.w;
#endif
    
    vec2 pa = vec2(q, p.z - s.z);
    vec2 diag = pa - vec2(r.z,sz2) * clamp(dot(pa,ba), 0.0, 1.0);
    vec2 h0 = vec2(max(q - r.z,0.0),p.z + s.z);
    vec2 h1 = vec2(max(q,0.0),p.z - s.z);
    
    return sqrt(min(dot(diag,diag),min(dot(h0,h0),dot(h1,h1))))
        * sign(max(dot(pa,vec2(-ba.y, ba.x)), d.z)) - r.y;
}

// s: width, height, depth, thickness
// r: xy corner radius, z corner radius, bottom radius offset
float sdUberprim(vec3 p, vec4 s, vec3 r) {
    // these operations can be precomputed
    s.xy -= r.x;
#ifdef CONVEX  
    r.x -= r.y;
#else
    r.x -= s.w;
    s.w -= r.y;
#endif
    s.z -= r.y;
    vec2 ba = vec2(r.z, -2.0*s.z);
    return sdUnterprim(p, s, r, ba/dot(ba,ba), ba.y);
}

float vmin(vec2 v) {
    return min(v.x, v.y);
}

float vmax(vec3 v) {
    return max(max(v.x, v.y), v.z);
}

float vmax(vec2 v) {
    return max(v.x, v.y);
}

float fBox(vec3 p, vec3 b) {
    vec3 d = abs(p) - b;
    return length(max(d, vec3(0))) + vmax(min(d, vec3(0)));
}

float fCorner(vec3 p, float r) {
    vec3 d = p + r;
    return length(max(d, vec3(0))) + vmax(min(d, vec3(0))) - r;
}

float fCorner(vec2 p, float r) {
    vec2 d = p + r;
    return length(max(d, vec2(0))) + vmax(min(d, vec2(0))) - r;
}

// https://www.shadertoy.com/view/4sS3zz
float sdEllipseApprox( in vec2 p, in vec2 r ) 
{
    float k0 = length(p/r);
    float k1 = length(p/(r*r));
    return k0*(k0-1.0)/k1;
}

// iq https://www.shadertoy.com/view/MldfWn
// modified to fix glitches
float sdEllipse( vec2 p, in vec2 ab )
{
    float approx = sdEllipseApprox(p, ab);

    p = abs( p ); if( p.x > p.y ){ p=p.yx; ab=ab.yx; }

    float l = ab.y*ab.y - ab.x*ab.x;
    
    float m = ab.x*p.x/l; 
    float n = ab.y*p.y/l; 
    float m2 = m*m;
    float n2 = n*n;
    
    float c = (m2 + n2 - 1.0)/3.0; 
    float c3 = c*c*c;

    float q = c3 + m2*n2*2.0;
    float d = c3 + m2*n2;
    float g = m + m*n2;

    float co;

    if( d<0.0 )
    {
        float h = acos(q/c3)/3.0;
        float s = cos(h);
        float t = sin(h)*sqrt(3.0);
        float rx = sqrt( -c*(s + t + 2.0) + m2 );
        float ry = sqrt( -c*(s - t + 2.0) + m2 );
        co = ( ry + sign(l)*rx + abs(g)/(rx*ry) - m)/2.0;
    }
    else
    {
        float h = 2.0*m*n*sqrt( d );
        float s = sign(q+h)*pow( abs(q+h), 1.0/3.0 );
        float u = sign(q-h)*pow( abs(q-h), 1.0/3.0 );
        float rx = -s - u - c*4.0 + 2.0*m2;
        float ry = (s - u)*sqrt(3.0);
        float rm = sqrt( rx*rx + ry*ry );
        co = (ry/sqrt(rm-rx) + 2.0*g/rm - m)/2.0;
    }

    float si = sqrt( 1.0 - co*co );
 
    vec2 r = ab * vec2(co,si);
    
    float accurate = length(r-p) * sign(p.y-r.y);
    
    // bound the quartic solution by the approximation to stop floating point errors getting out of hand
    accurate = clamp(abs(accurate), abs(approx) - .5, abs(approx) + .5) * sign(approx);

    // use the approximation near the surface so normal calculation looks smooth
    return mix(approx, accurate, smoothstep(.0, .01, abs(approx)));
}

// generic ellipsoid - approximated distance: https://www.shadertoy.com/view/tdS3DG
float sdEllipsoid( in vec3 p, in vec3 r ) 
{
    float k0 = length(p/r);
    float k1 = length(p/(r*r));
    return k0*(k0-1.0)/k1;
}

// symmetric ellipsoid - EXACT distance
float sdEllipsoidXXZ( in vec3 p, in vec2 r ) 
{
    return sdEllipse( vec2( length(p.xy), p.z ), r );
}

float sdEllipsoidXXZPill(vec3 p, vec2 r) {
    p.z = min(p.z, 0.);
    return sdEllipsoidXXZ(p, r);
}

// Torus in the XZ-plane
float fTorus(vec3 p, float smallRadius, float largeRadius) {
    return length(vec2(length(p.xz) - largeRadius, p.y)) - smallRadius;
}

float fEllipseTorus(vec3 p, vec2 smallRadius, float largeRadius) {
    vec2 c = vec2(length(p.xz), p.y);
    c.x -= largeRadius;
    return sdEllipse(c, smallRadius);
}

float fTorusEdge(vec3 p, float smallRadius, float largeRadius) {
    p.x += largeRadius;
    return fTorus(p, smallRadius, largeRadius);
}

float fPipe90(vec3 p, float smallRadius, float largeRadius) {
    float d = length(max(p.xz, vec2(0))) + vmax(min(p.xz, vec2(0)));
    vec2 c = vec2(d - largeRadius, p.y);
    return length(c) - smallRadius;
}

float fCone(vec3 p, float angle) {
    vec2 c = vec2(length(p.xz), p.y);
    pR(c, angle);
    return length(max(c, vec2(0))) + vmax(min(c, vec2(0)));
}

float fPillHalf(vec3 p) {
    p.y = min(p.y, 0.);
    return length(p);
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

// curve the x axis around the y axis
void pCurve(inout vec3 p, float r) {
    p.z -= r;
    r = abs(r);
    p = vec3(atan(p.x, -p.z) * r, p.y, length(p.xz) - r);
}

float fZygomatic2(vec3 p) {
    vec3 pp = p;
    p = pRy(pRx(pRz(p - vec3(.35,.215,-.26), .3), .12), -.2);
    float largeRadius = .35;
    p.x += largeRadius;
    vec3 c = vec3(atan(p.z, p.x), length(p.xz), p.y);
    float top = -.02;
    float bottom = .02;
    top = mix(top, -.04, smoothstep(.4, -.27, c.x));
    top = mix(top, -.1, smoothstep(-.12, -.27, c.x));
    top = mix(top, -.17, smoothstep(-.1, -.45, c.x));
    bottom = mix(bottom, .12, smoothstep(.3, -.33, c.x));
    bottom = mix(bottom, .04, smoothstep(-.3, -.7, c.x));
    float width = .01;
    width = mix(width, .03, smoothstep(.2, -.6, c.x));
    vec2 smallRadius = vec2(width, distance(top, bottom) / 2.);
    c.y -= largeRadius;
    c.z -= top + smallRadius.y;
    float rot = smoothstep(.3, -.27, c.x);
    pR(c.yz, rot * -.3);
    float d = sdEllipse(c.yz, smallRadius);
    p = pp;
    p = pRy(p - vec3(.2,.125,-.45), 1.);
    float cut = length(p.xy) - .11;
    p = pp;
    p = pRy(pRz(p - vec3(.35,0,0), .4), -.1);
    cut = min(cut, p.x);
    d = smax(d, -cut, .04);
    return d;
}

float fZygomatic(vec3 p) {
    float o = 1e12;
    float s = 1e12;
    float part;
    p -= vec3(.33,.23,-.35);
    vec3 pp = p;
    
    // surface base
    part = dot(p, normalize(vec3(1,-.05,-.4)));
    s = min(s, part);

    // surface top curve
    p = pp;
    p -= vec3(0,-.07,0);
    part = dot(p, normalize(vec3(1.,.07,-.4)));
    s = smin(s, part, .01);

    // surface front curve
    p -= vec3(-.06,.07,-.09);
    part = dot(p, normalize(vec3(.8,.1,-1)));
    s = smax(s, part, .03);

    // cheek bump
    p = pp;
    p -= vec3(-.06,.02,-.03);
    s = smin(s, length(p) - .01, .1);

    // surface back
    float s2 = -(s + .04);
    p = pp;
    p = pRy(p - vec3(-.06,-.12,-.1), -.2);
    part = length(p.zy) - .12;
    s2 = smin(s2, part, .02);

    s = max(s, s2);

    // outline eye
    p = pp;
    p = pRz(pRy(p - vec3(-.12,-.12,-.08), .6), .4);
    part = sdEllipse(p.xy, vec2(.08,.095));
    p = pRz(p - vec3(.035,.08543,0), -.5);
    if (p.x < 0.) part = p.y;
    part = smax(part, p.z-.09, .12);
    o = -part;

    // return part;

    // outline top left corner
    p = pp;
    p = p - vec3(.0,-.06,.04);
    p = p * vec3(1,1,-1);
    pCurve(p.yxz, .25);
    part = fCorner(p.zy * vec2(-1,1), .03);
    o = smax(o, -part, .04);

    // outline bottom
    p = pp;
    p = pRx(p - vec3(.0,.072,.065), .55);
    p = p * vec3(1,-1,1);
    pCurve(p.zxy, .5);
    part = -p.y;
    o = max(o, -part);

    float d = smax(o, s, .05);

    // inside corner
    p = pp;
    p = pRz(pRx(pRy(p - vec3(-.05,-.03,-.02), 1.1), .3), -.2);
    part = fCorner(p * vec3(1,1,-1), .04);
    d = smax(d, -part, .02);

    // join front
    p = pp;
    p -= vec3(-.06,.055,-.08);
    part = dot(p, normalize(vec3(-2,3,-1.3)));
    d = smax(d, part, .02);

    // join back
    p = pp;
    p -= vec3(0,-.005,.1);
    part = fCorner(p.zy * vec2(-1,1), .015);
    d = smax(d, -part, .02);

    // join behind
    p = pp;
    p -= vec3(-.1,-.04,0);
    part = dot(p, vec3(1,.2,-.3));
    d = smax(d, -part, .02);

    // join top
    p = pp;
    p -= vec3(-.04,-.19,-.03);
    part = dot(p, vec3(.3,1,-.2));
    // return part;
    d = smax(d, -part, .02);


    // p = pp;
    // p = p - vec3(-.12,-.12,-.08);
    // d = length(p) - .01;
    // d = min(d, dot(p, normalize(vec3(1,0,0))));

    return d;
}

float fZygomaticArch(vec3 p) {
    return fZygomatic2(p);

    vec3 pp = p;
    p = pRy(pRx(pRz(p - vec3(.35,.215,-.26), .3), .12), -.2);
    p.x += .35;
    vec2 c = vec2(length(p.xz), p.y);
    c.x -= .35;
    float d = sdEllipse(c, vec2(.01, .02));
    return d;
}

float fMaxillaCore(vec3 p) {
    float d = 1e12;
    vec3 pp = p;

    // Core
    p = pRx(p - vec3(0,.33,-.32), .3);
    d = length(p.xz) - .18;
    d = smax(d, -(length((p - vec3(0,0,.1)).xz) - .1), .1);
    d = smax(d, p.y - .02, .1);
    d = smax(d, -p.y - .1, .1);
    p = pp;

    // Gum
    p = pRx(p - vec3(0,.42,-.29), .4);
    d = smin(d, sdEllipsoidXXZ(p, vec2(.2, .27)), .07);
    p = pp;

    // Gum back inner
    p = pRx(p - vec3(0,.42,-.29), .1);
    float gumback = pRy(p, .55).z - .01;
    gumback = smin(gumback, pRy(p, -.55).z - .08, .04);
    d = smax(d, gumback, .08);
    p = pp;

    // Gum back outer
    p = p - vec3(.17,.3,-.24);
    float part = dot(p, normalize(vec3(1,-.2,1)));
    d = smax(d, part, .03);
    p = pp;

    return d;
}

float fMaxillaBottom(vec3 p) {
    float b = p.y - .33;
    p = pRx(p - vec3(0,.42,-.29), .42);
    pCurve(p.zxy, -.8);
    float d = p.y;
    return min(d, b);
}

float fSocketBump(vec3 p) {
    vec3 pp = p;
    p = pRz(pp - vec3(.13,.04,-.35), .2);
    float d = sdEllipsoidXXZ(p.xzy, vec2(.19,.13));
    p = pp - vec3(.14,.15,-.35);
    d = smin(d, sdEllipsoidXXZ(p, vec2(.19,.16)), .07);
    p = pp - vec3(.24,.03,-.38);
    d = smin(d, length(p) + .005, .15);
    return d;
}

float fSocketInset(vec3 p) {
    vec3 pp = p;
    p = pRz(pp - vec3(.16,.08,-.4), .3);
    p.z = max(p.z, 0.);
    float d = sdEllipsoidXXZ(p.xzy, vec2(.08,.05));
    p = pRz(pp - vec3(.18,.15,-.4), .2);
    d = smin(d, sdEllipsoidXXZ(p.xzy, vec2(.042,.02)), .16);
    p = pp - vec3(.235,.1,-.43);
    d = smin(d, length(p) - .023, .08);
    p = pp - vec3(.26,.11,-.36);
    d = smax(d, dot(p, normalize(vec3(1,.2,1))) + .01, .04);
    return d;
}

float fSocket(vec3 p) {
    vec3 pp = p;
    p = pRz(pp - vec3(.15,.06,-.38), .7);
    float d = sdEllipsoidXXZ(p.xzy, vec2(.11,.08));
    p = pRz(pp - vec3(.15,.16,-.38), -.15);
    d = smin(d, sdEllipsoidXXZ(p.xzy, vec2(.08,.03)), .1);
    p = pp - vec3(.26,.11,-.36);
    d = smax(d, dot(p, normalize(vec3(1.5,.2,1))) + .01, .04);
    return d;
}

float fNoseShape(vec3 p) {
    p = pRz(p, -.4);
    p.xy -= vec2(-.055, 0);  
    p.z = max(abs(p.z) - .15, 0.);
    return sdEllipsoidXXZ(p.xzy, vec2(.1, .18));
}

float fNose(vec3 p) {
    p = pRx(p - vec3(.0,.25,-.5), .4);
    float d = smax(fNoseShape(p), fNoseShape(p * vec3(-1,1,1)), .02);
    d = smax(d, p.y-.09, 0.04);
    d = smax(d, -sdEllipse(p.xy - vec2(0,.18), vec2(.02,.1)), 0.03);
    d = smax(d, p.z-.05, .05);
    return d;
}

float fNoseCut(vec3 p) {
    float r = .3;
    p = pRy(pRx(p - vec3(0,.49,-.57), -.45), .35);
    return length(p.yz + vec2(0,r)) - r;
}

float fArchhole(vec3 p) {
    p = pRz(pRy(p - vec3(.3,.15,-.25), -.4), .3);
    vec3 pp = p;
    p -= vec3(.045,0,0);
    float d = dot(p, normalize(vec3(1,0,-.12))) + .01;
    p = pp - vec3(0,0,-.085);
    d = smax(d, -dot(p, normalize(vec3(0,-.14,1))), .05);
    p = pRy(pRz(pRx(pp - vec3(.035,.1,-.08), -.29), -.2), .6);
    float h = .1;
    p.z += .013;
    p.z -= h;
    d = smin(d, sdEllipse(p.xz, vec2(.03, h)), .04);
    p = pp - vec3(.015,-.29,0);
    d = smax(d, -dot(p, normalize(vec3(-.1,1,.35))), .1); // top
    p = pp - vec3(-.05,0,-.05);
    d = smax(d, dot(p, normalize(vec3(-1,-.05,-.4))) - .005, .05);
    p = pp - vec3(-.038,-.1,.05);
    d = smax(d, dot(p, normalize(vec3(-.8,-.21,.08))) - .005, .03);
    p = pRz(pRy(pp - vec3(0,-.084,.25), .4), -.27);
    d = smax(d, -(length(p.zy) - .16), .15);
    p = pp - vec3(0,.2,.04);
    d = smax(d, dot(p, normalize(vec3(-.2,.8,1))), .05);
    p = pp;
    d = smax(d, p.y-.25, .1);
    return d;
}



float sdSkull(vec3 p) {
    bool db = p.x > 0.;
    p.x = abs(p.x);
    vec3 pp = p;
    float d = 1e12;
    float back = sdEllipsoidXXZ(p - vec3(0,-.11,.16), vec2(.4, .32));
    d = min(d, back);
    float base = length(p - vec3(0,-.08,.25)) - .15;
    d = smin(d, base, .3);
    float baseside = length(p - vec3(.2,.1,.25)) - .05;
    d = smin(d, baseside, .25);
    float forehead = sdEllipsoidXXZ(pRx(p - vec3(0,-.15,-.14), .5), vec2(.35, .44) * .97);
    d = smin(d, forehead, .22);
    float foreheadside = length(p - vec3(.17,-.13,-.3)) - .05;
    d = smin(d, foreheadside, .25);
    float socketbump = smin(fSocketBump(p), fSocketBump(p * vec3(-1,1,1)), .15);
    d = smin(d, socketbump, .09);

    p = pRx(p - vec3(0,.23,-.45), -.55);
    p.y = max(abs(p.y) - .15, 0.);
    float bridge = sdEllipsoidXXZ(p, vec2(.06, .15));
    d = smin(d, bridge, .1);
    p = pp;

    p = pRy(pRx(p - vec3(.22,.3,-.4), .4), -.1);
    float cheek = smax(sdEllipsoidXXZ(p.zyx, vec2(.02, .05)), -p.z, 0.05);
    d = smin(d, cheek, 0.1);
    p = pp;
    float maxilla = fMaxillaCore(p);
    d = smin(d, maxilla, .08);
    float foramen = fCone(pRx(pRy(p - vec3(.17,.27,-.465), .5), -.5) * vec3(1,-1,1), .6);
    foramen = smax(foramen, p.y-.45, .1);
    d = smax(d, -foramen - .01, .05);

    float socketinset = smin(fSocketInset(p), fSocketInset(p * vec3(-1,1,1)), .2);
    d = smax(d, -socketinset, .12);
    float socket = fSocket(p);
    d = smax(d, -socket, .04);
    float backbump = sdEllipsoidXXZ(pRx(pRy(p - vec3(.27,-.29,.0), -.25), .0), vec2(.1, .5) * .25);
    d = smin(d, backbump, .34);
    float topbump = sdEllipsoidXXZ(p - vec3(0,-.33,-.05), vec2(.1, .15) * .5);
    d = smin(d, topbump, .3);

    float side = sdEllipsoidXXZ(pRz(p - vec3(.2,.05,-.0), .3).yzx, vec2(.1, .05));
    d = smin(d, side, .25);

    // bridge adjust
    p = pRx(p - vec3(0,.2,-.5), -.2);
    p.y = max(abs(p.y + .1) - .2, 0.);
    float ba = length(p) - .03;
    d = smin(d, ba, .05);
    p = pp;

    // canine socket
    p = pRy(pRz(p - vec3(.14,.55,-.495), -.4), .5);
    p.x = max(abs(p.x + .03) - .03, 0.);
    p.y = max(abs(p.y + .1) - .09, 0.);
    float caninesocket = length(p) - .01;
    d = smin(d, caninesocket, .03);
    p = pp;

    // Nose
    float nos = fNose(p);
    d = p.z > -.3 ? d : smin(d, nos-.01, .02);
    p = pp;

    float nosecut = smin(fNoseCut(p), fNoseCut(p * vec3(-1,1,1)), .04);
    d = smax(d, -nosecut, .01);
    p = pp;

    // Nose hole
    float nosb = fPillHalf((p - vec3(.0,.362,-.54)).xzy) - .005;
    nosb = max(nosb, p.z+.4);
    d = smin(d, nosb, .05);
    d = smax(d, -nos+.005, .02);

    p = pp;
    float maxillaBottom = fMaxillaBottom(p);
    d = smax(d, maxillaBottom, .02);

    float roof = sdEllipsoidXXZ(p - vec3(0,.47,-.28), vec2(.13, .22));
    d = smax(d, -roof, .03);


    float temporal = sdEllipsoidXXZ(pRy(pRz(p - vec3(.25,.1,-.03), .2), -.4).yzx, vec2(.18, .08));
    temporal = smax(temporal, dot(p.zy, normalize(vec2(-.3,1))) - .2, .1);
    d = smin(d, temporal, .15);

    float archhole = fArchhole(p);
    d = smax(d, -archhole, .03);

    // return archhole;

    //float zygomatic = fZygomatic(p);
    // d = smin(d, zygomatic, .0);

    // float arch = fZygomaticArch(p);
    // d = smin(d, arch, .08);


    // float r = .3;
    // p = pRx(pRz(pRy(p - vec3(.3,.28,-.2), -.5), .2), .5);
    // float sidecut = sdEllipsoidXXZ(p.zyx, vec2(.4, .02));
    // sidecut = smax(sidecut, -p.z+.00, .1);
    // // sidecut = smax(sidecut, -p.x, .05);
    // sidecut -= .01;
    // d = smax(d, -sidecut, .05);
    // return max(sidecut-.1, -p.z-.15);




    // p = pRy(pRz(pRx(p - vec3(.32,-.0,-.34), .6), .3), -.6);
    // p.z = min(p.z, 0.);
    // d = smax(d, -(fPillHalf(p) - .02), .05);
    // return fPillHalf(p) - .02;


/*
    float cranium = d;

    return cranium;
    p = pp;

    float maxilla = fMaxilla(p);
    maxilla = smin(maxilla, cranium, .0);

    return maxilla;

    float zygomatic = fZygomatic(p);
    zygomatic = smin(zygomatic, cranium, .08);

    float arch = fZygomaticArch(p);
    arch = smin(arch, cranium, .08);

    // float brow = fBrow(p);
    // brow = smin(brow, cranium, .08);

    // return cranium;
    // return brow;

    float join;

    p -= vec3(.2,.28,-.39);
    join = dot(p, normalize(vec3(-2.5,1.5,.7))) + .06;
    p = pp;

    d = mix(zygomatic, maxilla, smoothstep(-.03, .03, join));

    p -= vec3(.33,.23,-.35);
    p -= vec3(0,-.005,.1);
    join = dot(p, vec3(.4,0,1));
    p = pp;

    d = mix(d, arch, smoothstep(0., .08, join + .03));
*/
    // d = smin2(d, zygomatic, .0);


    // return zygomatic;

    // float zygomatic = fZygomatic(p);
    // d = smin(d, zygomatic, .1);
    // p = pRy(pRz(p - vec3(.18,.105,-.47), .15), .4);
    // pCurve(p, .25);
    // float socket = fTorus(p.yzx, .02, .1);
    // float thic = .02;
    // socket = sdUberprim(p, vec4(.155,.12,thic,0), vec3(.12,thic,thic));
    // d = smin(d, socket, .05);

    // p -= vec3(.15,.1,-.5);
    // pCurve(p, .1);
    // d = fTorus((p).yzx, .02, .1);
    // d = length(p.yz) - .1;
    // d = max(d, -p.x);

    // d = topbump;
    // d = 1e12;
    // d = min(d, abs(p.x) - .001);
    // d = min(d, abs(p.y) - .001);
    // d = min(d, abs(p.z) - .001);
    // d = max(d, length(p) - .7);
    return d;
}

#pragma glslify: export(sdSkull)
