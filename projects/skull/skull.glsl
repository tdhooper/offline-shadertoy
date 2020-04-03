


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






// iq https://www.shadertoy.com/view/MldfWn
float sdEllipse( vec2 p, in vec2 ab )
{
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
    
    return length(r-p) * sign(p.y-r.y);
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

float fPipe90FlatOld(vec3 p, float smallRadius, float largeRadius) {
    float d = length(max(p.xz, vec2(0))) - largeRadius;
    d = min(d, max(p.z - largeRadius, 0.));
    vec2 c = vec2(d, max(p.y, 0.));
    return length(c) - smallRadius;
}

float fPipe90Flat(vec3 p, float smallRadius, float largeRadius) {
    float d = length(max(p.xz, vec2(0))) - largeRadius;
    // d = min(d, max(p.z - largeRadius, 0.));
    d = min(d, 0.);
    vec2 c = vec2(d, max(p.y, 0.));
    return length(c) - smallRadius;
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
    // p = pp;
    // p -= vec3(-.06,.055,-.08);
    // part = dot(p, normalize(vec3(-2,3,-1.3)));
    // d = smax(d, part, .02);

    // join back
    // p = pp;
    // p -= vec3(0,-.005,.1);
    // part = fCorner(p.zy * vec2(-1,1), .015);
    // d = smax(d, -part, .02);

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

float fMaxilla(vec3 p) {
    float d = 1e12;

    // Gum
    vec3 pp = p;
    p = pRx(p - vec3(0,.42,-.29), .4);
    float gum = sdEllipsoidXXZ(p, vec2(.2, .27));
    d = gum;

    // Gum base (used later)
    p = pRx(p, .02);
    pCurve(p.zxy, -.8);
    float base = p.y;

    // Core
    p = pp;
    p = pRx(p - vec3(0,.33,-.32), .3);
    float maxilla = length(p.xz) - .18;
    maxilla = smax(maxilla, -(length((p - vec3(0,0,.1)).xz) - .1), .1);
    maxilla = smax(maxilla, p.y - .02, .1);
    maxilla = smax(maxilla, -p.y - .2, .1);
    d = smin(d, maxilla, .07);
    p = pp;

    // Cheek
    p -= vec3(.2,.28,-.39);
    float t = dot(p, normalize(vec3(7,3,6))) - .06;
    t = smin(t, dot(p, normalize(vec3(-1,1,15))) - .03, .02);
    t = smax2(t, -dot(p, normalize(vec3(-3,-6,10))) - .055, .06);
    // t = smax2(t, -dot(p, normalize(vec3(-2.5,1.5,.7))) - .06, .01);
    d = smin(d, t, .04);
    p = pp;

    // float foramen = length(p - vec3(.17,.27,-.475)) - .0001;
    // d = smax(d, -foramen, .02);

    // Nose bridge
    float bridge = length((p - vec3(0,.055,-.71)).zy) - .17;
    d = smax(d, -bridge, .01);
    float bridge2 = length(pRx(p - vec3(.13,.1,-.6), 1.1).xy) - .12;
    d = smax(d, -bridge2, .02);

    // Eye socket part 1
    float socket = length(p - vec3(.18,.1,-.47)) - .11;
    d = smax(d, -socket, .05);

    // Nose hole
    p -= vec3(.0,.25,-.5);
    p = pRx(p, .4);
    float nosb = sdEllipse(p.xy - vec2(.0,.005), vec2(.02,.075));
    p = pRz(p, -.5);
    float nos = sdEllipse(p.xy, vec2(.05,.1));
    nos = max(nos, -p.z-.15);
    nosb = max(nosb, -p.z-.15);
    d = smin(d, nos, .075);
    d = smin(d, nosb, .05);
    d = smax(d, -nos, .04);
    p = pp;

    // Gum back
    p = pRx(p - vec3(0,.42,-.29), .1);
    float gumback = pRy(p, .55).z - .01;
    gumback = smin(gumback, pRy(p, -.55).z - .08, .04);
    d = smax(d, gumback, .08);
    p = pp;

    // Gum part 2
    d = smax(d, base, .02);
    float roof = sdEllipsoidXXZ(p - vec3(0,.47,-.28), vec2(.13, .22));
    d = smax(d, -roof, .03);

    // Back of maxilla
    p = p - vec3(.17,.3,-.24);
    float part = dot(p, normalize(vec3(1,-.2,1)));
    d = smax(d, part, .03);
    p = pp;

    // Eye socket part 2
    float cut = fCorner(pRz(pRx(p - vec3(.04,.18,-.45), .25), .15).zy * vec2(-1,1), .01);
    d = smax(d, -cut, .04);
    float cut2 = fCorner(pRx(p - vec3(0,.07,-.5), -.6).zy - vec2(.1,0), 0.);
    d = smax(d, -cut2, .02);

    return d;
}

float fBrow2(vec3 p) {
    vec3 pp = p;
    p = pRx(pRy(pRz(p - vec3(.13,.05,-.42), .3), .4), .4);
    float d = fPipe90Flat(-p.xzy, .02, .12);
    float f = -p.z - .02;
    p = pp;
    p = pRx(p - vec3(0,-.11,-.5), -.3);
    float part = -p.z;
    float mask = smax(f, part, .05);
    d = max(d, mask);
    return d;
}

float fBrow(vec3 p) {
    vec3 pp = p;
    float d;
    p = pRx(p - vec3(0,-.11,-.555), -.26);
    d = -p.z;
    return d;
}

float fSocketBump(vec3 p) {
    p = pRz(p - vec3(.13,.04,-.35), .5);
    return sdEllipsoidXXZ(p.xzy, vec2(.19,.13));
    return length(p) - .17;
}

float fSocketBump2(vec3 p) {
    p -= vec3(.13,.09,-.39);
    return length(p) - .17;
}

float fSocketCut(vec3 p) {
    p = pRz(p - vec3(.16,.08,-.4), .2);
    p.z = max(p.z, 0.);
    return sdEllipsoidXXZ(p.xzy, vec2(.08,.06));
}

float sdSkull(vec3 p) {
    // return fPipe90(p, .1, .5);

    p.x = abs(p.x);

    // p = pRx(pRy(pRz(p - vec3(.13,.05,-.42), .3), .4), .4);
    // return fPipe90Flat(-p.xzy, .02, .12);

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
    // float foreheadlower = length(p - vec3(0,-.0,-.25)) - .3;
    // d = smin(d, foreheadlower, .05);
    // return d;
    float socketbump = smin(fSocketBump(p), fSocketBump(p * vec3(-1,1,1)), .15);
    d = smin(d, socketbump, .11);
    float socketcut = smin(fSocketCut(p), fSocketCut(p * vec3(-1,1,1)), .2);
    d = smax(d, -socketcut, .12);
    p = pRz(p - vec3(.15,.06,-.38), .8);
    float socket = sdEllipsoidXXZ(p.xzy, vec2(.13,.1));
    d = smax(d, -socket-.02, .04);
    p = pp;

    float backbump = sdEllipsoidXXZ(pRx(pRy(p - vec3(.27,-.29,.0), -.25), .0), vec2(.1, .5) * .25);
    d = smin(d, backbump, .34);
    float topbump = sdEllipsoidXXZ(p - vec3(0,-.33,-.05), vec2(.1, .15) * .5);
    d = smin(d, topbump, .3);
    float side = sdEllipsoidXXZ(pRz(p - vec3(.25,.05,-.0), .3).yzx, vec2(.1, .05) * .8);
    d = smin(d, side, .25);
    return d;

    float sphenoid = sdEllipsoidXXZ(pRy(pRz(p - vec3(.1,.2,-.2), .4), -.5).yzx, vec2(.05, .025));
    d = smin(d, sphenoid, .3);
    float sphenoidcut = sdEllipsoidXXZ(pRx(pRz(p - vec3(.4,.1,-.35), .4), .3).xzy, vec2(.005, .25) * .5);
    d = smax(d, -sphenoidcut, .3);
    float cranium = d;
    p = pp;

    return d;

    float maxilla = fMaxilla(p);
    maxilla = smin(maxilla, cranium, .05);

    float zygomatic = fZygomatic(p);
    zygomatic = smin(zygomatic, cranium, .08);

    float arch = fZygomaticArch(p);
    arch = smin(arch, cranium, .08);

    float brow = fBrow(p);
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
