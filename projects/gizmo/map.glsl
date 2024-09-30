vec3 GIZMO_LOCAL_P;

void GIZMO(vec3 p) {
    GIZMO_LOCAL_P = p;
}




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



float vmax(vec3 v) {
    return max(max(v.x, v.y), v.z);
}

float vmax2(vec2 v) {
    return max(v.x, v.y);
}

float fBox(vec3 p, vec3 b) {
    vec3 d = abs(p) - b;
    return length(max(d, vec3(0))) + vmax(min(d, vec3(0)));
}

//https://www.shadertoy.com/view/wtfyWj
float sdEllipse(vec2 p, vec2 ab){
    p = abs(p);
    float t = 0.785398;
    vec2 xy;
    for (int i=0;i<3;i++){
        vec2 cs = vec2(cos(t),sin(t));
        xy = ab*cs;
        vec2 e = (ab.x*ab.x-ab.y*ab.y)*vec2(1,-1)*cs*cs*cs/ab;
        vec2 r = xy-e, q = p-e;
        //float rm = length(r), qm = length(q);
        //float dc = rm*asin((r.x*q.y-r.y*q.x)/(rm*qm));
        float dc = (r.x*q.y-r.y*q.x)/length(q);
        float dt = dc/sqrt(dot(ab,ab)-dot(xy,xy));
        t += dt;
        t = min(1.570796,max(0.,t));
    }
    vec2 q = p/ab;
    return sign(dot(q,q)-1.)*length(p-xy);
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

float fCone(vec3 p, float angle) {
    vec2 c = vec2(length(p.xz), p.y);
    pR(c, angle);
    return length(max(c, vec2(0))) + vmax2(min(c, vec2(0)));
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
    float b = p.z + .15;
    p = pRx(p - vec3(0,.42,-.29), .42);
    pCurve(p.zxy, -.8);
    float d = p.y;
    return min(d, -b);
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
    float d = sdEllipsoidXXZ(p.xzy - vec3(-.0,0,0), vec2(.08,.05));
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

float fNose(vec3 p) {

    p = pRx(p - vec3(.0,.25,-.5), .4);

    vec3 p2 = p;
    p2.x = sqrt(p.x * p.x + .0001) - .0;
    p2 = pRz(p2, -.4);
    float d = sdEllipse(p2.xy - vec2(-.055, 0), vec2(.1,.18));

    d = smax(d, p.y-.09, 0.04);
    d = smax(d, -sdEllipse(p.xy - vec2(0,.18), vec2(.02,.1)), .03);
    d = max(d, -p.z-.15);
    d = smax(d, p.z-.05, .05);
    return d;
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

    p -= vec3(0,0,-.1);

    p = -p;

    vec3 ps = p;
    ps.x = sqrt(p.x * p.x + .0005);

    vec3 ps2 = p;
    ps2.x = sqrt(p.x * p.x + .008) - .012;

    vec3 ps15 = p;
    ps15.x = sqrt(p.x * p.x + .0035) - .015;

    vec3 pa = p;

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
    float socketbump = fSocketBump(ps2);
    d = smin(d, socketbump, .09);

    
    p = pRx(p - vec3(0,.23,-.45), -.55);

    float bridge = sdEllipse(p.xz, vec2(.06, .15));
    bridge = max(bridge, -p.y-.3);
    d = smin(d, bridge, .1);
    GIZMO(p); return d;
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

    p.x = sqrt(p.x * p.x + .001);
    float socketinset = fSocketInset(p);
    //socketinset = fSocketInset2(pa);
    //socketinset -= .02;
    //return socketinset;
    d = smax(d, -socketinset, .12);// return d;
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
    d = smin(d, max(length(p.xz) - .03, -p.y-.3), .05);
    p = pp;

    // canine socket
    p = pRy(pRz(p - vec3(.14,.55,-.495), -.4), .5);
    p.x = max(p.x, 0.);
    float caninesocket = length(p.xz) - .01;
    caninesocket = smax(caninesocket, -p.y-.2, .01);
    d = smin(d, caninesocket, .03);
    p = pp;

    // Nose
    float nos = fNose(pa);
    d = smin(d, nos-.01, .02);
    p = pp;

    float r = .3;
    p = pRy(pRx(ps15 - vec3(0,.49,-.57), -.45), .35);
    float nosecut = length(p.yz + vec2(0,r)) - r;
    d = smax(d, -nosecut, .01);
    p = pp;
    
    //return d;

    // Nose hole
    float nosb = fPillHalf((p - vec3(.0,.362,-.54)).xzy) - .005;
    nosb = max(nosb, p.z+.4);
    d = smin(d, nosb, .05);
    d = smax(d, -nos+.005, .02);

    p = pp;
    d = smax(d, fMaxillaBottom(p), .02);
    //float roof = sdEllipsoidXXZ(p - vec3(0,.47,-.28), vec2(.13, .22));
    //d = smax(d, -roof, .03);

    float temporal = sdEllipsoidXXZ(pRy(pRz(p - vec3(.25,.1,-.03), .2), -.4).yzx, vec2(.18, .08));
    temporal = smax(temporal, dot(p.zy, normalize(vec2(-.3,1))) - .2, .1);
    d = smin(d, temporal, .15);

    float archhole = fArchhole(p);
    //d = smax(d, -archhole, .03);

    //d = max(d, -d - .05);

    return d;
}

//#pragma glslify: export(sdSkull)


float map(vec3 p) {
    
    return sdSkull(p);
    
    /*
    float d = 1e12;

    d = min(d, length(p) - .5);

    p -= vec3(0, 1, -2);

    GIZMO(p);

    d = min(d, length(p) - .25);

    return d;
    */
}

#pragma glslify: export(map)
