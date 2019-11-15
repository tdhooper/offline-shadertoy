precision mediump float;

uniform vec2 iResolution;
uniform vec2 iOffset;
uniform float iTime;

uniform mat4 projection;
varying vec3 eye;
varying vec3 dir;
varying vec3 cameraForward;
varying mat4 vView;
varying float fov;
varying float aspect;
varying vec2 vVertex;



#define PI 3.14159265359
#define s(a, b, x) smoothstep(a, b, x)
#define rot(a) mat2(cos(a + PI*0.5*vec4(0,1,3,0)))
#define Z min(0, iFrame)




// iq distance functions
float sdBox( in vec3 p, in vec3 b ) {
    vec3 d = abs(p) - b;
    return min(max(d.x,max(d.y,d.z)),0.0) + length(max(d,0.0));
}
float sdCapsule( in vec3 p, in vec3 a, in vec3 b, in float r ) {
    vec3 pa = p - a, ba = b - a;
    float h = clamp( dot(pa,ba)/dot(ba,ba), 0.0, 1.0 );
    return length( pa - ba*h ) - r;
}
float sdEllipsoid( in vec3 p, in vec3 r ) {
    return (length( p/r ) - 1.0) * min(min(r.x,r.y),r.z);
}
float smin( in float a, in float b, in float s ) {
    float h = clamp( 0.5 + 0.5*(b-a)/s, 0.0, 1.0 );
    return mix(b, a, h) - h*(1.0-h)*s;
}




// skeleton, represented as pitch/yaw/roll rotations
vec3 skel[16];


// rotate a limb
vec3 rotateLimb( in vec3 p, in vec3 sk ) {
    vec3 pitchYawRoll = sk;
    p.xz *= rot(pitchYawRoll.y);
    p.yz *= rot(pitchYawRoll.x);
    p.xz *= rot(pitchYawRoll.z);
    return p;
}

// rotate an arm
vec3 rotateArm(vec3 p, vec3 sk ) {
    vec3 pitchYawRoll = sk;
    p.xz *= rot(pitchYawRoll.y);
    p.xy *= rot(pitchYawRoll.x);
    p.yz *= rot(pitchYawRoll.z);
    return p;
}

vec3 select(int i) {
    if (i == 0) return skel[0];
    if (i == 1) return skel[0];
    if (i == 2) return skel[0];
    if (i == 3) return skel[0];
    if (i == 4) return skel[0];
    if (i == 5) return skel[0];
    if (i == 6) return skel[0];
    if (i == 7) return skel[0];
    if (i == 8) return skel[0];
    if (i == 9) return skel[0];
    if (i == 10) return skel[0];
    if (i == 11) return skel[0];
    if (i == 12) return skel[0];
    if (i == 13) return skel[0];
    if (i == 14) return skel[0];
    if (i == 15) return skel[0];
}

// rotate a limb
vec3 rotateLimb( in vec3 p, in int i ) {
    return rotateLimb(p, select(i));
}

// rotate an arm
vec3 rotateArm(vec3 p, int i ) {
    return rotateArm(p, select(i));
}

float getTorso( in vec3 p ) {
    
    #ifdef SIMPLE_HUMAN
    return sdBox(p - vec3(0, 0.25, 0.0), vec3(0.3, 0.25, 0.1))-0.15;
    #endif
    
    vec3 mainDim = vec3(0.35, 0.15, 0.05);
    mainDim.x -= cos(p.y*2.0+0.8)*0.19;
    mainDim.y -= cos(p.x*7.0)*0.05;
    vec3 inTorso = p - vec3(0, 0.15, 0.05);
    inTorso.z += s(-0.2, 0.5, inTorso.y)*0.2;
    float torso = sdBox(inTorso, mainDim) - 0.15;
    
    vec3 trapDim = vec3(0.15, 0.13, 0);
    vec3 inTrap = inTorso - vec3(0.2, 0.33, -0.07);
    inTrap.xy *= rot(0.4);
    inTrap.yz *= rot(-0.2);
    float trap = sdBox(inTrap, trapDim)-0.13;
    
    vec3 pecDim = vec3(0.11, 0.08, 0.0);
    pecDim.y += sin(inTorso.x*7.5)*0.05;
    vec3 inPec = inTorso - vec3(0.19, 0.2, 0.12);
    float pec = sdBox(inPec, pecDim) - 0.1;
    float pecMore = length(inPec)-0.15;
    pec = smin(pec, pecMore, 0.25);
    
    float spine = s(0.13, 0.0, p.x)*s(0.1, -0.3, p.z);
    
    float d = torso;
    d = smin(d, trap, 0.1);
    d = smin(d, pec, 0.05);
    d += spine*0.02;
    return d;
}

float getPelvis( in vec3 p ) {
    
    #ifdef SIMPLE_HUMAN
    return sdBox(p - vec3(0, -0.45, 0.08), vec3(0.13, 0.3, 0))-0.2;
    #endif
    
    vec3 mainDim = vec3(0.17, 0.3, 0);
    mainDim.x += sin(p.y*6.0)*0.04;
    vec3 inMain = p - vec3(0, -0.45, 0.07);
    inMain.z -= cos(inMain.y*6.0)*0.02;
    
    float main = sdBox(inMain, mainDim) - 0.2;
    
    vec3 absDim = vec3(0.13, 0.29, 0.0);
    absDim.z -= cos(p.x*30.0)*0.007;
    absDim.z -= cos(p.y*36.0)*0.007;
    vec3 inAbs = inMain - vec3(0, 0.1, 0.13);
    float absD = sdBox(inAbs, absDim)-0.1;
    
    vec3 penisDim = vec3(0.03, 0.05, 0.05);
    penisDim.x -= sin(p.y*10.0)*0.03;
    vec3 inPenis = p - vec3(0, -0.9, 0.13);
    inPenis.z += inPenis.y*0.2;
    float penis = sdBox(inPenis, penisDim)-0.12;
    
    float butt = sdEllipsoid(p - vec3(0.17, -0.75, -0.03),
                             vec3(0.2, 0.28, 0.2));
    
    float spine = s(0.1, 0.0, p.x)*s(0.1, -0.1, p.z);
    
    float d = main;
    d = smin(d, absD, 0.1);
    d = smin(d, penis, 0.1);
    d = smin(d, butt, 0.1);
    d += spine*0.02;
    return d;
}

float getNeck( in vec3 p ) {
    return sdCapsule(p, vec3(0), vec3(0, 0.24, 0.07), 0.15);
}

float getHead( in vec3 p ) {
    
    #ifdef SIMPLE_HUMAN
    return sdBox(p - vec3(0, 0.18, 0.05), vec3(0.1, 0.19, 0.14))-0.1;
    #endif
    
    vec3 brainDim = vec3(0.2, 0.23, 0.22);
    vec3 inBrain = p - vec3(0, 0.27, 0.0);
    float brain = sdEllipsoid(inBrain, brainDim);
    
    vec3 faceDim = vec3(0.04, 0.19, 0.03);
    faceDim.x += sin(p.y*5.0)*0.05;
    faceDim.z += cos(p.x*15.0)*0.02;
    faceDim.z += sin(p.y*6.0)*0.03;
    vec3 inFace = p - vec3(0, 0.18, 0.11);
    float face = sdBox(inFace, faceDim) - 0.1;
    
    float d = brain;
    d = smin(d, face, 0.1);
    return d;
}

float getUpperArm( in vec3 p ) {
    
    #ifdef SIMPLE_HUMAN
    return sdCapsule(p, vec3(0), vec3(0.89, 0, 0), 0.1);
    #endif
    
    vec3 shoulderDim = vec3(0.23, 0.15, 0.18);
    float shoulder = sdEllipsoid(p - vec3(0.05, 0.05, 0.0), shoulderDim);
    
    float muscle1Rad = 0.09;
    muscle1Rad -= cos(p.x*8.0)*0.02;
    vec3 muscle1Pos1 = vec3(0.0, 0.05, 0.03);
    vec3 muscle1Pos2 = vec3(0.74, 0.05, 0.03);
    float muscle1 = sdCapsule(p, muscle1Pos1, muscle1Pos2, muscle1Rad);
    
    float muscle2Rad = 0.09;
    muscle2Rad += sin(p.x*7.0)*0.02;
    vec3 muscle2Pos1 = vec3(0.0, -0.04, -0.03);
    vec3 muscle2Pos2 = vec3(0.78, -0.02, -0.03);
    float muscle2 = sdCapsule(p, muscle2Pos1, muscle2Pos2, muscle2Rad);
    
    float d = shoulder;
    d = smin(d, muscle1, 0.03);
    d = smin(d, muscle2, 0.03);
    return d;
}

float getForearm( in vec3 p ) {
    const vec3 handPos = vec3(0.58, 0, 0);
    
    #ifdef SIMPLE_HUMAN
    return sdCapsule(p, vec3(0), handPos, 0.1);
    #endif
    
    float rad = 0.06 + sin(p.x*9.0)*0.01;
    float muscle1 = sdCapsule(p, vec3(0.06, 0.06, 0.0),
                              handPos+vec3(0, 0.05, 0), rad);
    float muscle2 = sdCapsule(p, vec3(0.04, -0.02, 0.03),
                              handPos+vec3(0, -0.01, 0), rad);
    
    float elbow = length(p)-0.08;
    
    float d = muscle1;
    d = smin(d, muscle2, 0.03);
    d = smin(d, elbow, 0.05);
    return d;
}

float getHand( in vec3 p ) {
    
    #ifdef SIMPLE_HUMAN
    return sdBox(p - vec3(0.25, 0.02, 0.0), vec3(0.14, 0.07, 0.0)) - 0.05;
    #endif
    
    vec3 handDim = vec3(0.08, 0.07, 0.01);
    float cu1 = cos(p.y*11.0-0.3);
    handDim.x += cu1*0.06;
    handDim.z += cu1*0.03;
    handDim.z -= sin(p.x*4.0)*0.05;
    
    float hand = sdBox(p - vec3(0.25, 0.02, 0.0), handDim) - 0.05;
    float thumb = sdCapsule(p, vec3(0.1, 0.02, 0.03),
                            vec3(0.15, 0.18, 0.06), 0.04);
    
    float d = hand;
    d = smin(d, thumb, 0.07);
    return d;
}

float getUpperLeg( in vec3 p ) {
    const vec3 kneePos = vec3(0, -1.01, 0);
    
    #ifdef SIMPLE_HUMAN
    return sdCapsule(p, vec3(0), kneePos, 0.15);
    #endif
    
    float muscle1Rad = 0.15 - sin(p.y*4.0)*0.03;
    float muscle1 = sdCapsule(p, vec3(0.03, 0.0, 0.1), kneePos, muscle1Rad);
    float muscle2 = sdCapsule(p, vec3(-0.12, 0.0, -0.05), kneePos, muscle1Rad);
    
    float knee = sdEllipsoid(p - vec3(0, -0.95, 0.03), vec3(0.12, 0.2, 0.12));
    
    float d = muscle1;
    d = smin(d, muscle2, 0.02);
    d = smin(d, knee, 0.03);
    
    return d;
}

float getLowerLeg( in vec3 p ) {
    const vec3 footPos = vec3(0, -1.06, -0.08);
    
    #ifdef SIMPLE_HUMAN
    return sdCapsule(p, vec3(0), footPos, 0.15);
    #endif
    
    float muscle1Rad = 0.1 - sin(p.y*4.0)*0.03;
    float muscle1 = sdCapsule(p, vec3(0.02, 0.0, 0.0), footPos, muscle1Rad);
    
    float muscle2Rad = 0.09 - sin(p.y*5.3)*0.05;
    float muscle2 = sdCapsule(p, vec3(-0.02, 0.04, -0.08),
                              footPos + vec3(0.0, 0.04, -0.02), muscle2Rad);
    
    float d = muscle1;
    d = smin(d, muscle2, 0.02);
    return d;    
}

float getFoot( in vec3 p ) {
    
    #ifdef SIMPLE_HUMAN
    return sdBox(p - vec3(0.0, -0.13, 0.15), vec3(0.08, 0.0, 0.25))-0.05;
    #endif
    
    vec3 footDim = vec3(0.04, 0.0, 0.19);
    footDim.x -= cos(p.z*13.0-0.4)*0.04;
    footDim.z += cos(p.x*14.0+0.2)*0.05;
    vec3 inFoot = p - vec3(0.03, -0.13, 0.19);
    float foot = sdBox(inFoot, footDim)-0.05;
    
    float ankle = sdEllipsoid(inFoot - vec3(0.0, 0.07, -0.13),
                              vec3(0.1, 0.08, 0.18));
    
    float d = foot;
    d = smin(d, ankle, 0.1);
    return d;
}


// main distance function
float de( in vec3 p ) {
    
    p.y += 0.5;
    
    // main pivot point is upper body
    vec3 inUpperBody = p;
    inUpperBody = rotateLimb(inUpperBody, 0);
    
    inUpperBody -= vec3(0, 1.3, 0);
    vec3 inLowerBody = inUpperBody;
    inLowerBody = rotateLimb(inLowerBody, 1);
    
    // keep upper body unflipped for the head
    vec3 inUpperBodyNoFlip = inUpperBody;
    // do some flipping
    int upperOffset = int(inUpperBody.x > 0.0)*3;
    int lowerOffset = int(inLowerBody.x > 0.0)*3;
    inUpperBody = vec3(abs(inUpperBody.x), inUpperBody.yz);
    inLowerBody = vec3(abs(inLowerBody.x), inLowerBody.yz);
    
    // do the torso
    float torso = getTorso(inUpperBody);
    
    // do the pelvis
    float pelvis = getPelvis(inLowerBody);
    
    // do the neck and head
    vec3 inNeck = inUpperBodyNoFlip - vec3(0, 0.68, -0.1);
    inNeck = rotateLimb(inNeck, 2);
    float neck = getNeck(inNeck);
    vec3 inHead = inNeck - vec3(0, 0.24, 0.07);
    inHead = rotateLimb(inHead, 3);
    float head = getHead(inHead);
    
    // do the arms
    vec3 inShoulder = inUpperBody - vec3(0.4, 0.48, -0.12);
    inShoulder = rotateArm(inShoulder, 4+upperOffset);
    float shoulder = getUpperArm(inShoulder);
    vec3 inElbow = inShoulder - vec3(0.79, 0, 0);
    inElbow = rotateArm(inElbow, 5+upperOffset);
    float elbow = getForearm(inElbow);
    vec3 inHand = inElbow - vec3(0.56, 0, 0);
    inHand = rotateArm(inHand, 6+upperOffset);
    float hand = getHand(inHand);
    
    // do the legs
    vec3 inHip = inLowerBody - vec3(0.25, -0.79, 0);
    inHip = rotateLimb(inHip, 10+lowerOffset);
    float hip = getUpperLeg(inHip);
    vec3 inKnee = inHip - vec3(0, -1.01, 0);
    inKnee = rotateLimb(inKnee, 11+lowerOffset);
    float knee = getLowerLeg(inKnee);
    vec3 inFoot = inKnee - vec3(0, -1.06, -0.08);
    inFoot = rotateLimb(inFoot, 12+lowerOffset);
    float foot = getFoot(inFoot);
    
    // blend the body together
    float d = torso;
    d = smin(d, pelvis, 0.2);
    d = smin(d, neck, 0.15);
    d = smin(d, head, 0.04);
    // blend the arms together
    float arms = shoulder;
    arms = smin(arms, elbow, 0.05);
    arms = smin(arms, hand, 0.05);
    // blend the legs together
    float legs = hip;
    legs = smin(legs, knee, 0.05);
    legs = smin(legs, foot, 0.1);
    // blend everything and return the value
    d = smin(d, arms, 0.1);
    d = smin(d, legs, 0.05);
    return d;
}












float map(vec3 p) {
    return de(p);
    return length(p) - .25;
}


vec3 calcNormal(vec3 p) {
    vec2 e = vec2(1, -1) * .0001;
    return normalize(
        e.xyy * map(p + e.xyy) +
        e.yyx * map(p + e.yyx) +
        e.yxy * map(p + e.yxy) +
        e.xxx * map(p + e.xxx)
    );
}


void main() {

    // skel[0] = vec3(0);
    // skel[1] = vec3(0);
    // skel[2] = vec3(0);
    // skel[3] = vec3(0);
    // skel[4] = vec3(0);
    // skel[5] = vec3(0);
    // skel[6] = vec3(0);
    // skel[7] = vec3(0);
    // skel[8] = vec3(0);
    // skel[9] = vec3(0);
    // skel[10] = vec3(0);
    // skel[11] = vec3(0);
    // skel[12] = vec3(0);
    // skel[13] = vec3(0);
    // skel[14] = vec3(0);
    // skel[15] = vec3(0);


    bool bg = false;
    vec3 camPos = eye;
    vec3 pos = camPos;
    vec3 rd = normalize(dir);
    float dist = 0.;
    float len = 0.;

    for (int i = 0; i < 200; i++) {
        len += dist;
        pos = camPos + len * rd;
        dist = map(pos);
        
        if (dist < .000001) {
            break;
        }
        
        if (len > 10.) {
            bg = true;
            break;
        }
    }

    vec3 col = vec3(.5);
    
    if ( ! bg) {
        vec3 nor = calcNormal(pos) * .5 + .5;
        col = vec3(.3) * clamp(dot(nor, vec3(.1,.2,.3) * 2.), 0., 1.);
    }

    gl_FragColor = vec4(col, 1);
}
