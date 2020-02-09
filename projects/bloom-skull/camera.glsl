struct Waypoint {
    vec3 trans;
    vec4 rot;
    float scale;
};

Waypoint way0;
Waypoint way1;
Waypoint way2;
Waypoint way3;

vec3 wayOrigin;
vec3 wayAxis;


vec3 calcCylinderNormal(vec3 a, vec3 b, vec3 c) {
    vec3 tan = a - c;
    vec3 bin = cross(a - b, c - b);
    vec3 nor = normalize(cross(tan, bin));
    return nor;
}

vec3 projectOnPlane(vec3 v, vec3 n) {
    float scalar = dot(n, v) / length(n);
    vec3 v1 = n * scalar;
    return v - v1;
}

vec2 lineIntersection(vec2 point0, vec2 dir0, vec2 point1, vec2 dir1) {
    vec2 p1 = point0;
    vec2 p2 = point0 + dir0;
    vec2 p3 = point1;
    vec2 p4 = point1 + dir1;

    float a = (p2.y - p1.y) / (p2.x - p1.x);
    float b = (p2.x * p1.y - p1.x * p2.y) / (p2.x - p1.x);
    float c = (p4.y - p3.y) / (p4.x - p3.x);
    float d = (p4.x * p3.y - p3.x * p4.y) / (p4.x - p3.x);

    float x = (d - b) / (a - c);
    float y = (a * d - b * c) / (a - c);

    return vec2(x, y);
}

vec3 debug0;
vec3 debug1;

void calcCylinder() {
    vec3 up = vec3(0,-1,0);

    vec3 v0, v1, v2, v3;
    vec4 r0, r1, r2, r3;

    v0 = vec3(0);
    r0 = QUATERNION_IDENTITY;

    v1 = stepPosition;
    r1 = q_look_at(stepNormal, up);

    v2 = v1 + rotate_vector(stepPosition, r1);
    r2 = q_look_at(rotate_vector(stepNormal, r1), rotate_vector(up, r1));

    v3 = v2 + rotate_vector(stepPosition, r2);
    r3 = q_look_at(rotate_vector(stepNormal, r2), rotate_vector(up, r2));

    vec3 n0 = calcCylinderNormal(v0, v1, v2);
    vec3 n1 = calcCylinderNormal(v1, v2, v3);

    vec3 nor = normalize(cross(n0, n1));
    vec3 bin = normalize(cross(nor, up));
    vec3 tan = normalize(cross(nor, bin));
    mat3 m = mat3(nor, bin, tan);
    mat3 mi = inverse(m);

    // Project onto axis plane 
    n0 = n0 * m;
    n1 = n1 * m;
    v1 = v1 * m;
    v2 = v2 * m;
    n0.x = 0.;
    n1.x = 0.;
    v1.x = 0.;
    v2.x = 0.;

    vec2 center = lineIntersection(
        v1.yz, n0.yz,
        v2.yz, n1.yz
    );

    debug0 = v1 * mi;
    debug1 = v2 * mi;

    wayAxis = nor;
    wayOrigin = vec3(0, center) * mi;
}

void calcWaypoints() {
    calcCylinder();

    vec3 up = vec3(0,-1,0);
    vec3 normal = stepNormal;

    way0.scale = 1. / stepScale;
    way0.trans = vec3(0);
    way0.rot = QUATERNION_IDENTITY;

    way1.scale = way0.scale * stepScale;
    way1.trans = stepPosition * way1.scale;
    way1.rot = q_look_at(normal, up);

    way2.scale = way1.scale * stepScale;
    way2.trans = way1.trans + rotate_vector(stepPosition * way2.scale, way1.rot);
    way2.rot = q_look_at(rotate_vector(normal, way1.rot), rotate_vector(up, way1.rot));

    way3.scale = way2.scale * stepScale;
    way3.trans = way2.trans + rotate_vector(stepPosition * way3.scale, way2.rot);
    way3.rot = q_look_at(rotate_vector(normal, way2.rot), rotate_vector(up, way2.rot));
}


vec3 Catmull(vec3 p0, vec3 p1, vec3 p2, vec3 p3, float t){
    return (((-p0 + p1*3. - p2*3. + p3)*t*t*t + (p0*2. - p1*5. + p2*4. - p3)*t*t + (-p0 + p2)*t + p1*2.)*.5);
}

vec3 bezier(vec3 p0, vec3 p1, vec3 p2, vec3 p3, float t) {
    vec3 a0 = mix(p0, p1, t);
    vec3 a1 = mix(p1, p2, t);
    vec3 a2 = mix(p2, p3, t);
    vec3 b0 = mix(a0, a1, t);
    vec3 b1 = mix(a1, a2, t);
    return mix(a0, b1, t);
}

vec3 midpointNormal(vec3 a, vec3 b, vec3 c) {
    vec3 an = normalize(a - b);
    vec3 cn = normalize(c - b);
    vec3 mid = mix(an, cn, .5);
    return normalize(-mid);
}

vec3 planeNormal(vec3 a, vec3 b, vec3 c) {
    return normalize(cross(b - a, c - a));
}

vec3 controlDir(vec3 a, vec3 b, vec3 c) {
    return cross(
        midpointNormal(a, b, c),
        planeNormal(a, b, c)
    );
}

vec3 tweenCameraPos(float t) {
    vec3 c1 = way1.trans - controlDir(way0.trans, way1.trans, way2.trans) * distance(way1.trans, way2.trans) * .6;
    vec3 c2 = way2.trans + controlDir(way1.trans, way2.trans, way3.trans) * distance(way1.trans, way2.trans) * .2;
    t = pow(t, 1.08); // correct for non-constant speed
    return bezier(way1.trans, c1, c2, way2.trans, t);
}

float tweenCamera(inout vec3 p, float t) {
    vec4 rot = q_slerp(way1.rot, way2.rot, t);
    float ramp = (pow(stepScale, t) - 1.) / (stepScale - 1.);
    float scale = mix(way1.scale, way2.scale, ramp);
    vec3 trans = tweenCameraPos(ramp);
    p *= scale;
    p = rotate_vector(p, rot);
    p += trans;
    return scale;
}


float vmax(vec3 v) {
    return max(max(v.x, v.y), v.z);
}

float fBox(vec3 p, vec3 b) {
    vec3 d = abs(p) - b;
    return length(max(d, vec3(0))) + vmax(min(d, vec3(0)));
}

float fWaypoint(vec3 p, Waypoint w) {
    float s = 2.;
    p -= w.trans;
    p = rotate_vector(p, q_conj(w.rot));
    float d = fBox(p, w.scale * vec3(.02, .05, .03) * s);
    p -= w.scale * vec3(.01,.025,.04) * s;
    d = min(d, fBox(p, w.scale * vec3(.01,.025,.01) * s));
    return d;
}

#define saturate(x) clamp(x, 0., 1.)

float fLine(vec3 p, vec3 n) {
    float t = dot(p, n) / dot(n, n);
    return length((n * t) - p) ;
}


// Distance to line segment between <a> and <b>, used for fCapsule() version 2below
float fLineSegment(vec3 p, vec3 a, vec3 b) {
    vec3 ab = b - a;
    float t = saturate(dot(p - a, ab) / dot(ab, ab));
    return length((ab*t + a) - p);
}

// Capsule version 2: between two end points <a> and <b> with radius r 
float fCapsule(vec3 p, vec3 a, vec3 b, float r) {
    return fLine(p, normalize(a - b)) - r;
    return fLineSegment(p, a, b) - r;
}

float mapWaypoints(vec3 p) {
    float path = 1e12;
    const float WITER = 20.;
    for (float i = 0.; i < WITER; i++){
        path = min(path, length(p - tweenCameraPos(i / WITER)) - .005);
    }

    float blocks = min(
        min(
            fWaypoint(p, way0),
            fWaypoint(p, way1)
        ),
        min(
            fWaypoint(p, way2),
            fWaypoint(p, way3)
        )
    );

    float axis = fLine(p, wayAxis) - .02;

    float d = min(path, blocks);
    //d = min(d, axis);

    //d = min(d, length(p - debug0) - .03);
    //d = min(d, length(p - debug1) - .03);
    float pl = abs(dot(p, wayAxis)) - .01;
    pl = max(pl, length(p - wayOrigin) - .5);
    d = min(d, pl);

    // d = min(d, fLine(p - debug0, wayAxis) - .005);
    // d = min(d, fLine(p - debug1, wayAxis) - .005);

    d = min(d, fLine(p - wayOrigin, wayAxis) - .005);

    return d;
}
