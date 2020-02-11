uniform sampler2D iChannel0; // camera-data.glsl filter: nearest wrap: clamp
uniform vec2 iChannel0Size;


mat3 orientMatrix(vec3 A, vec3 B) {
    mat3 Fi = mat3(
        A,
        (B - dot(A, B) * A) / length(B - dot(A, B) * A),
        cross(B, A)
    );
    mat3 G = mat3(
        dot(A, B),              -length(cross(A, B)),   0,
        length(cross(A, B)),    dot(A, B),              0,
        0,                      0,                      1
    );
    return Fi * G * inverse(Fi);
}


// Cone with correct distances to tip and base circle. Y is up, 0 is in the middle of the base.
float fCone(vec3 p, float radius, float height) {
    vec2 q = vec2(length(p.xz), p.y);
    vec2 tip = q - vec2(0, height);
    vec2 mantleDir = normalize(vec2(height, radius));
    float mantle = dot(tip, mantleDir);
    float d = max(mantle, -q.y);
    float projected = dot(tip, vec2(mantleDir.y, -mantleDir.x));
    
    // distance to tip
    if ((q.y > height) && (projected < 0.)) {
        d = max(d, length(tip));
    }
    
    // distance to base ring
    if ((q.x > radius) && (projected > length(vec2(height, radius)))) {
        d = max(d, length(q - vec2(radius, 0)));
    }
    return d;
}

float fCone(vec3 p, vec3 n, vec3 base, vec3 apex, float radius) {
    p -= base;
    p *= orientMatrix(n, vec3(0,1,0));
    float height = distance(base, apex);
    return fCone(p, radius, height);
}


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
float wayAngle;


#pragma glslify: import('./camera-precalc.glsl')


void calcWaypoints() {
    wayAxis = calcAxis();
    mat3 mAxis = calcAxisMatrix(wayAxis);
    wayAngle = calcSpokeAngle(mAxis);
    // vec3 wayOrigin = calcCenter(wayAxis, mAxis, wayAngle);
    wayOrigin = findCenter();

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
    float scale = pow(stepScale, t);
    float angle = abs(wayAngle) * t;
    vec4 rot = rotate_angle_axis(angle, wayAxis);
    p -= wayOrigin;
    p = rotate_vector(p, rot);
    p *= scale;    
    p += wayOrigin;
    return scale;
}


float vmax(vec3 v) {
    return max(max(v.x, v.y), v.z);
}

float fBox(vec3 p, vec3 b) {
    vec3 d = abs(p) - b;
    return length(max(d, vec3(0))) + vmax(min(d, vec3(0)));
}

float fLine(vec3 p, vec3 n) {
    float t = dot(p, n) / dot(n, n);
    return length((n * t) - p) ;
}


float fWaypointB(vec3 p) {
    float s = 2.;
    float d = fBox(p, vec3(.02, .05, .03) * s);
    // d = min(d, max(length(p.yz) - .001, -p.x));
    // d = min(d, fLine(p, stepNormal) - .001);
    // d = min(d, abs(p.z) - .001);
    p -= vec3(.01,.025,.04) * s;
    d = min(d, fBox(p, vec3(.01,.025,.01) * s));
    return d;
}

float fWaypoint(vec3 p, Waypoint w) {
    float s = 2.;
    p -= w.trans;
    p = rotate_vector(p, q_conj(w.rot));
    float d = fWaypointB(p / w.scale) * w.scale;
    return d;
}



float mapWaypoints(vec3 p) {
    float path = 1e12;
    vec3 pp = p;
    float scale;
    const float WITER = 3. * 3.;
    for (float i = 0.; i < WITER; i++){
        p = pp;
        scale = tweenCamera(p, i / (WITER - 1.));
        p *= stepScale;
        scale *= stepScale;
        //path = min(path, length(p - tweenCameraPos(i / WITER)) - .005);
        path = min(path, fWaypointB(p) / scale);
    }
    p = pp;

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


    float d = min(path, blocks);
    // d = path;

    d = min(d, length(p - wayOrigin) - .05);

    float axis = fLine(p - wayOrigin, wayAxis) - .005;
    
    d = min(d, axis);

    // float cone = fCone(p, wayAxis, wayOrigin, wayOrigin+wayAxis*.5, length(wayOrigin));
    // cone = abs(cone) - .001;
    // cone = max(cone, -dot(p, wayAxis)+.02);
    // d = min(d, cone);

    // d = min(d, abs(dot(p, wayAxis)) - .0001);
    // d = min(d, length(p) - .2);
    // d = min(d, length(p - way1.trans) - .1);

    return d;
}
